
#library(sqldf)
##library(RSQLite)
#library(dplyr)
#library(warbase)
#source("woi-makedata.R")


connect.sqlite <- function (dbname="common-data/waronice.sqlite") {
    con <- dbConnect(SQLite(), dbname=dbname)
    return(con)
}
send.db <- function(connection, statement){
    res <- dbSendQuery(conn = connection, statement)
    dbClearResult(res)
}
close.db <- function(connection) {dbDisconnect(connection)}
##update.db <- function(statement) {c <- connect.db(); send.db(c, statement); close.db(c)}


dplyr.sqlite <- function (dbname="common-data/waronice.sqlite") {
    output <- src_sqlite (dbname = dbname)
    output
}



build.running.stats <- function (df, curvals=NULL) {
    ## df=filter (all.coplay, p1=="abbotsp88")
    message ("Running stats: ", df$p1[1])

    if (!is.null(curvals)) {
        row <- match (df$p1[1], curvals$p1)
        if (!is.na(row)) value.prime <- c(CF60=curvals$rCF60[row],
                                          CA60=curvals$rCA60[row],
                                          TOI60=curvals$rTOI60[row]) else value.prime <- c(CF60=50, CA60=50, TOI60=15)
    } else value.prime <- c(CF60=50, CA60=50, TOI60=15)

    final.fill <- data.frame(season=df$season,
                             gcode=df$gcode,
                             rCF60=rep(NA, nrow(df)),
                             rCA60=rep(NA, nrow(df)),
                             rTOI60=rep(NA, nrow(df)))
    
    for (rr in 1:nrow(df)) {
        value.prime.new <- c(0.96, 0.96, 0.86)*value.prime +
            c(0.04, 0.04, 0.14)*c((df$evf2[rr] + 1/60)/(df$el2[rr]+1)*3600,
                                  (df$eva2[rr] + 1/60)/(df$el2[rr]+1)*3600,
                                  (df$el2[rr])/(df$el2[rr] + df$el2off[rr])*60)
        value.prime.new[is.na(value.prime.new)] <- value.prime[is.na(value.prime.new)]
        value.prime <- value.prime.new
        final.fill[rr,-(1:2)] <- value.prime
    }

    final.fill <- final.fill %>% mutate (lCF60=lag(rCF60), lCA60=lag(rCA60), lTOI60=lag(rTOI60))
    final.fill[1, 6:8] <- final.fill[1, 2+1:3]

    return(final.fill)
    
}


make.collective.coplay.files <- function (games=gamestest,
                                          db=connect.sqlite("common-data/player-summary.sqlite")) {

    ##load ("source-data/nhlscrapr-core.RData")
    
    ##reduced.connect <- src_sqlite(sourcefile)
    ##all.coplay <- tbl(reduced.connect, sql("SELECT * FROM coplayer WHERE season >= 20052006")) %>% filter (p1 == p2) %>% collect

    coplayer <- NULL
    all.coplay <- rbind_all(lapply(paste0(games$season, games$gcode)[games$status==3], function(gg) {
        print(gg)
        try(load (paste0("common-data/games/",gg,".RData")));
        coplayer <- mutate(coplayer, season = anac(season))
        filter (coplayer, p1 == p2)
    }))
    
    running.stats <- all.coplay %>% group_by(p1) %>% do(build.running.stats(.))

    ##all.coplay <- arrange(all.coplay, p1, season, gcode)
    final.coplay <- left_join(all.coplay, running.stats, by=c("p1","season","gcode")) %>% arrange(season, gcode)
    
    dbWriteTable(conn = db, name = "reducedplayer", value = as.data.frame(final.coplay), overwrite=TRUE)
    res <- dbSendQuery (db, "CREATE INDEX season_gcode_1 ON reducedplayer (p1, season, gcode)"); dbClearResult(res)
    res <- dbSendQuery (db, "CREATE INDEX season_gcode_2 ON reducedplayer (season, gcode, p1)"); dbClearResult(res)

    dbDisconnect(db)            # Close connection
    
}

make.collective.coplay.server <- function (games=gamestest,
                                           db.read=dplyr.sqldb(),
                                           db.write=connect.sqlite("common-data/player-summary.sqlite")) {
    
    all.coplay <- tbl(db.read, sql("SELECT * FROM coplayer WHERE season >= 20052006")) %>% filter (p1 == p2) %>% collect
    running.stats <- all.coplay %>% group_by(p1) %>% do(build.running.stats(.))
    
    final.coplay <- left_join(all.coplay, running.stats, by=c("p1","season","gcode")) %>% arrange(season, gcode)
    
    dbWriteTable(conn = db.write, name = "reducedplayer",
                 value = running.stats %>% arrange (season, gcode) %>% as.data.frame, overwrite=TRUE)
    
    
    res <- dbSendQuery (db.write, "CREATE INDEX season_gcode_rs ON reducedplayer (p1, season, gcode)");
    dbClearResult(res)
    res <- dbSendQuery (db.write, "CREATE INDEX season_gcode_rs2 ON reducedplayer (season, gcode, p1)");
    dbClearResult(res)

    dbDisconnect(db.write)            # Close connection
    
}

                                       
augment.collective.coplay <- function (games=gamestest,
                                       reduced.connect = src_sqlite("common-data/player-summary.sqlite"),
                                       db=connect.sqlite("common-data/player-summary.sqlite")
                                           ##targetfile="common-data/player-summary.sqlite"
                                       ) {

    statement <- paste0('SELECT DISTINCT season, gcode FROM reducedplayer')
    preprimer <- dbSendQuery(db, statement)
    primer <- fetch(preprimer, n = -1)
    
    possibles <- subset(games, status == 3)
    new.to.include <- setdiff (paste (possibles$season, possibles$gcode),
                               paste (primer$season, primer$gcode))
    if (length(new.to.include) > 0) {
        
        all.coplay <- rbind_all(lapply(new.to.include, function (gg) {
            d1 <- try({load (paste0("common-data/games/", substr(gg,1,8), substr(gg,10,14), ".RData")); as.data.frame(filter(coplayer, p1 == p2))}, TRUE)
            if (class(d1) == "try-error") data.frame() else d1
        })) %>% mutate (date=games$date[match(paste(season, gcode), paste(games$season, games$gcode))]) %>% arrange (date) 
        
        ## get last values. Add these to the new running.stats.
        last.series <- tbl(reduced.connect, sql("SELECT p1, rCF60, rCA60, rTOI60 FROM reducedplayer")) %>% filter (p1 %in% all.coplay$p1) %>% collect %>% as.data.frame %>% group_by (p1) %>% summarize (rCF60=last(rCF60), rCA60=last(rCA60), rTOI60=last(rTOI60))
        
        running.stats <- all.coplay %>% group_by(p1) %>% do(build.running.stats(., curvals=last.series))
        final.coplay <- left_join(all.coplay, running.stats, by=c("p1","season","gcode")) %>% select (-date)
                
        ##db <- dbConnect(SQLite(), dbname=targetfile)
        dbWriteTable(conn = db, name = "reducedplayer", value = as.data.frame(final.coplay), append=TRUE)
        dbDisconnect(db)            # Close connection
    }
    
    return(TRUE)
}


first.build <- function () {
    make.collective.coplay (subset(gamestest, season==20022003))
    for (ss in seasons[2:12]) {message(ss); augment.collective.coplay (subset(gamestest, season <= ss))}
}


make.indices <- function (con=connect.sqldb()
                          ) {

    res <- dbSendQuery (con, "CREATE INDEX season_gcode_1 ON teamrun (season, gcode, period)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX season_gcode_2 ON goalierun (season, gcode, period)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX season_gcode_3 ON playerrun (season, gcode, period)"); dbClearResult(res)

    res <- dbSendQuery (con, "CREATE INDEX season_gcode_4 ON playbyplay (season, gcode)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX season_gcode_5 ON gameroster (season, gcode)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX season_gcode_6 ON coplayer (season, gcode)"); dbClearResult(res)

    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_1 ON teamrun (period, gamestate, scorediffcat)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_2 ON goalierun (period, gamestate, scorediffcat)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_3 ON playerrun (period, gamestate, scorediffcat)"); dbClearResult(res)

    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_3_1 ON playerrun (season, gamestate, scorediffcat, Date(10))"); dbClearResult(res)

    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_4 ON playerrun (ID(9), gamestate, scorediffcat, Date(10))"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_5 ON teamrun (Team(3), period, gamestate, scorediffcat)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_6 ON goalierun (ID(9), period, gamestate, Date(10))"); dbClearResult(res)

    dbDisconnect(con)

}

make.indices.sqlite <- function (con=connect.sqlite("common-data/waronice.sqlite")
                          ) {

    res <- dbSendQuery (con, "CREATE INDEX season_gcode_1 ON teamrun (season, gcode)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX season_gcode_2 ON goalierun (season, gcode)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX season_gcode_3 ON playerrun (season, gcode)"); dbClearResult(res)
    
    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_1 ON teamrun (period, gamestate, scorediffcat)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_2 ON goalierun (period, gamestate, scorediffcat)"); dbClearResult(res)
    res <- dbSendQuery (con, "CREATE INDEX sdc_gm_pd_3 ON playerrun (period, gamestate, scorediffcat)"); dbClearResult(res)

    dbDisconnect(con)

}

scrub <- function() {
        if (augment) {
            dbSendQuery(conn = con, paste("DELETE from playerrun WHERE season = ",season, "AND gcode = ",gcode)); dbClearResult(res)
            dbSendQuery(conn = con, paste("DELETE from teamrun WHERE season = ",season, "AND gcode = ",gcode)); dbClearResult(res)
            dbSendQuery(conn = con, paste("DELETE from goalierun WHERE season = ",season, "AND gcode = ",gcode)); dbClearResult(res)
            dbSendQuery(conn = con, paste("DELETE from playbyplay WHERE season = ",season, "AND gcode = ",gcode)); dbClearResult(res)
            dbSendQuery(conn = con, paste("DELETE from gameroster WHERE season = ",season, "AND gcode = ",gcode)); dbClearResult(res)
            dbSendQuery(conn = con, paste("DELETE from coplayer WHERE season = ",season, "AND gcode = ",gcode)); dbClearResult(res)
        }
}

create.master.database <- function (thesegames=gamestest,
                                    in.reverse=FALSE,
                                    augment=FALSE,
                                    con=connect.sqlite("common-data/waronice.sqlite")
                                    ##grand.dbname="common-data/waronice.sqlite"
) {
    ## thesegames=filter(gamestest, season=="20142015", gcode>"21200"); in.reverse=FALSE; augment=FALSE; grand.dbname="common-data/waronice.sqlite"
    
    if (augment) {
        primer <- dbSendQuery(con, 'SELECT season, gcode, COUNT(GF) FROM teamrun WHERE gamestate = 1 AND period = 0 GROUP BY season, gcode')  
        rt <- fetch(primer, n = -1)
        to.include <- which(thesegames$status == 3 &
                            is.na(match(paste(thesegames$season, thesegames$gcode), paste(rt$season, rt$gcode))))
    } else {to.include <- which(thesegames$status == 3)}
    if (in.reverse) to.include <- rev(to.include)

    for (gg in to.include) try({
        message (thesegames$season[gg], " ", thesegames$gcode[gg], " ", thesegames$date[gg])
        load (paste0("common-data/games/", thesegames$season[gg], thesegames$gcode[gg], ".RData"))
       
        thisdate <- thesegames$date[gg]
        season <- anac(thesegames$season[gg])
        gcode <- anac(thesegames$gcode[gg])

        r1 <- dbWriteTable(conn = con, name = "playerrun",
                           value = playerrun %>% as.data.frame %>% filter (period == 0) %>% mutate (Date = thisdate, season=anac(season), gcode=anac(gcode)) %>%
                           rename(scorediffcat = score.diff.cat),
                           append=TRUE)
        r2 <- dbWriteTable(conn = con, name = "teamrun",
                           value = teamrun %>% as.data.frame %>% mutate (Date = thisdate, season=anac(season), gcode=anac(gcode)) %>%
                           rename(scorediffcat = score.diff.cat),
                           append=TRUE)
        r3 <- dbWriteTable(conn = con, name = "goalierun",
                           value = goalierun %>% as.data.frame %>% mutate (Date = thisdate, season=anac(season), gcode=anac(gcode)) %>%
                           rename(scorediffcat = score.diff.cat),
                           append=TRUE)
                           
        r4 <- dbWriteTable(conn = con, name = "playbyplay",
                           value = as.data.frame(playbyplay %>% mutate (Date = thisdate, season=anac(season), gcode=anac(gcode))), append=TRUE)
        r5 <- dbWriteTable(conn = con, name = "gameroster",
                           value = as.data.frame(mutate(gameroster,
                               season=anac(thesegames$season[gg]), gcode=anac(thesegames$gcode[gg]), Date = thisdate)),
                           append=TRUE)
        r6 <- dbWriteTable(conn = con, name = "coplayer",
                           value = as.data.frame(mutate(coplayer, 
                               season=anac(thesegames$season[gg]), gcode=anac(thesegames$gcode[gg]), Date = thisdate)), append=TRUE)
        message (thesegames$season[gg], " ", thesegames$gcode[gg], " ", thesegames$date[gg], "Done")
    })
    
    message (thesegames$season[gg], " ", thesegames$gcode[gg], " ", thesegames$date[gg], "Done")

    dbDisconnect(con)

##  if (!augment) make.indices(con)
    
}


replace.master.database <- function (seasongcode,
                                     con = connect.sqlite("common-data/waronice.sqlite")
) {
    
    ##con <- dbConnect(SQLite(), dbname=grand.dbname)
    
    for (gg in seasongcode) try({
        ## gg = 2015201620001
        thisseason <- as.numeric(substr(gg, 1, 8))
        thisgcode <- as.numeric(substr(gg, 9, 13))
        
        message (thisseason, " ", thisgcode)
        load (paste0("common-data/games/", thisseason, thisgcode, ".RData"))
        
        thisdate <- gamestest$date[match(paste0(thisseason, thisgcode),
                                         paste0(gamestest$season, gamestest$gcode))]

        dbSendQuery(conn = con, paste("DELETE from playerrun WHERE season = ",thisseason, "AND gcode =",thisgcode))
        r1 <- dbWriteTable(conn = con, name = "playerrun",
                           value = playerrun %>% as.data.frame %>% filter (period == 0) %>% mutate (Date = thisdate, season=anac(season), gcode=anac(gcode)) %>%
                           rename(scorediffcat = score.diff.cat),
                           append=TRUE)

        dbSendQuery(conn = con, paste("DELETE from teamrun WHERE season = ",thisseason, "AND gcode =",thisgcode))
        r2 <- dbWriteTable(conn = con, name = "teamrun",
                           value = teamrun %>% as.data.frame %>% mutate (Date = thisdate, season=anac(season), gcode=anac(gcode)) %>%
                           rename(scorediffcat = score.diff.cat),
                           append=TRUE)

        dbSendQuery(conn = con, paste("DELETE from goalierun WHERE season = ",thisseason, "AND gcode =",thisgcode))
        r3 <- dbWriteTable(conn = con, name = "goalierun",
                           value = goalierun %>% as.data.frame %>% mutate (Date = thisdate, season=anac(season), gcode=anac(gcode)) %>%
                           rename(scorediffcat = score.diff.cat),
                           append=TRUE)
                           
        dbSendQuery(conn = con, paste("DELETE from playbyplay WHERE season = ",thisseason, "AND gcode =",thisgcode))
        r4 <- dbWriteTable(conn = con, name = "playbyplay",
                           value = as.data.frame(playbyplay %>% mutate (Date = thisdate, season=anac(season), gcode=anac(gcode))), append=TRUE)
        dbSendQuery(conn = con, paste("DELETE from gameroster WHERE season = ",thisseason, "AND gcode =",thisgcode))
        r5 <- dbWriteTable(conn = con, name = "gameroster",
                           value = as.data.frame(mutate(gameroster,
                               season=thisseason, gcode=thisgcode, Date = thisdate)),
                           append=TRUE)
        dbSendQuery(conn = con, paste("DELETE from coplayer WHERE season = ",thisseason, "AND gcode =",thisgcode))
        r6 <- dbWriteTable(conn = con, name = "coplayer",
                           value = as.data.frame(mutate(coplayer, 
                               season=thisseason, gcode=thisgcode, Date = thisdate)), append=TRUE)
    })

    dbDisconnect(con)

    
}




## More bits here. Compress to seasons for players.


## compress.players.full (last(seasons), target.link = "common-data/woi-playerseason2.sqlite")
## compress.players.full (last(seasons), db=connect.sqlite("common-data/woi-playerseason.sqlite"))
## Compress to seasons, etc.
local.grab.playerrun <- function (ss, tt, games) {
    ## ss = 20152016; tt = "TOR"; games=gamestest
    game.set <- filter(games, season == ss, hometeam == tt | awayteam == tt, status == 3)
    gameload <- paste0(game.set$season, game.set$gcode)
    
    output <- if (length(gameload) == 0) NULL else rbind_all(lapply (gameload, function (gg) {
        load (paste0("common-data/games/",gg,".RData"))
        filter(playerrun, Team == tt) %>% rename (scorediffcat = score.diff.cat) %>%
            mutate (season = anac(season), gcode = anac (gcode))
    })) 

    output
}

compress.players.sql <- function (seas = seasons,
                                  tms = teams,
                                  games = gamestest,
                                  reduced.connect=dplyr.sqldb(),
                                  db=connect.sqldb()
                                   ##, target.link = "common-data/woi-playerseason.sqlite"
                                   ) {
    
    ## ss <- "20152016"; tt <- "MIN"; db=connect.sqldb()
    for (ss in seas) {
        ## delete previous values.
        
        ##db <- dbConnect(SQLite(), dbname=target.link)
        ##dbDisconnect(db)
        for (tt in tms) {

            message(ss, " ", tt)
            ##try(dbSendQuery(conn = db, paste("DELETE from playerseason WHERE season = ",ss)))
    
            all.events <- local.grab.playerrun (ss, tt, games)
            if (!is.null(all.events)) {
                
                results.1 <- all.events %>% mutate (gp = substr(gcode,1,1)) %>% group_by (ID, scorediffcat, gamestate, period, home, Team, gp) %>% select (-Opponent) %>% summarize_each (funs(sum), TOI:ASSIST_2) %>% collect %>% ungroup 
            
                results.2 <- all.events %>% mutate (gp = substr(gcode,1,1)) %>% group_by (ID, scorediffcat, gamestate, period, home, Team, gp) %>% select (-Opponent) %>%
                    summarize (Gm = n_distinct(gcode),
                               tCF60 = sum(tCF60*TOI)/sum(TOI),
                               tCA60 = sum(tCA60*TOI)/sum(TOI),
                               tTOI60 = sum(tTOI60*TOI)/sum(TOI),
                               
                               cCF60 = sum(cCF60*TOI)/sum(TOI),
                               cCA60 = sum(cCA60*TOI)/sum(TOI),
                               cTOI60 = sum(cTOI60*TOI)/sum(TOI)) %>%
                                   collect %>% ungroup
                results <- left_join (results.1, results.2) %>% mutate(gcode = paste0(gp,"0000"), season = ss) 
                message(ss, " ", tt, "computed")
            
            ## save.
                try(res <- dbSendQuery(conn = db, paste0("DELETE from playerseason WHERE season = ",ss," AND Team = '", tt,"'")))
                message(ss, " ", tt, "deleted. Loading data")
    
            ##db <- dbConnect(SQLite(), dbname=target.link)
                dbWriteTable(conn = db, name = "playerseason", value = as.data.frame(results), append=TRUE)
            ##dbDisconnect(db)
            }
        }
    }
    
    ##db <- dbConnect(SQLite(), dbname=target.link)
    try({
        res <- dbSendQuery (db, "CREATE INDEX season_gcode_4 ON playerseason (season, gamestate, scorediffcat)");
        dbClearResult(res)
    })
    
    dbDisconnect(db)
    
    ## db <- dbConnect(SQLite(), dbname=sqlite.link); res <- dbSendQuery (db, "DROP TABLE playerseason"); dbDisconnect(db)
    
    
}


compress.players.full <- function (seas = seasons,
                                   tms = teams,
                                   source.link = "common-data/waronice.sqlite",
                                   db=connect.sqlite("common-data/woi-playerseason.sqlite")
                                   ##, target.link = "common-data/woi-playerseason.sqlite"
                                   ) {
    
    ## ss <- "20052006"; tt <- "MIN"
    for (ss in seas) {

        ## delete previous values.
        
        ##db <- dbConnect(SQLite(), dbname=target.link)
        ##dbDisconnect(db)
        
        for (tt in tms) {

            message(ss, " ", tt)
            try(dbSendQuery(conn = db, paste0("DELETE from playerseason WHERE season = ",ss," AND Team = '", tt,"'")))
        
        ## create.
            reduced.connect <- src_sqlite(source.link)
            all.coplay <- tbl(reduced.connect, sql(paste0("SELECT * FROM playerrun")))

            ## results.0 <- all.coplay %>% filter (Team == tt, season == ss) %>% mutate (gp = substr(gcode,1,1))
            results.1 <- all.coplay %>% filter (Team == tt, season == ss) %>% mutate (gp = substr(gcode,1,1)) %>% group_by (ID, scorediffcat, gamestate, period, home, Team, gp) %>% select (-Opponent, -Date) %>% summarize_each (funs(sum), TOI:ASSIST_2) %>% collect %>% ungroup 
            
            results.2 <- all.coplay %>% filter (Team == tt, season == ss) %>% mutate (gp = substr(gcode,1,1)) %>% group_by (ID, scorediffcat, gamestate, period, home, Team, gp) %>% select (-Opponent, -Date) %>%
                summarize (Gm = n_distinct(gcode),
                           tCF60 = sum(tCF60*TOI)/sum(TOI),
                           tCA60 = sum(tCA60*TOI)/sum(TOI),
                           tTOI60 = sum(tTOI60*TOI)/sum(TOI),
                           
                           cCF60 = sum(cCF60*TOI)/sum(TOI),
                           cCA60 = sum(cCA60*TOI)/sum(TOI),
                           cTOI60 = sum(cTOI60*TOI)/sum(TOI)) %>%
                               collect %>% ungroup
            results <- left_join (results.1, results.2) %>% mutate(gcode = paste0(gp,"0000"), season = ss) 
            
        ## save.
        
            ##db <- dbConnect(SQLite(), dbname=target.link)
            dbWriteTable(conn = db, name = "playerseason", value = as.data.frame(results), append=TRUE)
            ##dbDisconnect(db)
        
        }
    }
    
    ##db <- dbConnect(SQLite(), dbname=target.link)
    try({
        res <- dbSendQuery (db, "CREATE INDEX season_gcode_4 ON playerseason (season, gamestate, scorediffcat)");
        dbClearResult(res)
    })
    
    dbDisconnect(db)
    
    ## db <- dbConnect(SQLite(), dbname=sqlite.link); res <- dbSendQuery (db, "DROP TABLE playerseason"); dbDisconnect(db)
    
    
}




make.hextally <- function (seas=seasons,
                           db.hex=connect.sqlite("common-data/hextally.sqlite")
                           ##dbfile="common-data/hextally.sqlite"
                           ) {

    ##db.hex <- connect.sqldb()
    ##db.hex <- dbConnect(SQLite(), dbname=dbfile)

    for (s.ind in 1:length(seas)) {
        ss <- seas[s.ind]
        message ("Hextally ",ss)
        hex.set <- produce.hexshots (get(load(paste0("source-data/nhlscrapr-",ss,".RData"))))
        
        dbWriteTable(conn = db.hex, name = "shots",
                     value = as.data.frame(hex.set$shots) %>%
                     mutate(ID = roster.unique$woi.id[ID], season=anac(season), gcode=anac(gcode)),
                     append=s.ind > 1, overwrite=s.ind == 1)
        dbWriteTable(conn = db.hex, name = "TOI",
                     value = as.data.frame(hex.set$TOI) %>% mutate(ID = roster.unique$woi.id[ID], season=anac(season), gcode=anac(gcode)),
                     append=s.ind > 1, overwrite=s.ind == 1)
        dbWriteTable(conn = db.hex, name = "shotsteam",
                     value = as.data.frame(hex.set$shots.team) %>% mutate(season=anac(season), gcode=anac(gcode)),
                     append=s.ind > 1, overwrite=s.ind == 1)
        dbWriteTable(conn = db.hex, name = "TOIteam",
                     value = as.data.frame(hex.set$TOI.team) %>% mutate(season=anac(season), gcode=anac(gcode)),
                     append=s.ind > 1, overwrite=s.ind == 1)
        dbWriteTable(conn = db.hex, name = "baselinedata",
                     value = as.data.frame(hex.set$baseline.data) %>% mutate(season=anac(season)),
                     append=s.ind > 1, overwrite=s.ind == 1)
        
        ##save (hex.set, file=paste0("common-data/hextally-",ss,".RData"))
    }
    
    res <- dbSendQuery (db.hex, "CREATE INDEX hex1 ON shots (ID(9), season, gamestate)"); dbClearResult(res)
    res <- dbSendQuery (db.hex, "CREATE INDEX hex2 ON TOI (ID(9), season, gamestate)"); dbClearResult(res)
    res <- dbSendQuery (db.hex, "CREATE INDEX hex3 ON shotsteam (Team(3), season, gamestate)"); dbClearResult(res)
    res <- dbSendQuery (db.hex, "CREATE INDEX hex4 ON TOIteam (Team(3), season, gamestate)"); dbClearResult(res)
    res <- dbSendQuery (db.hex, "CREATE INDEX hex5 ON baselinedata (season, gamestate)"); dbClearResult(res)

    
    dbDisconnect(db.hex)

}
## make.hextally()

update.hextally <- function (seas=seasons,
                             db.hex=connect.sqlite("common-data/hextally.sqlite")
                             ##dbfile="common-data/hextally.sqlite"
                             ) {

    ##db.hex <- connect.sqldb()
    ##db.hex <- dbConnect(SQLite(), dbname=dbfile)

    for (s.ind in 1:length(seas)) {
        ss <- seas[s.ind]
        message ("Hextally ",ss)
        hex.set <- produce.hexshots (get(load(paste0("source-data/nhlscrapr-",ss,".RData"))))

        message ("Hextally shots",ss)
        res <- dbSendQuery(conn = db.hex, paste("DELETE from shots WHERE season = ",ss)); dbClearResult(res)
        dbWriteTable(conn = db.hex, name = "shots",
                     value = as.data.frame(hex.set$shots) %>%
                     mutate(ID = roster.unique$woi.id[ID], season=anac(season), gcode=anac(gcode)),
                     append=TRUE)

        message ("Hextally TOI",ss)
        res <- dbSendQuery(conn = db.hex, paste("DELETE from TOI WHERE season = ",ss)); dbClearResult(res)
        dbWriteTable(conn = db.hex, name = "TOI",
                     value = as.data.frame(hex.set$TOI) %>% mutate(ID = roster.unique$woi.id[ID], season=anac(season), gcode=anac(gcode)),
                     append=TRUE)

        message ("Hextally shotsteam",ss)
        res <- dbSendQuery(conn = db.hex, paste("DELETE from shotsteam WHERE season = ",ss)); dbClearResult(res)
        dbWriteTable(conn = db.hex, name = "shotsteam",
                     value = as.data.frame(hex.set$shots.team) %>% mutate(season=anac(season), gcode=anac(gcode)),
                     append=TRUE)

        message ("Hextally TOIteam",ss)
        res <- dbSendQuery(conn = db.hex, paste("DELETE from TOIteam WHERE season = ",ss)); dbClearResult(res)
        dbWriteTable(conn = db.hex, name = "TOIteam",
                     value = as.data.frame(hex.set$TOI.team) %>% mutate(season=anac(season), gcode=anac(gcode)),
                     append=TRUE)

        message ("Hextally baselinedata",ss)
        res <- dbSendQuery(conn = db.hex, paste("DELETE from baselinedata WHERE season = ",ss)); dbClearResult(res)
        dbWriteTable(conn = db.hex, name = "baselinedata",
                     value = as.data.frame(hex.set$baseline.data) %>% mutate(season=anac(season)),
                     append=TRUE)
        
        ##save (hex.set, file=paste0("common-data/hextally-",ss,".RData"))
    }
        
    dbDisconnect(db.hex)

}
