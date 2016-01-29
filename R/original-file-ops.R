
make.common <- function () {

    message("Making common file")

    load ("source-data/nhlscrapr-core.RData")

    ## add montreal.
    ##mtlgames <- filter (mtlgames, season >= 20052006)
    
    ## OK, here's where we make a big determination.    
    gamestest <- filter(games, season >= 20052006)
##    gamestest$date[match(paste(mtlgames$season,mtlgames$gcode),
##                         paste(gamestest$season,gamestest$gcode))] <- mtlgames$date
    
    seasons <- unique(gamestest$season)

    ## Just in case.
    repl <- grep("^[0-9]+$", gamestest$date)
    gamestest$date[repl] <- as.character(as.Date("1970-01-01") + as.numeric(gamestest$date[repl]))
    
    teams <- unique(as.character(team.colors$team))#; teams <- teams[teams != ""]
    
    message ("Common data done")
    suppressWarnings(dir.create("common-data"))  
    suppressWarnings(dir.create("common-data/games"))  
    
    save(gamestest, roster.master, roster.unique,
         teams, team.colors, seasons, file="common-data/woi-common.RData")
    
    ##write.csv(roster.unique, "common-data/roster.csv")

    out <- gamestest[!is.na(gamestest$date),]
    out <- out[nchar(out$date) > 0,]
    colnames(out)[which(colnames(out)=="date")] <- "GameDate"
    colnames(out)[which(colnames(out)=="homescore")] <- "Home Score"
    colnames(out)[which(colnames(out)=="awayscore")] <- "Away Score"
    colnames(out)[which(colnames(out)=="session")] <- "Session"
    out$status <- c("Irretrievable", "NotStarted", "InProgress", "Complete")[out$status + 1]
    ## remove future games.
    out <- filter(out, as.Date(GameDate) <= as.Date(as.POSIXlt(Sys.time() - 8*3600, "UTC")))
    out <- out[rev(order(out$GameDate)),][1:75,]
    save(out, file="common-data/today-games.RData")
    
    make.short.gametable (out, "common-data/gamestoday.html")
    
}




## Augment shot locations
augment.espn <- function (season="20132014") {
    merge.locs(season)
}
## for (season in seasons[3:11]) augment.espn(season)
augment.spo <- function (season="20142015") {
    ##source("warbase/R/sportsnet-scrape.R")
    merge.locs.sportsnet(season)
}

impute.shot.locs <- function (season="20132014") {
    print(season)
    impute.locations(season)
}


## for (season in seasons[3:11]) impute.shot.locs(season)
make.and.save.single.game.tables <- function (playbyplay, swaps=data.frame (oldID="petanniNA", newID="petanni95", stringsAsFactors=FALSE)) {
    ## playbyplay <- split.data[[rr]]
    ## playbyplay = {load ("source-data/nhlscrapr-20152016.RData"); filter(grand.data, gcode==20001)}
    ## playbyplay = {load ("~/Documents/nhlr/war-on-ice.com/source-data/nhlscrapr-20152016.RData"); filter(grand.data, gcode==20227)}; load ("~/Documents/nhlr/war-on-ice.com/source-data/nhlscrapr-core.RData")

    if (!is.null(swaps)) {
        roster.unique$woi.id[match(swaps$oldID, roster.unique$woi.id)] <- swaps$newID
    }
    
    season <- playbyplay$season[1]
    gcode <- playbyplay$gcode[1]

    gameroster <- {
        load (paste0("nhlr-data/",season,"-",gcode,"-processed.RData"))
        game.info$players
    }
    
    ptrun <- getSingleGameTable(playbyplay, roster.unique)

    playerrun <- ptrun$player.table
    teamrun <- team.table.sub(playbyplay) #ptrun$team.table
    goalierun <- getGoaltenderTable.object(playbyplay, roster.unique)
    
    coplayer <- make.coplay.edge.lists (playbyplay, roster.unique) 

    try(playerrun <- augment.playerrun.teamcomp (playerrun, coplayer))
    
    playbyplay <- switch.number.woiid(playbyplay, roster.unique)
    
    save (playbyplay, playerrun,
          teamrun, goalierun,
          coplayer, gameroster,
          file=paste0("common-data/games/", season, gcode, ".RData"))
    gc()
}




## 55  66  88 106 115 131 158 173 184 212 243
make.and.save.single.game.by.code <- function (season, gcode) {
    ## season="20142015"; gcode="20200"
    load (file=paste0("common-data/games/", season, gcode, ".RData"))
    make.and.save.single.game.tables (playbyplay) 
}

replace.pbp.all <- function () {

    for (ss in seasons) {
        load (paste0("source-data/nhlscrapr-",ss,".RData"))
        
        for (qq in which(gamestest$status == 3 & gamestest$season == ss))  {
            season <- gamestest$season[qq]
            gcode <- gamestest$gcode[qq]
            
            message(season,gcode)
            load (file=paste0("common-data/games/", season, gcode, ".RData"))
            
            playbyplay <- switch.number.woiid(filter(grand.data, gcode == gamestest$gcode[qq]),
                                                     roster.unique)
            
            save (playbyplay, playerrun,
                  teamrun, goalierun,
                  coplayer, gameroster,
                  file=paste0("common-data/games/", season, gcode, ".RData"))

        }
    }
}


replace.team.by.code <- function (season, gcode) {
    ## season="20072008"; gcode="20001"
    load (file=paste0("common-data/games/", season, gcode, ".RData"))
    season <- playbyplay$season[1]
    gcode <- playbyplay$gcode[1]
    #playerrun <- getSingleGameTable(playbyplay)
    teamrun <- team.table.sub(playbyplay)
    #goalierun <- getGoaltenderTable.object(playbyplay)
    save (playbyplay, playerrun, teamrun, goalierun,
          file=paste0("common-data/games/", season, gcode, ".RData"))
}

replace.team.all <- function () {
    foreach (qq = which(gamestest$status == 3)) %dopar%
    ##for (qq in 1:nrow(gamestest))
        tryCatch(replace.team.by.code (gamestest$season[qq],gamestest$gcode[qq]), error=function(cond) message("Fail ",qq))
}


replace.goalie.by.code <- function (season, gcode) {
    ## season="20072008"; gcode="20001"
    load (file=paste0("common-data/games/", season, gcode, ".RData"))
    goalierun <- getGoaltenderTable.object(playbyplay)
    save (playbyplay, playerrun,
          teamrun, goalierun,
          coplayer, gameroster,
          file=paste0("common-data/games/", season, gcode, ".RData"))
}

replace.goalie.all <- function () {
    registerDoMC(12)
    foreach (qq = 1:nrow(gamestest)) %dopar%
##    for (qq in 1:nrow(gamestest))    which(gamestest$season == "20072008
        tryCatch(replace.goalie.by.code (gamestest$season[qq],gamestest$gcode[qq]),
                 error=function(cond) message("Fail ",gamestest$season[qq],gamestest$gcode[qq]))
}


## Player sequences.


make.game.files <- function (seas=seasons, datesback=NULL, use.gameids=FALSE, gameids=NULL, start=1) {
    ## seas=seasons; datesback=NULL; use.gameids=TRUE; gameids="2015201620001"; start=1
    ## update this for "today" to be an option.

    if (use.gameids) seas <- unique (substr(gameids,1,8))
    
    for (ss in seas) {
        load (paste0("source-data/nhlscrapr-",ss,".RData"))
        grand.data <- mutate(grand.data, season = anac(season))
        split.data <- by(grand.data, grand.data$gcode, identity)
        gcodes <- names(split.data) 
        ## print(gcodes)
        
        ##d1 <- foreach (rr = 1:length(gcodes)) %dopar%
        for (rr in start:length(gcodes))
            tryCatch({
            date <- gamestest$date[match(paste0(ss, gcodes[rr]),
                                         paste0(gamestest$season, gamestest$gcode))]
            status <- gamestest$status[match(paste0(ss, gcodes[rr]),
                                             paste0(gamestest$season, gamestest$gcode))]
            ## message(date, " ", status)
            
            if (!use.gameids) {
                doit <- is.null(datesback)
                if (!is.null(datesback) && ((as.Date(as.POSIXlt(Sys.time(), "GMT")-9*3600) - as.Date(date)) %in% datesback) && status > 1) doit <- TRUE
            } else {
                doit <- paste0(ss, gcodes[rr]) %in% gameids
            }
            
            if (doit) make.and.save.single.game.tables(split.data[[rr]])
##            return(TRUE)
        }, error=function(cond) message (cond," Goof-up ",ss," ",gcodes[rr]))
        
    }
}
## make.game.files(seasons[-c(1,12)])

replace.coplay.by.code <- function (season, gcode) {
    ## season="20072008"; gcode="20001"
    
    load (file=paste0("common-data/games/", season, gcode, ".RData"))
    coplayer <- make.coplay.edge.lists (playbyplay)
    save (playbyplay, playerrun,
          teamrun, goalierun,
          coplayer, gameroster,
          file=paste0("common-data/games/", season, gcode, ".RData"))
}

replace.coplay.all <- function () {
    registerDoMC(10)
    foreach (qq = which(gamestest$status==3)) %dopar%
        tryCatch(replace.coplay.by.code (gamestest$season[qq],gamestest$gcode[qq]),
                 error=function(cond) message("Fail ",gamestest$season[qq],gamestest$gcode[qq]))
}

replace.gamefile.cols.by.code <- function (season, gcode) {
    ## season="20022003"; gcode="20748"
   
    load (file=paste0("common-data/games/", season, gcode, ".RData"))
    playerrun <- playerrun[,which(!(colnames(playerrun) %in% c("TOIPctC","TOIPctT","FenwickC","FenwickT","CorsiC","CorsiT","seasonTOIpct","seasonFpct","seasonCpct")))]
    playerrun <- mutate (playerrun,
                         tCF60=NA, tCA60=NA, tFF60=NA, tFA60=NA, tTOI60=NA,
                         cCF60=NA, cCA60=NA, cFF60=NA, cFA60=NA, cTOI60=NA)
    save (playbyplay, playerrun,
          teamrun, goalierun,
          coplayer, gameroster,
          file=paste0("common-data/games/", season, gcode, ".RData"))
}
replace.gamefile.cols.all <- function () {
    registerDoMC(10)
    foreach (qq = which(gamestest$status==3)) %dopar%
        tryCatch(replace.gamefile.cols.by.code (gamestest$season[qq],gamestest$gcode[qq]),
                 error=function(cond) message("Fail ",gamestest$season[qq],gamestest$gcode[qq]))
}
## replace.gamefile.cols.all()


#augment.playerrun.teamcomp <- function (playerrun, coplayer)
augment.tc.by.code <- function (season, gcode) {
    ## season=20152016; gcode=20001
    load (file=paste0("common-data/games/", season, gcode, ".RData"))
    playerrun <- augment.playerrun.teamcomp (playerrun, coplayer)
    save (playbyplay, playerrun,
          teamrun, goalierun,
          coplayer, gameroster,
          file=paste0("common-data/games/", season, gcode, ".RData"))
}
replace.tc.all <- function (thesegames=gamestest) {
    ##library(doMC)
    ##registerDoMC(parcores)
    gms <- filter(thesegames, status == 3)
    foreach (qq = 1:nrow(gms)) %do%
    tryCatch(augment.tc.by.code (gms$season[qq],gms$gcode[qq]),
             error=function(cond) message("Fail ",gms$season[qq],gms$gcode[qq]))
}
#replace.tc.all()





## for (season in seasons[3:11]) impute.shot.locs(season)
replace.pbp.single <- function (playbyplay) {
    ## playbyplay <- split.data[[rr]]
    ## playbyplay = {load ("source-data/nhlscrapr-20142015.RData"); subset(grand.data, gcode==20001)}
    
    season <- playbyplay$season[1]
    gcode <- playbyplay$gcode[1]
    message ("Replace pbp ",season,gcode)
    pbp2 <- playbyplay
    load (paste0("common-data/games/", season, gcode, ".RData"))
    playbyplay <- switch.number.woiid(pbp2, roster.unique)

    save (playbyplay, playerrun, teamrun, goalierun, coplayer, gameroster,
          file=paste0("common-data/games/", season, gcode, ".RData"))
}

replace.pbp.all <- function (seas=seasons, datesback=NULL, use.gameids=FALSE, gameids=NULL, start=1) {
    
    ## update this for "today" to be an option.
    for (ss in seas) {
        load (paste0("source-data/nhlscrapr-",ss,".RData"))
        split.data <- by(grand.data, grand.data$gcode, identity)
        gcodes <- names(split.data) 
        for (rr in start:length(gcodes))
            tryCatch({
            date <- gamestest$date[match(paste0(ss, gcodes[rr]),
                                         paste0(gamestest$season, gamestest$gcode))]
            status <- gamestest$status[match(paste0(ss, gcodes[rr]),
                                             paste0(gamestest$season, gamestest$gcode))]            
            if (!use.gameids) {
                doit <- is.null(datesback)
                if (!is.null(datesback) && ((as.Date(as.POSIXlt(Sys.time(), "GMT")-9*3600) - as.Date(date)) %in% datesback) && status > 1) doit <- TRUE
            } else {doit <- paste0(ss, gcodes[rr]) %in% gameids}
            
            if (doit) replace.pbp.single (split.data[[rr]])
            
        }, error=function(cond) message (cond," Goof-up ",ss," ",gcodes[rr]))
    }
}
## make.game.files(seasons[-c(1,12)])


make.JSON.file <- function (seasongcode = "2014201520001") {

    load (paste0("common-data/games/",seasongcode,".RData"))

    converted <- toJSON (list (coplayer=coplayer,
                               gameroster=gameroster,
                               goalierun=goalierun,
                               playbyplay=playbyplay,
                               playerrun=playerrun,
                               teamrun=teamrun))
    write (converted, file=paste0("common-data/games/",seasongcode,".json"))
    
    write.csv (coplayer, file=paste0("common-data/games/coplayer",seasongcode,".csv"))
    write.csv (gameroster, file=paste0("common-data/games/gameroster",seasongcode,".csv"))
    write.csv (goalierun, file=paste0("common-data/games/goalierun",seasongcode,".csv"))
    write.csv (playbyplay, file=paste0("common-data/games/playbyplay",seasongcode,".csv"))
    write.csv (playerrun, file=paste0("common-data/games/playerrun",seasongcode,".csv"))
    write.csv (teamrun, file=paste0("common-data/games/teamrun",seasongcode,".csv"))
    
}


make.rink.bias <- function () {

    registerDoMC(5)
    dangers <- c("","1","2","3","4")

    aa1 <- foreach (dd = dangers) %dopar% rbind(make.all.adjustments(1, dd),
                        make.all.adjustments(2, dd),
                        make.all.adjustments(4, dd))
    all.adjust <- rbind_all(aa1)

    save(all.adjust, file="source-data/rink-counts.RData")
    save(all.adjust, file="warbase/data/rinkcounts.RData")
    write.csv(all.adjust, "source-data/all-rink-adjustments.csv")

}


