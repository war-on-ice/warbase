



create.pl.tm.gl.db.alternate <- function (games=gamestest,
                                          do.player=TRUE, do.team=TRUE, do.goalie=TRUE,
                                          player.dbname="common-data/player2.sqlite",
                                          team.dbname="common-data/team2.sqlite"
                                          ## , in.reverse=TRUE
                                          ) {

    db.player <- dbConnect(SQLite(), dbname=player.dbname)
    db.team <- dbConnect(SQLite(), dbname=team.dbname)

    file.single.pl <- function (playersub, append=TRUE, overwrite=FALSE) {
        pieces <- with(playersub, c(score.diff.cat[1], gamestate[1], period[1]))
        dbWriteTable(conn = db.player, name = paste0("player", paste(pieces,collapse="_")),
                     value = as.data.frame(playersub), append=append, overwrite=overwrite)
        data.frame()
    }

    to.include <- which(games$status == 3)
    ##if (in.reverse) to.include <- rev(to.include)

    ##seasons <- unique (games$season[to.include])
    
    for (ss in rev(seasons)) try({

        to.include <- which(games$status == 3 & games$season == ss)
        games.1 <- lapply(to.include, function(gg) {
        ##message (games$season[gg], " ", games$gcode[gg])
            load (paste0("common-data/games/", games$season[gg], games$gcode[gg], ".RData"))
            list (pl=filter(playerrun, period==0),
                  tm=teamrun,
                  gl=goalierun)
        })
        playerrun <- rbind_all(lapply(games.1, function(gg) gg[[1]]))
        teamrun <- rbind_all(lapply(games.1, function(gg) gg[[2]]))
        goalierun <- rbind_all(lapply(games.1, function(gg) gg[[2]]))
        
        if (do.player) r1 <- playerrun %>% 
            group_by (score.diff.cat, gamestate, period) %>% do (file.single.pl(.))
        if (do.team) r2 <- dbWriteTable(conn = db.team, name = "team",
                                        value = as.data.frame(teamrun), append=TRUE)
        if (do.goalie) r3 <- dbWriteTable(conn = db.team, name = "goalie",
                                          value = as.data.frame(goalierun), append=TRUE)
    })

    dbDisconnect(db.player)
    dbDisconnect(db.team)
    #dbDisconnect(db.goalie)    

}



create.pl.tm.gl.db <- function (games=gamestest,
                                do.player=TRUE, do.team=TRUE, do.goalie=TRUE,
                                player.dbname="common-data/player.sqlite",
                                team.dbname="common-data/team.sqlite", in.reverse=TRUE) {

    db.player <- dbConnect(SQLite(), dbname=player.dbname)
    db.team <- dbConnect(SQLite(), dbname=team.dbname)

    file.single.pl <- function (playersub, append=TRUE, overwrite=FALSE) {
        pieces <- with(playersub, c(score.diff.cat[1], gamestate[1], period[1]))
        dbWriteTable(conn = db.player, name = paste0("player", paste(pieces,collapse="_")),
                     value = as.data.frame(playersub), append=append, overwrite=overwrite)
        data.frame()
    }

    to.include <- which(games$status == 3)
    if (in.reverse) to.include <- rev(to.include)
    
    for (gg in to.include) try({
        message (games$season[gg], " ", games$gcode[gg])
        load (paste0("common-data/games/", games$season[gg], games$gcode[gg], ".RData"))
        
        if (do.player) r1 <- playerrun %>% filter (period == 0) %>%
            group_by (score.diff.cat, gamestate, period) %>% do (file.single.pl(.))
        if (do.team) r2 <- dbWriteTable(conn = db.team, name = "team",
                                        value = as.data.frame(teamrun), append=TRUE)
        if (do.goalie) r3 <- dbWriteTable(conn = db.team, name = "goalie",
                                          value = as.data.frame(goalierun), append=TRUE)
    })

    dbDisconnect(db.player)
    dbDisconnect(db.team)
    #dbDisconnect(db.goalie)    

}




create.tm.gl.db <- function (games=gamestest,
                             team.dbname="common-data/team.sqlite", in.reverse=TRUE) {

    db.team <- dbConnect(SQLite(), dbname=team.dbname)

    to.include <- which(games$status == 3)
    if (in.reverse) to.include <- rev(to.include)
    
    for (gg in to.include) try({
        message (games$season[gg], " ", games$gcode[gg])
        load (paste0("common-data/games/", games$season[gg], games$gcode[gg], ".RData"))
        
        r2 <- dbWriteTable(conn = db.team, name = "team",
                           value = as.data.frame(teamrun), append=TRUE)
        r3 <- dbWriteTable(conn = db.team, name = "goalie",
                           value = as.data.frame(goalierun), append=TRUE)
    })

    dbDisconnect(db.team)

}




augment.tm.gl.db <- function (games=gamestest,
                              team.dbname="common-data/team.sqlite", in.reverse=TRUE) {

    ## Establish games in system.
    reduced.connect <- src_sqlite(team.dbname)
    primer <- tbl(reduced.connect, sql("SELECT season, gcode FROM team")) %>% group_by (season, gcode) %>% summarize (nn = n()) %>% as.data.frame

    db.team <- dbConnect(SQLite(), dbname=team.dbname)
    
    ## %>% filter (p1 %in% player.set, season == playerrun$season[1])
    possibles <- subset(games, status == 3)
    new.to.include <- setdiff (paste (possibles$season, possibles$gcode),
                               paste (primer$season, primer$gcode))
    if (in.reverse) new.to.include <- rev(new.to.include)
    
    if (length(new.to.include) > 0) for (gg in new.to.include) try({
        message (gg)
        load (paste0("common-data/games/", substr(gg,1,8), substr(gg,10,14), ".RData"))
        r2 <- dbWriteTable(conn = db.team, name = "team",
                           value = as.data.frame(teamrun), append=TRUE)
        r3 <- dbWriteTable(conn = db.team, name = "goalie",
                           value = as.data.frame(goalierun), append=TRUE)
    })
    dbDisconnect(db.team)

}

replace.game.tm.gl.db <- function (season, gcode,    ##games=gamestest,
                                   team.dbname="common-data/team.sqlite") {

    message ("Team/goalies: Replacing game ",season,gcode)
    db.team <- dbConnect(SQLite(), dbname=team.dbname)
    try({
        load (paste0("common-data/games/", season, gcode, ".RData"))

        dbSendQuery(conn = db.team, paste("DELETE from team WHERE season = ",season, "AND gcode =",gcode))
        dbSendQuery(conn = db.team, paste("DELETE from goalie WHERE season = ",season, "AND gcode =",gcode))

        r2 <- dbWriteTable(conn = db.team, name = "team",
                           value = as.data.frame(teamrun), append=TRUE)
        r3 <- dbWriteTable(conn = db.team, name = "goalie",
                           value = as.data.frame(goalierun), append=TRUE)
    })
    
    dbDisconnect(db.player)

}



## source("woi-makedata.R"); augment.pl.tm.gl.db (games=gamestest, player.dbname="common-data/playercomplete.sqlite", team.dbname="common-data/teamcomplete.sqlite", in.reverse=TRUE)




## source("woi-makedata.R"); augment.pl.tm.gl.db (games=gamestest, do.player=FALSE, team.dbname="common-data/team.sqlite", in.reverse=TRUE)


augment.pl.db <- function (games=gamestest,
                           player.dbname="common-data/player.sqlite",
                           in.reverse=TRUE) {

    ## Establish games in system.
    reduced.connect <- src_sqlite(player.dbname)
    primer <- tbl(reduced.connect, sql("SELECT season, gcode FROM player7_7_0")) %>% group_by (season, gcode) %>% summarize (nn = n()) %>% as.data.frame

    db.player <- dbConnect(SQLite(), dbname=player.dbname)

    file.single.pl <- function (playersub, append=TRUE, overwrite=FALSE) {
        pieces <- with(playersub, c(score.diff.cat[1], gamestate[1], period[1]))
        dbWriteTable(conn = db.player, name = paste0("player", paste(pieces,collapse="_")),
                     value = as.data.frame(playersub), append=append, overwrite=overwrite)
        data.frame()
    }
    
    ## %>% filter (p1 %in% player.set, season == playerrun$season[1])
    possibles <- subset(games, status == 3)
    new.to.include <- setdiff (paste (possibles$season, possibles$gcode),
                               paste (primer$season, primer$gcode))
    
    if (in.reverse) new.to.include <- rev(new.to.include)
    
    if (length(new.to.include) > 0) for (gg in new.to.include) try({
        message (gg)
        load (paste0("common-data/games/", substr(gg,1,8), substr(gg,10,14), ".RData"))
        r1 <- playerrun %>% filter (period == 0) %>% group_by (score.diff.cat, gamestate, period) %>%
            do (file.single.pl(.))
    })

    dbDisconnect(db.player)

}

replace.game.pl.db <- function (season, gcode,    ##games=gamestest,
                                player.dbname="common-data/player.sqlite") {

    db.player <- dbConnect(SQLite(), dbname=player.dbname)
    
    file.single.pl <- function (playersub, append=TRUE, overwrite=FALSE) {
        pieces <- with(playersub, c(score.diff.cat[1], gamestate[1], period[1]))
        tablename <- paste0("player", paste(pieces,collapse="_"))
        
        ## purge old rows
        dbSendQuery(conn = db.player, paste("DELETE from",tablename,"WHERE season = ",playersub$season[1], "AND gcode =",playersub$gcode[1]))
       
        dbWriteTable(conn = db.player, name = tablename,
                     value = as.data.frame(playersub), append=append, overwrite=overwrite)
        data.frame()
    }
    
    try({
        load (paste0("common-data/games/", season, gcode, ".RData"))
        r1 <- playerrun %>% filter (period == 0) %>% group_by (score.diff.cat, gamestate, period) %>%
            do (file.single.pl(.))
    })
    
    dbDisconnect(db.player)

}



replace.game.all.db <- function (season, gcode,    ##games=gamestest,
                                 player.dbname="common-data/player.sqlite",
                                 team.dbname="common-data/team.sqlite") {

    db.player <- dbConnect(SQLite(), dbname=player.dbname)
    db.team <- dbConnect(SQLite(), dbname=team.dbname)
    
    file.single.pl <- function (playersub, append=TRUE, overwrite=FALSE) {
        pieces <- with(playersub, c(score.diff.cat[1], gamestate[1], period[1]))
        tablename <- paste0("player", paste(pieces,collapse="_"))
        
        ## purge old rows
        dbSendQuery(conn = db.player, paste("DELETE from",tablename,"WHERE season = ",playersub$season[1], "AND gcode =",playersub$gcode[1]))
       
        dbWriteTable(conn = db.player, name = tablename,
                     value = as.data.frame(playersub), append=append, overwrite=overwrite)

        data.frame()
    }
    message ("Replacing goalie player team ", season, gcode)
    try({
        load (paste0("common-data/games/", season, gcode, ".RData"))
        r1 <- playerrun %>% filter (period == 0) %>% group_by (score.diff.cat, gamestate, period) %>%
            do (file.single.pl(.))

        dbSendQuery(conn = db.team, paste("DELETE from team WHERE season = ",season, "AND gcode =",gcode))
        dbSendQuery(conn = db.team, paste("DELETE from goalie WHERE season = ",season, "AND gcode =",gcode))

        r2 <- dbWriteTable(conn = db.team, name = "team",
                           value = as.data.frame(teamrun), append=TRUE)
        r3 <- dbWriteTable(conn = db.team, name = "goalie",
                           value = as.data.frame(goalierun), append=TRUE)
         
    })
    
    dbDisconnect(db.player)
    dbDisconnect(db.team)

}

## replace.game.all.db
## for (gcode in 20001:21194) replace.game.all.db("20142015", gcode, player.dbname="common-data/playercomplete.sqlite", team.dbname="common-data/teamcomplete.sqlite")



## augment.pl.db (player.db="common-data/playercomplete.sqlite")
## source("woi-makedata.R"); augment.pl.db (player.db="common-data/playercomplete.sqlite")
    ## create.pl.tm.gl.db()



create.uplayer.db <- function (games=gamestest,
                               player.dbname="common-data/uplayer.sqlite",
                               in.reverse=TRUE) {

    db.player <- dbConnect(SQLite(), dbname=player.dbname)
    to.include <- which(games$status == 3)
    if (in.reverse) to.include <- rev(to.include)
    
    for (gg in to.include) try({
        message (games$season[gg], " ", games$gcode[gg])
        load (paste0("common-data/games/", games$season[gg], games$gcode[gg], ".RData"))
        r1 <- dbWriteTable(conn = db.player, name = "player",
                           value = as.data.frame(playerrun %>% filter (period == 0)), append=TRUE)
    })

    dbDisconnect(db.player)

}

