
########################################################################
##
## ESPN stuff.

## Go through ESPN NHL data, day by day, get game data.
library(dplyr)

get.one.day <- function (day, direc="espn-games/") {
    message(day)
    mainpage <- readLines(paste0("http://scores.espn.go.com/nhl/scoreboard?date=",day))
    gameids <- unique(unlist(regmatches (mainpage, gregexpr("gameId=[0-9]+", mainpage))))
    gamebits <- lapply(gameids, function(this.id) {
        readLines(paste0("http://sports.espn.go.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&",this.id))
    })
    save (mainpage, gameids, gamebits, file=paste0(direc,"espn-",day,".RData"))
}

parse.game <- function (gamefeed) tryCatch({
    ## load("espn-games/espn-20141009.RData"); gamefeed = gamebits[[1]]

    eventidnames = c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL",
        "STOP", "PRDY", "PSTR", "PEND", "PERD", "SOC", "GEnd", "SOut",
        "error", "TAKE", "GIVE", "early intermission", "nothing", "nothing")
    eventidcodes = as.character(c(502, 503, 504, 505, 506, 507, 508, 509,
        516, 517, 518, 519, 520, 521, 522, 0, 9999, 1401, 1402, -2147483648, 1, 5))

    gameinfo <- unlist(regmatches (gamefeed, gregexpr("<Game.*/Game>", gamefeed)))
    eventinfo <- unlist(regmatches (gamefeed, gregexpr("<Play.*?/Play>", gamefeed)))
    
    game.split <- unlist(strsplit(gameinfo, "~"))
    teams <- toupper(game.split[c(30, 25)])
    teams[teams == "WAS"] <- "WSH"; teams[teams == "LOS"] <- "L.A"
    teams[teams == "TAM"] <- "T.B"; teams[teams == "SAN"] <- "S.J"
    teams[teams == "NJD"] <- "N.J"; teams[teams == "CLS"] <- "CBJ"
    teams[teams == "MON"] <- "MTL";
    GameDate <- as.character(as.Date (game.split[8], "%B %d, %Y"))

    event.split <- t(do.call(cbind, strsplit (eventinfo, "[\\[~]")))
    event.split[,2] <- eventidnames[match(event.split[,5], eventidcodes)]

    event.split <- event.split[,c(2,3,4,6,7,11)]  #
    colnames(event.split) <- c("etype","xcoord","ycoord","time","period","event.description")
    event.split <- event.split[,1:5]
    
    event.split <- as.data.frame(cbind (GameDate, awayteam=teams[1], hometeam=teams[2], event.split), stringsAsFactors=FALSE)
    timesplits <- do.call(rbind, strsplit(event.split$time, ":"))
    seconds <- 1200*(as.numeric(event.split$period)-1) + as.numeric(timesplits[,1])*60 + as.numeric(timesplits[,2])
    event.split$seconds <- seconds
    return(event.split)

}, error=function(cond) NULL)


parse.day <- function (GameDate=as.character(Sys.Date()), get=FALSE, direc="espn-games/") {
    ## GameDate="20131004"
    GameDate <- gsub("-","", GameDate); print(GameDate)
    if (get) get.one.day(GameDate, direc)
    tryCatch(load(paste0(direc,"espn-",GameDate,".RData")),
             error=function(cond) {get.one.day(GameDate, direc); load(paste0(direc,"espn-",GameDate,".RData"))})
    evtable <- do.call(rbind, lapply(gamebits, parse.game))
    evtable
}

prep.season <- function(year1=2005, savethis=TRUE, direc="source-data/") {

    dates <- seq(as.Date(paste0(year1,"-07-01")),
                 min(as.Date(paste0(year1+1,"-06-30")), Sys.Date()), by=1)
    gimme <- lapply(dates, parse.day)
    event.table <- do.call(rbind, gimme)
    ## games.list <- unique(paste0(gimme.too$GameDate, gimme.too$awayteam, gimme.too$hometeam))
    if (year1 < 2011) event.table$hometeam[event.table$hometeam=="WPG"] <- "ATL"
    if (year1 < 2011) event.table$awayteam[event.table$awayteam=="WPG"] <- "ATL"
    if (year1 < 2014) event.table$hometeam[event.table$hometeam=="ARI"] <- "PHX"
    if (year1 < 2014) event.table$awayteam[event.table$awayteam=="ARI"] <- "PHX"
    if (savethis) save (event.table, file=paste0(direc,"espn-", year, year+1,".RData"))
    return(event.table)
    
}

#for (year in 2005:2014) event.table <- prep.season(year)

merge.locs <- function (season="20132014", direc="source-data/", common="common-data/", verbose=TRUE) {

    if (verbose) message ("merge.locs ",season)
    load(paste0(direc, "nhlscrapr-",season,".RData"))
    load(paste0(direc, "espn-",season,".RData"))
    load(paste0("common-data/woi-common.RData"))
    #find missing locations.

    event.table <- subset(event.table, etype %in% c("GOAL","SHOT","MISS","BLOCK"))
    event.table$xcoord <- as.numeric(event.table$xcoord)
    event.table$ycoord <- as.numeric(event.table$ycoord)
    
    espn.bit <- with(event.table, paste0(GameDate, awayteam, etype, seconds))
    nhl.date <- gamestest$date[match(paste(grand.data$season, grand.data$gcode),
                                     paste(gamestest$season, gamestest$gcode))]
                                     
    nhl.bit <- with(grand.data, paste0(nhl.date, awayteam, etype, seconds))
    missing.x <- which (grand.data$etype %in% c("GOAL","SHOT","MISS","BLOCK") &
                        is.na(grand.data$xcoord))

    #write.csv(nhl.bit[missing.x], "nhlx.csv"); write.csv(espn.bit, "espnx.csv")
    trymatch <- match(nhl.bit[missing.x], espn.bit)
    matches <- missing.x[!is.na(trymatch)]
    print(table(is.na(trymatch)))
    
    grand.data[matches, c("xcoord", "ycoord")] <-
        event.table[trymatch[!is.na(trymatch)], c("xcoord", "ycoord")]
    grand.data$import.ies[matches] <- 2
    
    
    save(grand.data, file=paste0(direc, "nhlscrapr-",season,".RData"))
    
}


test.routines <- function () {

    t1 <- parse.day("2005-11-01", get=FALSE)
    dates <- seq(as.Date("2005-10-01"), as.Date("2006-06-30"), by=1)
    gimme <- lapply(dates, parse.day)
    gimme.too <- do.call(rbind, gimme)
    games.list <- unique(paste0(gimme.too$GameDate, gimme.too$awayteam, gimme.too$hometeam))

    set.one <- lapply(dates, parse.day)
    fulljob <- do.call(rbind, set.one)

    dates <- seq(as.Date("2014-11-10"), as.Date("2014-11-21"), by=1)
    datesstraight <- gsub("-","", dates)
    
    s1 <- lapply(gamebits, parse.game)
    
    for (kk in 1:length(s1)) print(head(s1[[kk]],2))
    
    write.csv (s1[[1]], "testespn.csv")
    for (dd in rev(datesstraight)) try(get.one.day(dd), TRUE)
    lapply (datesstraight, function(dd) try(get.one.day(dd), TRUE))
}


################################################################################
##
## Sportsnet bits.

## Go through Sportsnet NHL data, day by day, get game data.

##    {"id":53,
##     "location":[-65,0],
##     "event":"shot-blocked",
##     "teamId":24,
##     "participants":[{"playerId":8471887,"teamId":5,"role":"shooter","cumulative":null},
##                     {"playerId":8473933,"teamId":24,"role":"blocker","cumulative":null}],
##     "scoreAway":0,"scoreHome":0,"time":"19:43",
##     "elapsed":"00:17",
##     "period":1,
##     "description":"Shot by Patric Hornqvist blocked by Ben Lovejoy",
##     "strength":"even-strength",
##     "sequence":10017000007,
##     "unixstamp":1412896276,
##     "goalZone":null,
##     "type":"block",
##     "momentumAway":60,
##     "winProbabilityAway":54}


get.one.day.sportsnet <- function (day, direc="espn-games/") {
    #day="2015-10-07"
    message(day)
    mainpage <- readLines(paste0("http://www.sportsnet.ca/hockey/nhl/scores/?datepicker-date=",day))
    gameids <- unique(unlist(regmatches (mainpage, gregexpr("http://www.sportsnet.ca/hockey/nhl/livetracker/game/[0-9]+", mainpage))))

    gamebits <- lapply(gameids, function(this.id) readLines(this.id))
    save (mainpage, gameids, gamebits, file=paste0(direc,"spo-",day,".RData"))
}

parse.game.sportsnet <- function (gamefeed, GameDate) tryCatch({
    ## load("espn-games/spo-2014-10-08.RData"); gamefeed = gamebits[[4]]
    ## {"game":{"id":20007
    gamefeed <- gsub("null", "420", gamefeed)
    
    gameinfo1 <- unlist(regmatches (gamefeed, gregexpr('\\{"game":\\{"id":[0-9]+', gamefeed)))
    gameinfo <- unlist(regmatches (gameinfo1, gregexpr('[0-9]+', gameinfo1)))

    eventinfo <- unlist(regmatches (gamefeed, gregexpr('\\{"id":[0-9]+,"loc.*?"momentum', gamefeed)))
    event.split.prime <- gsub('.*"location":\\[([0-9-]+),([0-9-]+)\\],"event":"([a-z-]+).*"elapsed":"([0-9:]+)".*"period":([0-9]+).*"type":"([a-z]+).*', '\\1;\\2;\\3;\\4;\\5;\\6',eventinfo)
    event.split.2 <- do.call(rbind, strsplit(event.split.prime,";"))
    
    timesplits <- do.call(rbind, strsplit(event.split.2[,4], ":"))
    
    event.split <- data.frame(gcode=gameinfo,
                              xcoord=as.numeric(event.split.2[,1]),
                              ycoord=as.numeric(event.split.2[,2]),
                              etype=c("GOAL","SHOT","MISS","BLOCK","PENL","HIT")[match(event.split.2[,3], c("score","shot-on-goal","shot-missed","shot-blocked","penalty","hit"))],
                              period=as.numeric(event.split.2[,5]),
                              seconds=1200*(as.numeric(event.split.2[,5])-1) + as.numeric(timesplits[,1])*60 + as.numeric(timesplits[,2]))
                              
    return(event.split)

}, error=function(cond) {message("whoops ",GameDate); NULL})


parse.day.sportsnet <- function (GameDate=as.character(Sys.Date()), getdl=FALSE, direc="espn-games/") {
    ## GameDate="2015-10-07"
    message ("Parsing ",GameDate)
    if (getdl) get.one.day.sportsnet(GameDate, direc)
    tryCatch(load(paste0(direc,"spo-",GameDate,".RData")),
             error=function(cond) {get.one.day.sportsnet(GameDate, direc);
                                   load(paste0(direc,"spo-",GameDate,".RData"))})
    ev.table <- NULL
    tryCatch(evtable <- do.call(rbind, lapply(gamebits, parse.game.sportsnet, GameDate)),
             error=function(cond) {message (cond, " -- no games yet"); evtable=NULL})
    evtable
}

prep.season.sportsnet <- function(year1=2015, savethis=TRUE, arch.direc="source-data/") {

    dates <- seq(as.Date(paste0(year1,"-10-08")),
                 min(as.Date(paste0(year1+1,"-06-30")), Sys.Date()), by=1)
    gimme <- lapply(dates, parse.day.sportsnet)
    event.table <- do.call(rbind, gimme)
    event.table$season <- paste0(year1, year1+1)

    if (savethis) save (event.table, file=paste0(arch.direc,"spo-", year1, year1+1,".RData"))
    return(event.table)
    
}

add.today.sportsnet <- function(year1=2015, date=0, arch.direc="source-data/") {

    today.now <- format(as.POSIXct(Sys.time(), tz="America/Los_Angeles"), tz="America/Los_Angeles", usetz=TRUE)
    today <- as.Date(today.now)
    
    this.date <- as.character(today-date)
    gimme <- parse.day.sportsnet(this.date, getdl=TRUE)

    if (!is.null(gimme)) {
        gimme$season <- paste0(year1, year1+1)

        load (file=paste0(arch.direc,"spo-", year1, year1+1,".RData"))
        event.table <- rbind(event.table[!(event.table$gcode %in% unique(gimme$gcode)),],
                             gimme)
        
        save (event.table, file=paste0(arch.direc,"spo-", year1, year1+1,".RData"))
    }
    return(TRUE)
    
}

add.dayrange.sportsnet <- function(days, arch.direc="source-data/") {

    ##today.now <- format(as.POSIXct(Sys.time(), tz="America/Los_Angeles"), tz="America/Los_Angeles", usetz=TRUE)
    ##today <- as.Date(today.now)
    
    ##this.date <- as.character(today-date)
    gimme <- rbind_all(lapply(days, function(dd) parse.day.sportsnet(dd, getdl=TRUE)))
    
    if (!is.null(gimme)) {
        year1 <- as.numeric(substr(days[1], 1, 4)) - 1*(as.numeric(substr(days[1], 6,7)) < 7)
        gimme$season <- paste0(year1, year1+1)

        if (file.exists(paste0(arch.direc,"spo-", year1, year1+1,".RData"))) {
            load (file=paste0(arch.direc,"spo-", year1, year1+1,".RData"))
            event.table <- rbind(event.table[!(event.table$gcode %in% unique(gimme$gcode)),],
                                 gimme)
        } else {
            event.table <- gimme
        }
        
        save (event.table, file=paste0(arch.direc,"spo-", year1, year1+1,".RData"))
    }
    return(TRUE)
    
}



merge.locs.sportsnet <- function (season="20152016", direc="source-data/", common="common-data/", verbose=TRUE) {
    ## season="20142015"; direc="source-data/"; common="common-data/"; verbose=TRUE
    
    if (verbose) message ("merge.locs sportsnet ",season)
    load(paste0(direc, "nhlscrapr-",season,".RData"))
    load(paste0(direc, "spo-",season,".RData"))
    load(paste0("common-data/woi-common.RData"))
    #find missing locations.

    event.table <- subset(event.table, etype %in% c("GOAL","SHOT","MISS","BLOCK"))
    #event.table$xcoord <- as.numeric(event.table$xcoord)
    #event.table$ycoord <- as.numeric(event.table$ycoord)
    
    spo.bit <- with(event.table, paste0(gcode, etype, seconds))
#    nhl.date <- gamestest$date[match(paste(grand.data$season, grand.data$gcode),
#                                     paste(gamestest$season, gamestest$gcode))]
                                     
    nhl.bit <- with(grand.data, paste0(gcode, etype, seconds))
    missing.x <- which (grand.data$etype %in% c("GOAL","SHOT","MISS","BLOCK") &
                        is.na(grand.data$xcoord))

    #write.csv(nhl.bit[missing.x], "nhlx.csv"); write.csv(espn.bit, "espnx.csv")
    trymatch <- match(nhl.bit[missing.x], spo.bit)
    matches <- missing.x[!is.na(trymatch)]
    print(table(is.na(trymatch)))
    
    grand.data[matches, c("xcoord", "ycoord")] <-
        event.table[trymatch[!is.na(trymatch)], c("xcoord", "ycoord")]
    grand.data$import.ies[matches] <- 3

    
    save(grand.data, file=paste0(direc, "nhlscrapr-",season,".RData"))
    
}

#############################################################################
##
## Imputation bits.

impute.locations <- function (season, direc="source-data/", common="common-data/", verbose=TRUE) {
    ## season <- "20142015"
    
    load(paste0(direc, "nhlscrapr-",season,".RData"))
    load(paste0(common,"woi-common.RData"))

    #pieces.1 <- with(grand.data, paste(etype, ev.player.1, distance))
    subresample <- function(dat) {
        if (sum(!is.na(dat$xcoord)) > 1) {
            dat[is.na(dat$xcoord),c("import.ies")] <- 1
            dat[is.na(dat$xcoord),c("xcoord","ycoord")] <-
                dat[sample(which(!is.na(dat$xcoord)), sum(is.na(dat$xcoord)),
                           replace=TRUE), c("xcoord","ycoord")]
        }
        dat[,c("season","gcode","event","import.ies","xcoord","ycoord")] #dat$import.ies==1
    }
    

    ## First cut.
    grand.data$subdistance <- round((grand.data$distance+2.4)/5)
    repurpose <- grand.data %>% filter(etype %in% c("GOAL","SHOT","MISS")) %>% group_by (etype, ev.player.1, subdistance) %>% do (subresample(.))
    grand.data[match(paste0(repurpose$season,repurpose$gcode,repurpose$event),
                     paste0(grand.data$season,grand.data$gcode,grand.data$event)),
               c("import.ies","xcoord","ycoord")] <- repurpose[,c("import.ies","xcoord","ycoord")]
    ## Second cut.
    repurpose <- grand.data %>% filter(etype %in% c("GOAL","SHOT","MISS")) %>% group_by (ev.player.1, type) %>% do (subresample(.))
    grand.data[match(paste0(repurpose$season,repurpose$gcode,repurpose$event),
                     paste0(grand.data$season,grand.data$gcode,grand.data$event)),
               c("import.ies","xcoord","ycoord")] <- repurpose[,c("import.ies","xcoord","ycoord")]
    ## Third cut.
    repurpose <- grand.data %>% filter(etype %in% c("GOAL","SHOT","MISS")) %>% group_by (type) %>% do (subresample(.))
    grand.data[match(paste0(repurpose$season,repurpose$gcode,repurpose$event),
                     paste0(grand.data$season,grand.data$gcode,grand.data$event)),
               c("import.ies","xcoord","ycoord")] <- repurpose[,c("import.ies","xcoord","ycoord")]

    
    repurpose.2 <- grand.data %>% filter(etype %in% c("BLOCK")) %>% group_by (type, ev.player.2) %>% do (subresample(.))
    grand.data[match(paste0(repurpose.2$season,repurpose.2$gcode,repurpose.2$event),
                     paste0(grand.data$season,grand.data$gcode,grand.data$event)),
               c("import.ies","xcoord","ycoord")] <- repurpose.2[,c("import.ies","xcoord","ycoord")]
    repurpose.2 <- grand.data %>% filter(etype %in% c("BLOCK")) %>% group_by (type) %>% do (subresample(.))
    grand.data[match(paste0(repurpose.2$season,repurpose.2$gcode,repurpose.2$event),
                     paste0(grand.data$season,grand.data$gcode,grand.data$event)),
               c("import.ies","xcoord","ycoord")] <- repurpose.2[,c("import.ies","xcoord","ycoord")]

    
    ## distances.
    grand.data$distance[is.na(grand.data$distance)] <- (sqrt((89-abs(grand.data$xcoord))^2 + grand.data$ycoord^2))[is.na(grand.data$distance)]
    save(grand.data, file=paste0(direc, "nhlscrapr-",season,".RData"))

}
