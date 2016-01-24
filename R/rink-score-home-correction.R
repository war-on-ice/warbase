
###################################################################################
##
## Input: getPlayerTable columns. Precalculated rink adjustment factors.
## Output: adjusted playertable.

make.count.adjustments.player <- function (eventtable,
                                           subseason=warbase::seasons,
                                           do.rink=TRUE,
                                           do.home=TRUE,
                                           do.scorestate=TRUE){ ##  rink.adjustments=warbase::all.adjust) {

    season <- subseason[1]

    ## 
    
    ## load ("../common-data/woi-common.RData")
    ## playertable <- rbind_all(lapply(11:length(seasons), function(ss) get(load(paste0("../common-data/aug-player-", seasons[ss], ".RData")))))
    ## teamtable <- get(load(paste0("../../common-data/teams/teamrecord-TOR.RData")))
    ## eventtable=swapnames(playertable); do.rink=TRUE; do.home=TRUE; do.scorestate=TRUE;
    ## rink.adjustments = get(load("../source-data/rink-counts.RData"))
    #message (do.rink, " ", do.home, " ", do.scorestate)
    ## get the hometeam column.
    ##print(head(eventtable,2))
    ## eventtable = playerrun

    eventtable <- mutate(eventtable, rink = Team)
    if (any (names(eventtable) == "Opponent")) {
        eventtable[["rink"]][eventtable$home == 0] <- eventtable[["Opponent"]][eventtable$home == 0]
        can.find.rink <- TRUE
        rink.adjustments <- filter(warbase::all.adjust,
                                   season %in% subseason,
                                   rink %in% unique(eventtable$rink))
    } else {
        can.find.rink <- FALSE
        rink.adjustments <- filter(warbase::all.adjust,
                                   season %in% unique(eventtable$season),
                                   rink == "ANA")
    }

    try({
        rink.adjustments$overfactor <- rep(1, nrow(rink.adjustments))
        if (!do.rink & can.find.rink) {rink.adjustments$BL.a <- 1; rink.adjustments$MS.a <- 1; rink.adjustments$SH.a <- 1}
        if (!do.home) {rink.adjustments$homeblock <- 1; rink.adjustments$homemiss <- 1; rink.adjustments$homeshot <- 1}
        if (!do.scorestate) {rink.adjustments$scoreblock <- 1; rink.adjustments$scoremiss <- 1; rink.adjustments$scoreshot <- 1}
        
        tt.label <- with(eventtable, paste(season, gamestate, score.diff.cat, period, rink, home)) ##danger
        aa.label <- with(rink.adjustments, paste(season, gamestate, score.diff.cat, period, rink, homestate)) ##rink.adjustments$danger,
        
        other.gamestate <- rink.adjustments$gamestate;      other.gamestate[other.gamestate == 2] <- 12
        other.gamestate[other.gamestate == 3] <- 2;         other.gamestate[other.gamestate == 12] <- 3
        
        aa.label.flip <- with(rink.adjustments, paste(season, other.gamestate, 6-anac(score.diff.cat),
                                                      period, rink, 1-anac(homestate)))
        
        if (any (do.rink, do.home, do.scorestate)) for (danger in c("","1","2","3","4")) {   #,"1","2","3"
            
            tt.bits <- paste(tt.label, danger)
            aa.bits <- paste(aa.label, rink.adjustments$danger)
            aa.bits.flip <- paste(aa.label.flip, rink.adjustments$danger)
            
            rowmatcher <- match(tt.bits, aa.bits)
            suppressWarnings(rink.adjustments <- rbind(rink.adjustments, 1))
            rowmatcher[is.na(rowmatcher)] <- nrow(rink.adjustments)
            all.2 <- rink.adjustments[rowmatcher,]
            
            rowmatcher.2 <- match(tt.bits, aa.bits.flip)
            rowmatcher.2[is.na(rowmatcher.2)] <- nrow(rink.adjustments)
            all.3 <- rink.adjustments[rowmatcher.2,]
            
            for (forag in c("F","A")) for (onoff in c("","off")) {
                
                all.1 <- if (forag=="F") all.2 else all.3
                
                eventtable[[paste0("B",forag,danger,onoff)]] <-
                    (eventtable[[paste0("C",forag,danger,onoff)]]-
                     eventtable[[paste0("F",forag,danger,onoff)]])/
                         (all.1$homeblock*all.1$scoreblock*all.1$BL.a)
                
                eventtable[[paste0("M",forag,danger,onoff)]] <-
                    (eventtable[[paste0("F",forag,danger,onoff)]]-
                     eventtable[[paste0("S",forag,danger,onoff)]])/
                         (all.1$homemiss*all.1$scoremiss*all.1$MS.a)
                
                eventtable[[paste0("V",forag,danger,onoff)]] <-
                    (eventtable[[paste0("S",forag,danger,onoff)]]-
                     eventtable[[paste0("G",forag,danger,onoff)]])/
                         (all.1$homeshot*all.1$scoreshot*all.1$SH.a)
                
                eventtable[[paste0("S",forag,danger,onoff)]] <-
                    eventtable[[paste0("G",forag,danger,onoff)]] + eventtable[[paste0("V",forag,danger,onoff)]]
                eventtable[[paste0("F",forag,danger,onoff)]] <-
                    eventtable[[paste0("S",forag,danger,onoff)]] + eventtable[[paste0("M",forag,danger,onoff)]]
                eventtable[[paste0("C",forag,danger,onoff)]] <-
                    eventtable[[paste0("F",forag,danger,onoff)]] + eventtable[[paste0("B",forag,danger,onoff)]]
                
            }
        }
    })

    return(eventtable)

}


## rink bias, home/away balance, score effects.
make.count.adjustments.team <- function (eventtable,
                                         subseason=warbase::seasons,
                                         do.rink=TRUE,
                                         do.home=TRUE,
                                         do.scorestate=TRUE) {
    ## teamtable <- rbind_all(lapply(3:length(seasons), function(ss) get(load(paste0("../common-data/all-team-seasons-", seasons[ss], ".RData")))))
    ## teamtable <- get(load(paste0("../../common-data/teams/teamrecord-TOR.RData")))
    ## eventtable=teamtable; do.rink=TRUE; do.home=TRUE; do.scorestate=TRUE;
    ## rink.adjustments = get(load("../../source-data/rink-counts.RData"))
    #message (do.rink, " ", do.home, " ", do.scorestate)
    ## get the hometeam column.
    #eventtable <- as.data.frame(eventtable)
    
    eventtable <- mutate(eventtable, rink = Team)
    eventtable[["rink"]][eventtable$home == 0] <- eventtable[["Opponent"]][eventtable$home == 0]

    try({

    rink.adjustments <- filter(warbase::all.adjust,
                               season %in% subseason,
                               rink %in% unique(eventtable$rink))

    season <- subseason
    rink.adjustments$overfactor <- rep(1, nrow(rink.adjustments))
    if (!do.rink) {rink.adjustments$BL.a <- 1; rink.adjustments$MS.a <- 1; rink.adjustments$SH.a <- 1}
    if (!do.home) {rink.adjustments$homeblock <- 1; rink.adjustments$homemiss <- 1; rink.adjustments$homeshot <- 1}
    if (!do.scorestate) {rink.adjustments$scoreblock <- 1; rink.adjustments$scoremiss <- 1; rink.adjustments$scoreshot <- 1}

    tt.label <- with(eventtable, paste(season, gamestate, score.diff.cat, period, rink, home)) ##danger
    aa.label <- with(rink.adjustments, paste(season, gamestate, score.diff.cat, period, rink, homestate)) ##rink.adjustments$danger,

    other.gamestate <- rink.adjustments$gamestate;      other.gamestate[other.gamestate == 2] <- 12
    other.gamestate[other.gamestate == 3] <- 2;         other.gamestate[other.gamestate == 12] <- 3
    
    aa.label.flip <- with(rink.adjustments, paste(season, other.gamestate, 6-anac(score.diff.cat),
                                                  period, rink, 1-anac(homestate)))
    
    for (danger in c("","1","2","3")) {

        tt.bits <- paste(tt.label, danger)
        aa.bits <- paste(aa.label, rink.adjustments$danger)
        aa.bits.flip <- paste(aa.label.flip, rink.adjustments$danger)
        
        rowmatcher <- match(tt.bits, aa.bits)
        suppressWarnings(rink.adjustments <- rbind(rink.adjustments, 1))
        rowmatcher[is.na(rowmatcher)] <- nrow(rink.adjustments)
        all.2 <- rink.adjustments[rowmatcher,]

        rowmatcher.2 <- match(tt.bits, aa.bits.flip)
#        suppressWarnings(rink.adjustments <- rbind(rink.adjustments, 1))
        rowmatcher.2[is.na(rowmatcher.2)] <- nrow(rink.adjustments)
        all.3 <- rink.adjustments[rowmatcher.2,]

        for (forag in c("F","A")) {

            all.1 <- if (forag=="F") all.2 else all.3
            
            eventtable[[paste0("B",forag,danger)]] <-
                (eventtable[[paste0("C",forag,danger)]]-
                 eventtable[[paste0("F",forag,danger)]])/
                     (all.1$homeblock*all.1$scoreblock*all.1$BL.a)
                    
            eventtable[[paste0("M",forag,danger)]] <-
                (eventtable[[paste0("F",forag,danger)]]-
                 eventtable[[paste0("S",forag,danger)]])/
                     (all.1$homemiss*all.1$scoremiss*all.1$MS.a)
        
            eventtable[[paste0("V",forag,danger)]] <-
                (eventtable[[paste0("S",forag,danger)]]-
                 eventtable[[paste0("G",forag,danger)]])/
                     (all.1$homeshot*all.1$scoreshot*all.1$SH.a)
            
            eventtable[[paste0("S",forag,danger)]] <-
                eventtable[[paste0("G",forag,danger)]] + eventtable[[paste0("V",forag,danger)]]
            eventtable[[paste0("F",forag,danger)]] <-
                eventtable[[paste0("S",forag,danger)]] + eventtable[[paste0("M",forag,danger)]]
            eventtable[[paste0("C",forag,danger)]] <-
                eventtable[[paste0("F",forag,danger)]] + eventtable[[paste0("B",forag,danger)]]

        }
    }
})
    return(eventtable)

}

