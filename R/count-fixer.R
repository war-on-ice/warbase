
####################################################################################
##
## Compose shot rate adjustments from war-on-ice.com files.
## ACT, 2014-12-27

## Preamble.

#library(lme4)
#library(dplyr)
#library(doMC)
#load("../common-data/woi-common.RData")
#library(RSQLite)

anac <- function(...) as.numeric(as.character(...))

## For a particular season and "game state" (ES,PP, etc) get the Poisson regression correction factors for the rinks and states in question.
## danger: all (""), low ("1") medium ("2") high ("3")

grab.table <- function (ss = "20142015",
                        game.state=1,
                        danger="") {
    ## ss="20142015"; game.state=1; danger="3"
    ##

    team.connect <- src_sqlite("common-data/team.sqlite")
    sql.statement <- paste ('SELECT * FROM team WHERE period > 0 AND period < 4 AND gamestate = 1')  ## period = 0 AND 
    teamtable <- tbl(team.connect, sql(sql.statement)) %>% filter (TOI > 0, score.diff.cat < 7, season == ss) %>% collect
    
    message(ss," ",game.state," ",danger)
    ## teamtable <- rbind_all(lapply(ss, function(season) get(load (paste0("common-data/teamsall-",season,".RData"))))) %>% filter (TOI > 0, gamestate %in% game.state, score.diff.cat %in% 0:6)

    ## since we're doing home and away, "for" alone will do in each case for the lead/trail situation.
    ## decouple blocks, misses and saves from Corsi, Fenwick and Shots.
    
    teamtable$BF <- teamtable[[paste0("CF",danger)]]-teamtable[[paste0("FF",danger)]]
    teamtable$MF <- teamtable[[paste0("FF",danger)]]-teamtable[[paste0("SF",danger)]]
    teamtable$saves <- teamtable[[paste0("SF",danger)]]-teamtable[[paste0("GF",danger)]]
    teamtable$rink <- teamtable$Team
    teamtable$rink[teamtable$home == 0] <- teamtable$Opponent[teamtable$home == 0]


    ## Run the GLMs.
    block.count <- lme4::glmer (BF ~ (1|home) +                   ## 0-1 indicator for home vs away
                          (1|score.diff.cat:period) +       ## score.diff.cat -- 0=trail by 3 or more, 3=tied, 6=lead by 3 or more
                          (1|rink) +                        ## home team factor
                          (1|Team) + (1|Opponent),          ## event ream, opponent team
                          offset=log(TOI),                  ## multiply the event count by the underlying time on ice.  
                          data=teamtable, family=poisson)   
    
    miss.count <- lme4::glmer (MF ~ (1|home) +
                               (1|score.diff.cat:period) +
                               (1|rink) +
                               (1|Team) + (1|Opponent),
                         offset=log(TOI), data=teamtable, family=poisson)   
    shot.count <- lme4::glmer (saves ~ (1|home) +
                               (1|score.diff.cat:period) +
                               (1|rink) +
                               (1|Team) + (1|Opponent),
                         offset=log(TOI), data=teamtable, family=poisson)   

    ## We can do hits too, but unnecessary to this study.
    
    #hit.count <- glmer (HIT ~ (1|home) + (1|score.diff.cat) + (1|rink) + (1|Team) + (1|Opponent),
    #                    offset=log(TOI), data=teamtable, family=poisson)   # + 

    
    output.rinks <- data.frame(season=ss,
                               gamestate=game.state,
                               danger=danger,
                               rink=rownames(ranef(shot.count)$rink),
                               shot.ad=unlist(exp(ranef(shot.count)$rink)),
                               miss.ad=unlist(exp(ranef(miss.count)$rink)),
                               block.ad=unlist(exp(ranef(block.count)$rink))
#                               , hit.ad=unlist(exp(ranef(hit.count)$rink))
                               )

    output.homes <- data.frame(season=ss,
                               gamestate=game.state,
                               danger=danger,
                               homestate=rownames(ranef(shot.count)$home),
                               homeshot=unlist(exp(ranef(shot.count)$home)),
                               homemiss=unlist(exp(ranef(miss.count)$home)),
                               homeblock=unlist(exp(ranef(block.count)$home))
#                               ,homehit=unlist(exp(ranef(hit.count)$home))
                               )

    score.period.bits <- do.call(rbind, strsplit(rownames(ranef(shot.count)$score.diff.cat), ":"))
    output.scores <- data.frame(season=ss,
                                gamestate=game.state,
                                danger=danger,
                                score.diff.cat=score.period.bits[,1], ##rownames(ranef(shot.count)$score.diff.cat),
                                period=score.period.bits[,2],
                                scoreshot=unlist(exp(ranef(shot.count)$score.diff.cat)),
                                scoremiss=unlist(exp(ranef(miss.count)$score.diff.cat)),
                                scoreblock=unlist(exp(ranef(block.count)$score.diff.cat))
#                                , homehit=unlist(exp(ranef(hit.count)$score.diff.cat))
                                )
    
    return(list(output.rinks=output.rinks,
                output.homes=output.homes,
                output.scores=output.scores))
}

## For a given danger state, put together the correction table.

make.all.adjustments <- function (game.state, danger="") {

    ## actual work here.
    all.adjustments <- lapply(seasons[3:12], grab.table, game.state=game.state, danger=danger)
    
    all.adjust <- do.call(rbind, lapply(all.adjustments, function(pp) pp[[1]]))
    all.adjust.home <- do.call(rbind, lapply(all.adjustments, function(pp) pp[[2]]))
    all.adjust.score <- do.call(rbind, lapply(all.adjustments, function(pp) pp[[3]]))


    ## Get the annual correlation for each statistic, over each team. A few will overlap, of course.
    
    all.adjust$lBL <- log(all.adjust$block.ad)
    all.adjust$lMS <- log(all.adjust$miss.ad)
    all.adjust$lSH <- log(all.adjust$shot.ad)
    
    all.adjust$rink <- as.character(all.adjust$rink)
    all.adjust$rink[all.adjust$rink %in% c("PHX","ARI")] <- "PHA"
    all.adjust$rink[all.adjust$rink %in% c("ATL","WPG")] <- "APG"
    all.adjust <- all.adjust[order(all.adjust$season, all.adjust$rink),]

    lBLfac <- cor(all.adjust$lBL[-(1:30)],
                  all.adjust$lBL[-(nrow(all.adjust) - 30 + 1:30)])
    lMSfac <- cor(all.adjust$lMS[-(1:30)],
                  all.adjust$lMS[-(nrow(all.adjust) - 30 + 1:30)])
    lSHfac <- cor(all.adjust$lSH[-(1:30)],
              all.adjust$lSH[-(nrow(all.adjust) - 30 + 1:30)])
    message(game.state, " ", danger, " ", lBLfac, " ", lMSfac, " ", lSHfac)
    ##lHTfac <- cor(all.adjust$lHT[-(1:30)],
    ##              all.adjust$lHT[-(nrow(all.adjust) - 30 + 1:30)])

    all.adjust$BL.a <- exp(all.adjust$lBL * lBLfac)
    all.adjust$MS.a <- exp(all.adjust$lMS * lMSfac)
    all.adjust$SH.a <- exp(all.adjust$lSH * lSHfac)
    ##all.adjust$HT.a <- exp(all.adjust$lHT * lHTfac)

    all.adjust$rink[all.adjust$rink == "PHA" & anac(all.adjust$season) < 20142015] <- "PHX"
    all.adjust$rink[all.adjust$rink == "PHA" & anac(all.adjust$season) == 20142015] <- "ARI"
    all.adjust$rink[all.adjust$rink == "APG" & anac(all.adjust$season) < 20112012] <- "ATL"
    all.adjust$rink[all.adjust$rink == "APG" & anac(all.adjust$season) >= 20112012] <- "WPG"


    ## rebind event count bias to the outcomes for home/away and score/period.
    
    all.adjust <- all.adjust[,c("season","gamestate","danger","rink","BL.a","MS.a","SH.a")]

    all.adjust.2 <- merge(all.adjust, all.adjust.home)
    all.adjust.3 <- merge(all.adjust.2, all.adjust.score)
    
    return(all.adjust.3)
} 


## all.adjust <- make.all.adjustments(1)
