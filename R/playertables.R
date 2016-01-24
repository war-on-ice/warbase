# Sam Ventura
# Tables for war-on-ice.com
# 20 August 2014

# the functions below all assume that nhlscrapr-probs.RData 
# has been loaded into the workspace

# reverse parameterization of is.element() for fast lookup with apply()

# getPlayerTable
# Author:  Sam Ventura
# Date:  20 August 2014
# Description:  gets player data for a specified subset of grand.data
# sub:  subset of grand.data (all columns) for a given period, game, timeframe, season, etc
# id:  the player ID

## Revisions: acthomas, 2014-11-25 -- dplyr in one cut.

##library(dplyr)

#scoring.chance <- function (pbp.data)
#    (pbp.data$etype %in% c("GOAL","SHOT","MISS","BLOCK")) &
#    (pbp.data$shot.feature %in% higher.prob.events |
#     (pbp.data$etype %in% c("GOAL","SHOT","MISS") & pbp.data$danger.zone == 2) |
#     pbp.data$danger.zone == 3)
    


## Extra compression of the existing tables. Used to get composite scoring modes.
collapse.all.base <- function (sub.p)   #, score.diff.cat.temp=7
##    group_by (sub.p, ID, gamestate, period, home, Team, Opponent) %>%
    summarize (sub.p,## score.diff.cat=score.diff.cat.temp,
               
               TOI=sum(TOI), ZSO=sum(ZSO), ZSN=sum(ZSN), ZSD=sum(ZSD),
               GF=sum(GF),               GA=sum(GA),               SF=sum(SF),               SA=sum(SA),
               FF=sum(FF),               FA=sum(FA),               CF=sum(CF),               CA=sum(CA),
               OZF=sum(OZF),               OZA=sum(OZA),               

               GF1=sum(GF1),               GA1=sum(GA1),               SF1=sum(SF1),               SA1=sum(SA1),
               FF1=sum(FF1),               FA1=sum(FA1),               CF1=sum(CF1),               CA1=sum(CA1),
               GF2=sum(GF2),               GA2=sum(GA2),               SF2=sum(SF2),               SA2=sum(SA2),
               FF2=sum(FF2),               FA2=sum(FA2),               CF2=sum(CF2),               CA2=sum(CA2),
               GF3=sum(GF3),               GA3=sum(GA3),               SF3=sum(SF3),               SA3=sum(SA3),
               FF3=sum(FF3),               FA3=sum(FA3),               CF3=sum(CF3),               CA3=sum(CA3),
               GF4=sum(GF4),               GA4=sum(GA4),               SF4=sum(SF4),               SA4=sum(SA4),
               FF4=sum(FF4),               FA4=sum(FA4),               CF4=sum(CF4),               CA4=sum(CA4),

               SCF=sum(SCF),               SCA=sum(SCA),               sSCF=sum(sSCF),             sSCA=sum(sSCA),
               ExpGF=sum(ExpGF),           ExpGA=sum(ExpGA),

               TOIoff=sum(TOIoff),   ZSOoff=sum(ZSOoff),      ZSNoff=sum(ZSNoff),   ZSDoff=sum(ZSDoff),
               GFoff=sum(GFoff),     GAoff=sum(GAoff),        SFoff=sum(SFoff),     SAoff=sum(SAoff),
               FFoff=sum(FFoff),     FAoff=sum(FAoff),        CFoff=sum(CFoff),     CAoff=sum(CAoff),
               OZFoff=sum(OZFoff),   OZAoff=sum(OZAoff),
               
               GF1off=sum(GF1off),   GA1off=sum(GA1off),      SF1off=sum(SF1off),   SA1off=sum(SA1off),
               FF1off=sum(FF1off),   FA1off=sum(FA1off),      CF1off=sum(CF1off),   CA1off=sum(CA1off),

               GF2off=sum(GF2off),   GA2off=sum(GA2off),      SF2off=sum(SF2off),   SA2off=sum(SA2off),
               FF2off=sum(FF2off),   FA2off=sum(FA2off),      CF2off=sum(CF2off),   CA2off=sum(CA2off),

               GF3off=sum(GF3off),   GA3off=sum(GA3off),      SF3off=sum(SF3off),   SA3off=sum(SA3off),
               FF3off=sum(FF3off),   FA3off=sum(FA3off),      CF3off=sum(CF3off),   CA3off=sum(CA3off),

               GF4off=sum(GF4off),   GA4off=sum(GA4off),      SF4off=sum(SF4off),   SA4off=sum(SA4off),
               FF4off=sum(FF4off),   FA4off=sum(FA4off),      CF4off=sum(CF4off),   CA4off=sum(CA4off),

               SCFoff=sum(SCFoff),   SCAoff=sum(SCAoff),      sSCFoff=sum(sSCFoff),   sSCAoff=sum(sSCAoff),
               ExpGFoff=sum(ExpGFoff),           ExpGAoff=sum(ExpGAoff),
           
               GOAL=sum(GOAL),       SHOT=sum(SHOT),    MISS=sum(MISS),    BLOCKED_SHOT=sum(BLOCKED_SHOT),
               GOAL1=sum(GOAL1),     SHOT1=sum(SHOT1),  MISS1=sum(MISS1),  BLOCKED_SHOT1=sum(BLOCKED_SHOT1),
               GOAL2=sum(GOAL2),     SHOT2=sum(SHOT2),  MISS2=sum(MISS2),  BLOCKED_SHOT2=sum(BLOCKED_SHOT2),
               GOAL3=sum(GOAL3),     SHOT3=sum(SHOT3),  MISS3=sum(MISS3),  BLOCKED_SHOT3=sum(BLOCKED_SHOT3),
               GOAL4=sum(GOAL4),     SHOT4=sum(SHOT4),  MISS4=sum(MISS4),  BLOCKED_SHOT4=sum(BLOCKED_SHOT4),
               
               iSC=sum(iSC),                      isSC=sum(isSC),       iExpG=sum(iExpG),
                   
               HIT=sum(HIT),                 PENL_TAKEN=sum(PENL_TAKEN),               TAKE=sum(TAKE),
               GIVE=sum(GIVE),               FAC_WIN=sum( FAC_WIN),               BLOCK =sum(BLOCK),
               ASSIST =sum(ASSIST),          HIT_TAKEN =sum(HIT_TAKEN),
               PENL_DRAWN=sum(PENL_DRAWN),   FAC_LOSE=sum(FAC_LOSE),     ASSIST_2=sum(ASSIST_2)
               )

collapse.all.scdiff <- function (sub.p) group_by (sub.p, ID, gamestate, period, home, Team, Opponent) %>% collapse.all.base
collapse.all.gmst <- function (sub.p) group_by (sub.p, ID, score.diff.cat, period, home, Team, Opponent) %>% collapse.all.base
collapse.all.period <- function (sub.p) group_by (sub.p, ID, gamestate, score.diff.cat, home, Team, Opponent) %>% collapse.all.base





gpt.reduced <- function(sub.p, home=1) 
    summarize(sub.p,
              home=home,
              Team=if (home==1) hometeam[1] else awayteam[1],
              Opponent=if (home==0) hometeam[1] else awayteam[1],
              
              TOI=sum(event.length),
              ZSO=sum(etype == "FAC" & ((homezone == "Off" & home==1) | (homezone == "Def" & home==0))),
              ZSN=sum(etype == "FAC" & homezone == "Neu"),
              ZSD=sum(etype == "FAC" & ((homezone == "Off" & home==0) | (homezone == "Def" & home==1))),
              
              GF=sum(etype == "GOAL" & ev.team == Team),    GA=sum(etype == "GOAL" & ev.team != Team),
              SF=GF+sum(etype == "SHOT" & ev.team == Team), SA=GA+sum(etype == "SHOT" & ev.team != Team),
              FF=SF+sum(etype == "MISS" & ev.team == Team), FA=SA+sum(etype == "MISS" & ev.team != Team),
              CF=FF+sum(etype == "BLOCK" & ev.team == Team),CA=FA+sum(etype == "BLOCK" & ev.team != Team),

              OZF=CF+sum(etype %in% c("HIT","GIVE","TAKE") & ((homezone == "Off" & home==1) |
                  (homezone == "Def" & home==0))),
              OZA=CA+sum(etype %in% c("HIT","GIVE","TAKE") & ((homezone == "Off" & home==0) |
                  (homezone == "Def" & home==1))),
              
              GF1=sum(etype == "GOAL" & ev.team == Team & danger.zone == 1),
              GA1=sum(etype == "GOAL" & ev.team != Team & danger.zone == 1),
              SF1=GF1+sum(etype == "SHOT" & ev.team == Team & danger.zone == 1),
              SA1=GA1+sum(etype == "SHOT" & ev.team != Team & danger.zone == 1),
              FF1=SF1+sum(etype == "MISS" & ev.team == Team & danger.zone == 1),
              FA1=SA1+sum(etype == "MISS" & ev.team != Team & danger.zone == 1),
              CF1=FF1+sum(etype == "BLOCK" & ev.team == Team & danger.zone == 1),
              CA1=FA1+sum(etype == "BLOCK" & ev.team != Team & danger.zone == 1),
              
              GF2=sum(etype == "GOAL" & ev.team == Team & danger.zone == 2),
              GA2=sum(etype == "GOAL" & ev.team != Team & danger.zone == 2),
              SF2=GF2+sum(etype == "SHOT" & ev.team == Team & danger.zone == 2),
              SA2=GA2+sum(etype == "SHOT" & ev.team != Team & danger.zone == 2),
              FF2=SF2+sum(etype == "MISS" & ev.team == Team & danger.zone == 2),
              FA2=SA2+sum(etype == "MISS" & ev.team != Team & danger.zone == 2),
              CF2=FF2+sum(etype == "BLOCK" & ev.team == Team & danger.zone == 2),
              CA2=FA2+sum(etype == "BLOCK" & ev.team != Team & danger.zone == 2),
              
              GF3=sum(etype == "GOAL" & ev.team == Team & danger.zone == 3),
              GA3=sum(etype == "GOAL" & ev.team != Team & danger.zone == 3),
              SF3=GF3+sum(etype == "SHOT" & ev.team == Team & danger.zone == 3),
              SA3=GA3+sum(etype == "SHOT" & ev.team != Team & danger.zone == 3),
              FF3=SF3+sum(etype == "MISS" & ev.team == Team & danger.zone == 3),
              FA3=SA3+sum(etype == "MISS" & ev.team != Team & danger.zone == 3),
              CF3=FF3+sum(etype == "BLOCK" & ev.team == Team & danger.zone == 3),
              CA3=FA3+sum(etype == "BLOCK" & ev.team != Team & danger.zone == 3),

              GF4=sum(etype == "GOAL" & ev.team == Team & danger.zone == 4),
              GA4=sum(etype == "GOAL" & ev.team != Team & danger.zone == 4),
              SF4=GF4+sum(etype == "SHOT" & ev.team == Team & danger.zone == 4),
              SA4=GA4+sum(etype == "SHOT" & ev.team != Team & danger.zone == 4),
              FF4=SF4+sum(etype == "MISS" & ev.team == Team & danger.zone == 4),
              FA4=SA4+sum(etype == "MISS" & ev.team != Team & danger.zone == 4),
              CF4=FF4+sum(etype == "BLOCK" & ev.team == Team & danger.zone == 4),
              CA4=FA4+sum(etype == "BLOCK" & ev.team != Team & danger.zone == 4),

              SCF=sum(FF2+ CF3+ CF4),
              SCA=sum(FA2+ CA3+ CA4),
              sSCF=sum(FF3+ CF4),
              sSCA=sum(FA3+ CA4),

              ExpGF=sum(expG[ev.team == Team]),
              ExpGA=sum(expG[ev.team != Team])

##              SCF=sum(scoring.chance & ev.team == Team),    SCA=sum(scoring.chance & ev.team != Team),
              
              )

gpt.events.one <- function(sub.p)
    summarize(sub.p,
              GOAL=sum(etype == "GOAL"),     SHOT=sum(etype == "SHOT"),
              MISS=sum(etype == "MISS"),     BLOCKED_SHOT=sum(etype == "BLOCK"),
              
             
              GOAL1=sum(etype == "GOAL" & danger.zone == 1),
              SHOT1=sum(etype == "SHOT" & danger.zone == 1),
              MISS1=sum(etype == "MISS" & danger.zone == 1),
              BLOCKED_SHOT1=sum(etype == "BLOCK" & danger.zone == 1),
              
              GOAL2=sum(etype == "GOAL" & danger.zone == 2),
              SHOT2=sum(etype == "SHOT" & danger.zone == 2),
              MISS2=sum(etype == "MISS" & danger.zone == 2),
              BLOCKED_SHOT2=sum(etype == "BLOCK" & danger.zone == 2),
              
              GOAL3=sum(etype == "GOAL" & danger.zone == 3),
              SHOT3=sum(etype == "SHOT" & danger.zone == 3),
              MISS3=sum(etype == "MISS" & danger.zone == 3),
              BLOCKED_SHOT3=sum(etype == "BLOCK" & danger.zone == 3),

              GOAL4=sum(etype == "GOAL" & danger.zone == 4),
              SHOT4=sum(etype == "SHOT" & danger.zone == 4),
              MISS4=sum(etype == "MISS" & danger.zone == 4),
              BLOCKED_SHOT4=sum(etype == "BLOCK" & danger.zone == 4),

              iSC=sum(GOAL2+SHOT2+MISS2 +
                  GOAL3+SHOT3+MISS3+BLOCKED_SHOT3 +
                  GOAL4+SHOT4+MISS4+BLOCKED_SHOT4),
              isSC=sum(GOAL3+SHOT3+MISS3 +
                  GOAL4+SHOT4+MISS4+BLOCKED_SHOT4),
              iExpG=sum(expG),

              HIT=sum(etype == "HIT"),       PENL_TAKEN=sum(etype == "PENL"),
              TAKE=sum(etype == "TAKE"),     GIVE=sum(etype == "GIVE")
              ,FAC_WIN=sum(etype == "FAC")   ## This is verified now -- faceoff winners come first.
              )

gpt.events.two <- function(sub.p)
    summarize(sub.p,
              BLOCK=sum(etype == "BLOCK"),   ASSIST=sum(etype == "GOAL"),
              HIT_TAKEN=sum(etype == "HIT"), PENL_DRAWN=sum(etype == "PENL")
              ,      FAC_LOSE=sum(etype == "FAC"))

gpt.events.three <- function(sub.p)
    summarize(sub.p, ASSIST_2=sum(etype == "GOAL"))

augment.with.off <- function (dfsub, gridpieces, grand.events) {
    ## dfsub=home.events.reduced; gridpieces=full.set; grand.events=home.events
    
    missing.ones <- which(is.na(match(apply(gridpieces,1,paste,collapse=""),
                                      apply(dfsub[,1:4],1,paste,collapse=""))))
    
    if (length(missing.ones) > 0) {
        orig <- nrow(dfsub)
        stuffer <- as.data.frame(matrix (0, ncol=ncol(dfsub), nrow=length(missing.ones)))
        colnames(stuffer) <- colnames(dfsub)
        stuffer[,1:4] <- gridpieces[missing.ones,]
        for (cc in 5:7) stuffer[,cc] <- dfsub[1,cc]
        dfsub <- rbind(dfsub, stuffer)
    }

    just.grand <- grand.events[match(apply(dfsub[,2:4],1,paste,collapse=""),
                               apply(grand.events[,1:3],1,paste,collapse="")), -(1:6)]
    events.off <- matrix(as.numeric(unlist(just.grand)) - as.numeric(unlist(dfsub[,-(1:7)])),
                         nrow=nrow(just.grand))

    colnames(events.off) <- paste0(colnames(dfsub)[-(1:7)], "off")
    dfsub <- cbind(dfsub, events.off)
    return(dfsub)
}

colSums.noID <- function(mydf, omits=1:7) 
    merge(data.frame(#period=unlist(mydf[1,4]),
                     home=unlist(mydf[1,5]),
                     Team=unlist(mydf[1,6]),
                     Opponent=unlist(mydf[1,7]),
                     stringsAsFactors=FALSE),
          data.frame(rbind(colSums(mydf[,-omits]))))
##    as.data.frame(rbind(c(unlist(mydf[1,4]), unlist(mydf[1,5]), )))



## score.diff.cat, gamestate
getPlayerTable.all <- function (subpiece, verbose = FALSE) { #, home.ids, away.ids
    ## load ("source-data/nhlscrapr-20142015.RData"); subpiece=subset(grand.data, gcode==20001) 
    ## subpiece <- subset(game.pbp, score.diff.cat==3 & gamestate==2)
    ## homecols <- c("h1","h2","h3","h4","h5","h6","home.G"); awaycols <- c("a1","a2","a3","a4","a5","a6","away.G" 
    ##print(c(subpiece$score.diff.cat[1],subpiece$gamestate[1]))
    ## subpiece=game.pbp
    ##danger.zone <- c(0, 1,1,1, 1,2,2,2,1, 1,2,3,3,2,1, 1,1)
    subpiece$danger.zone <- danger.zone[subpiece$new.loc.section + 1] + 1*(subpiece$shot.feature %in% higher.prob.events)

    #subpiece$danger.zone <- danger.zone[subpiece$new.loc.section + 1]
    #subpiece$scoring.chance <- scoring.chance(subpiece)
    
    home.events <- {s1 <- subpiece %>% group_by (score.diff.cat, gamestate, period) %>% gpt.reduced(home=1);
                    s1} #colnames(s1)[1] <- "ID"; 
    
    home.events.full <-
        rbind_list({s1 <- subpiece %>% group_by (h1, score.diff.cat, gamestate, period) %>% gpt.reduced(home=1);
                    colnames(s1)[1] <- "ID"; s1},
                   {s1 <- subpiece %>% group_by (h2, score.diff.cat, gamestate, period) %>% gpt.reduced(home=1);
                    colnames(s1)[1] <- "ID"; s1},
                   {s1 <- subpiece %>% group_by (h3, score.diff.cat, gamestate, period) %>% gpt.reduced(home=1);
                    colnames(s1)[1] <- "ID"; s1},
                   {s1 <- subpiece %>% group_by (h4, score.diff.cat, gamestate, period) %>% gpt.reduced(home=1);
                    colnames(s1)[1] <- "ID"; s1},
                   {s1 <- subpiece %>% group_by (h5, score.diff.cat, gamestate, period) %>% gpt.reduced(home=1);
                    colnames(s1)[1] <- "ID"; s1},
                   {s1 <- subpiece %>% group_by (h6, score.diff.cat, gamestate, period) %>% gpt.reduced(home=1);
                    colnames(s1)[1] <- "ID"; s1},
                   {s1 <- subpiece %>% group_by (home.G, score.diff.cat, gamestate, period) %>% gpt.reduced(home=1);
                    colnames(s1)[1] <- "ID"; s1})
    
    home.events.reduced <-
        subset(home.events.full %>% group_by(ID, score.diff.cat, gamestate, period) %>% do(colSums.noID(.)), ID > 1)
    #for (cc in (1:ncol(home.events.reduced))[-(5:6)])
    #    home.events.reduced[,cc] <- as.numeric(home.events.reduced[,cc])

    home.ids <- unique(home.events.reduced$ID)
    
    stateset <- as.data.frame(unique(cbind(home.events$score.diff.cat, home.events$gamestate, home.events$period)))
    colnames(stateset) <- c("score.diff.cat", "gamestate", "period")
    f1 <- function(tt) stateset
    full.set <- as.data.frame(cbind(home.ids)) %>% group_by(home.ids) %>% do(f1(.))

    home.full <- augment.with.off (home.events.reduced, full.set, home.events) 


    ###########################################################################
    away.events <- {s1 <- subpiece %>% group_by (score.diff.cat, gamestate, period) %>% gpt.reduced(home=0);
                    s1} #colnames(s1)[1] <- "ID"; 
    
    away.events.full <-
        rbind_list({s1 <- subpiece %>% group_by (a1, score.diff.cat, gamestate, period) %>% gpt.reduced(home=0);
               colnames(s1)[1] <- "ID"; s1},
              {s1 <- subpiece %>% group_by (a2, score.diff.cat, gamestate, period) %>% gpt.reduced(home=0);
               colnames(s1)[1] <- "ID"; s1},
              {s1 <- subpiece %>% group_by (a3, score.diff.cat, gamestate, period) %>% gpt.reduced(home=0);
               colnames(s1)[1] <- "ID"; s1},
              {s1 <- subpiece %>% group_by (a4, score.diff.cat, gamestate, period) %>% gpt.reduced(home=0);
               colnames(s1)[1] <- "ID"; s1},
              {s1 <- subpiece %>% group_by (a5, score.diff.cat, gamestate, period) %>% gpt.reduced(home=0);
               colnames(s1)[1] <- "ID"; s1},
              {s1 <- subpiece %>% group_by (a6, score.diff.cat, gamestate, period) %>% gpt.reduced(home=0);
               colnames(s1)[1] <- "ID"; s1},
              {s1 <- subpiece %>% group_by (away.G, score.diff.cat, gamestate, period) %>% gpt.reduced(home=0);
               colnames(s1)[1] <- "ID"; s1})
    
    away.events.reduced <-
        as.data.frame(subset(away.events.full %>% group_by(ID, score.diff.cat, gamestate, period) %>%
                             do(colSums.noID(.)), ID > 1))
    away.ids <- unique(away.events.reduced$ID)

    stateset <- as.data.frame(unique(cbind(away.events$score.diff.cat, away.events$gamestate, home.events$period)))
    colnames(stateset) <- c("score.diff.cat", "gamestate", "period")
    f1 <- function(tt) stateset
    full.set <- as.data.frame(cbind(away.ids)) %>% group_by(away.ids) %>% do(f1(.))

    away.full <- augment.with.off (away.events.reduced, full.set, away.events) 
    
    
    all.events <- rbind(home.full, away.full)
    prime.events <- subpiece %>% group_by(ev.player.1, score.diff.cat, gamestate, period) %>% gpt.events.one; colnames(prime.events)[1] <- "ID"
    sec.events <- subpiece %>% group_by(ev.player.2, score.diff.cat, gamestate, period) %>% gpt.events.two; colnames(sec.events)[1] <- "ID"
    ter.events <- subpiece %>% group_by(ev.player.3, score.diff.cat, gamestate, period) %>% gpt.events.three; colnames(ter.events)[1] <- "ID"

    all.events <- merge(merge(merge(all.events, prime.events, c("ID", "score.diff.cat", "gamestate", "period"), all.x=TRUE),
                              sec.events, c("ID", "score.diff.cat", "gamestate", "period"), all.x=TRUE),
                        ter.events, c("ID", "score.diff.cat", "gamestate", "period"), all.x=TRUE)
    all.events[is.na(all.events)] <- 0

    ##print(dim(all.events))
    ##print(colnames(all.events))
    ##return(all.events)

    output <- list(playerrun=all.events, teamrun=rbind(home.events, away.events))
    return(output)
}


getSingleGameTable <- function(game.pbp, roster.this=roster.unique){
  # make sure input is okay
  ## games.row = NULL; season = "20142015"; gcode = "20051"; player.ids = NULL
    ## load (url("http://war-on-ice.com/data/games/2015201620203.RData")); game.pbp=playbyplay
    ## load ("source-data/nhlscrapr-20142015.RData"); game.pbp=subset(grand.data, gcode=="20001")
    ## game.pbp=playbyplay

    this.season <- game.pbp$season[1]
    season <- game.pbp$season[1]
    gcode <- game.pbp$gcode[1]
    message("Player ",game.pbp$season[1], game.pbp$gcode[1])
    game.pbp$event.length[game.pbp$event.length == 0.5] <- 0
    
    homecols <- c("h1","h2","h3","h4","h5","h6","home.G")
    awaycols <- c("a1","a2","a3","a4","a5","a6","away.G")
    skatercols <- c(homecols, awaycols)
    home.ids <- sort(unique(unlist(game.pbp[,homecols])))[-1]
    away.ids <- sort(unique(unlist(game.pbp[,awaycols])))[-1]
    
    ## Screw the shootout!
    if (substr(game.pbp$gcode[1],1,1) == "2") game.pbp <- game.pbp[game.pbp$period < 5,]   #home.skaters > 2 & away.skaters > 2)
    #game.pbp$score.diff.cat <- with(game.pbp,  ## Just upped to 3 goal splits.
    #                                0*(home.score - away.score <= -3) +
    #                                1*(home.score - away.score == -2) +
    #                                2*(home.score - away.score == -1) +
    #                                3*(home.score - away.score == 0) +
    #                                4*(home.score - away.score == 1) +
    #                                5*(home.score - away.score == 2) +
    #                                6*(home.score - away.score >= 3))
    
    ## there was an error here that @omgitsdomi spotted. 9-7-14
    ## close <- (abs(game.pbp$home.score-game.pbp$away.score) == 1 & game.pbp$seconds <= 2400)
      
    game.pbp$gamestate <- with(game.pbp,
                               1*(home.skaters == 6 & away.skaters == 6 & home.G > 1 & away.G > 1) +
                               2*(home.skaters > away.skaters & away.skaters > 2 & home.G > 1 & away.G > 1) +
                               3*(home.skaters < away.skaters & home.skaters > 2 & home.G > 1 & away.G > 1) +
                               4*(home.skaters == 5 & away.skaters == 5 & home.G > 1 & away.G > 1) +
                               5*(home.skaters > 2 & away.skaters > 2 & home.G > 1 & away.G == 1) +
                                        # the team with their goalie in
                               6*(home.skaters > 2 & away.skaters > 2 & home.G == 1 & away.G > 1) +
                               9*(home.skaters == 4 & away.skaters == 4 & home.G > 1 & away.G > 1))
                                        # the team with their goalie pulled

    game.pbp$danger.zone <- danger.zone[game.pbp$new.loc.section + 1] +
        1*(game.pbp$shot.feature %in% higher.prob.events)
    game.pbp$homeevent <- 1*(game.pbp$ev.team == game.pbp$hometeam)
    game.pbp <- left_join(game.pbp, shot.results %>% mutate (season = anac(season)))
    game.pbp$expG[!(game.pbp$etype %in% c("SHOT","GOAL"))] <- 0
    
    ## Screen for penalties.
    game.pbp <- subset (game.pbp, !grepl("Misconduct", game.pbp$type))
    
    ## Remove matching penalties.
    penbits.1 <- with (game.pbp, paste (etype, type, seconds, ev.player.1, ev.player.2))
    penbits.2 <- with (game.pbp, paste (etype, type, seconds, ev.player.2, ev.player.1))

    matches.1 <- !is.na(match (penbits.1, penbits.2)) & game.pbp$etype == "PENL" & !is.na(game.pbp$ev.player.1)
    matches.2 <- !is.na(match (penbits.2, penbits.1)) & game.pbp$etype == "PENL" & !is.na(game.pbp$ev.player.1)

    game.pbp <- subset(game.pbp, !matches.1 & !matches.2)
    
    playerteam <- getPlayerTable.all (game.pbp)

    ## colnames(player.table)[colnames(player.table) == "V1"] <- "ID"
    
    player.table <- augmentflip(playerteam$playerrun)
    
    ## 0:6, 0:6, 2:4, 2:4,  4:6, 5:6, 0:1, 0:2
    ## Create new states right now. 
    pt.new <- rbind_list(player.table %>% collapse.all.scdiff %>% mutate(score.diff.cat=7),  ## all
                         player.table %>% make.count.adjustments.player(subseason=this.season) %>%
                           collapse.all.scdiff %>% mutate(score.diff.cat=8),     ## score-adjusted
                         player.table %>% filter (score.diff.cat %in% 2:4) %>%
                           collapse.all.scdiff %>% mutate(score.diff.cat=9),  ## within 1
                         player.table %>% filter ((score.diff.cat %in% 2:4 & period %in% 1:2) | score.diff.cat == 3) %>%
                           collapse.all.scdiff %>% mutate(score.diff.cat=10), ## close
                         
                         player.table %>% filter (score.diff.cat %in% 4:6) %>%
                           collapse.all.scdiff %>% mutate(score.diff.cat=11), ## leading 
                         player.table %>% filter (score.diff.cat %in% 5:6) %>%
                           collapse.all.scdiff %>% mutate(score.diff.cat=12), ## leading 2+
                         player.table %>% filter (score.diff.cat %in% 0:1) %>%
                           collapse.all.scdiff %>% mutate(score.diff.cat=13), ## trailing 2+
                         player.table %>% filter (score.diff.cat %in% 0:2) %>%
                           collapse.all.scdiff %>% mutate(score.diff.cat=14)) ## trailing
                                      
    player.table <- rbind(player.table, pt.new)

    pt.all <- player.table %>% collapse.all.gmst %>% mutate(gamestate=7)
    player.table <- rbind(player.table, pt.all)

    pt.all.2 <- player.table %>% collapse.all.period %>% mutate (period=0)
    player.table <- rbind(player.table, pt.all.2)

    player.table <- mutate (player.table,
                            season=season, gcode=gcode,
                            tCF60=NA, tCA60=NA, tFF60=NA, tFA60=NA, tTOI60=NA,
                            cCF60=NA, cCA60=NA, cFF60=NA, cFA60=NA, cTOI60=NA)
        
    #player.table$season <- season; player.table$gcode <- gcode
    #player.table$seasonCpct <- player.table$seasonFpct <- player.table$seasonTOIpct <-
        #player.table$CorsiT <- player.table$CorsiC <- player.table$FenwickT <-
            #player.table$FenwickC <- player.table$TOIPctT <- player.table$TOIPctC <- NA

    player.table$ID <- roster.this$woi.id[player.table$ID]
    
    return(list(player.table=player.table))  #, team.table=team.table
}






redo.table.scoreadjusted <- function (player.table) {
    ## load ("common-data/games/2014201520001.RData"); player.table <- playerrun
    
    pt.new <- player.table %>% filter (score.diff.cat < 7) %>% make.count.adjustments.player(subseason=player.table$season[1]) %>%
        collapse.all.scdiff %>% mutate(score.diff.cat = 8)

    pt.new$season <- player.table$season[1]; pt.new$gcode <- player.table$gcode[1]
    pt.new$seasonCpct <- pt.new$seasonFpct <- pt.new$seasonTOIpct <-
        pt.new$CorsiT <- pt.new$CorsiC <- pt.new$FenwickT <-
            pt.new$FenwickC <- pt.new$TOIPctT <- pt.new$TOIPctC <- NA

    
    rbind (player.table %>% filter (score.diff.cat != 8),
           pt.new)
    
}




####################################################################################################


get.player.time.onesub <- function (subc, roster.this=roster.unique) {
    #subc <- sub[close,]
    hpc <- c("h1","h2","h3","h4","h5","h6")
    apc <- c("a1","a2","a3","a4","a5","a6")
    pl1 <- as.matrix(subc[,c(hpc,apc)])
    
    home.players <- unique(c(pl1[,1:6])); home.players <- home.players[as.numeric(home.players)>1]
    away.players <- unique(c(pl1[,6+1:6])); away.players <- away.players[as.numeric(away.players)>1]
    out <- array(0, rep(length(c(home.players, away.players)),2))
    hom.pl <- length(home.players)
    awa.pl <- length(away.players)
    
    for (rr in 1:nrow(subc)) {
        rw <- match(pl1[rr,c(hpc,apc)], c(home.players,away.players)); rw <- rw[!is.na(rw)]
        out[rw,rw] <- out[rw,rw] + subc$event.length[rr]
    }
    
    teammates <- opponents <- teamtime <- opptime <- array(0, c(hom.pl+awa.pl,20))
    colnames (teammates) <- paste0("tm",1:20)
    colnames (opponents) <- paste0("op",1:20)
    colnames (teamtime) <- paste0("tt",1:20)
    colnames (opptime) <- paste0("ot",1:20)
    
    teammates[1:hom.pl, 1:hom.pl] <- matrix(home.players, nrow=hom.pl, ncol=hom.pl, byrow=TRUE)
    teamtime[1:hom.pl, 1:hom.pl] <- out[1:hom.pl, 1:hom.pl]
    teammates[hom.pl + 1:awa.pl, 1:awa.pl] <- matrix(away.players, nrow=awa.pl, ncol=awa.pl, byrow=TRUE)
    teamtime[hom.pl + 1:awa.pl, 1:awa.pl] <- out[hom.pl + 1:awa.pl, hom.pl + 1:awa.pl]

    opponents[1:hom.pl, 1:awa.pl] <- matrix(away.players, nrow=hom.pl, ncol=awa.pl, byrow=TRUE)
    opptime[1:hom.pl, 1:awa.pl] <- out[1:hom.pl, hom.pl+1:awa.pl]
    opponents[hom.pl + 1:awa.pl, 1:hom.pl] <- matrix(home.players, nrow=awa.pl, ncol=hom.pl, byrow=TRUE)
    opptime[hom.pl + 1:awa.pl, 1:hom.pl] <- out[hom.pl + 1:awa.pl, 1:hom.pl]

    ID <- roster.this$woi.id[c(home.players, away.players)]
    teammates <- array(roster.this$woi.id[teammates], dim(teammates))
    opponents <- array(roster.this$woi.id[opponents], dim(opponents))
        
    return(data.frame(ID=ID, season=subc$season[1], gcode=subc$gcode[1],
                 teammates, teamtime, opponents, opptime))

}

get.player.time.one <- function (sub, roster.this=roster.unique) {
    #sub=subset(grand.data, gcode=="20098")
    season <- sub$season[1]
    gcode <- sub$gcode[1]
    
    sub <- subset(sub, home.skaters==6 & away.skaters==6 & home.G > 1 & away.G > 1)
        
    #close <- (abs(sub$home.score - sub$away.score) == 1 & sub$seconds < 2400) |
    #    (sub$home.score == sub$away.score)
  
    ## calculate all data for each player
    time.table <- try({get.player.time.onesub (sub)}, TRUE)
    if (class(time.table) == "try-error") time.table <- data.frame()

##    time.table.close <- NULL
##    if (sum(close)>0) try({
 ##       time.table.close <- get.player.time.onesub (sub[close,])
 ##       time.table.close$close <- TRUE
 ##   }, TRUE)

 ##   time.table.far <- NULL
 ##   if (sum(!close)>0) try({
 ##       time.table.far <- get.player.time.onesub (sub[!close,])
 ##       time.table.far$close <- FALSE
 ##   }, TRUE)
    
 ##   time.table <- rbind(time.table.close, time.table.far)
    
    return(time.table)
}


compress.by.season.corsi.fenwick <- function (dftable,
                                              thisgamestest=gamestest,
                                              roster.this=roster.unique) {
    ##dftable=playertable #subset(playertable, score.diff.cat < 6)
    ##dftable2=sapply(dftable, function(cc) unlist(cc))

    dfsg <- paste0(dftable$season, dftable$gcode)
    gtsg <- paste0(thisgamestest$season, thisgamestest$gcode)
    dftable$date <- thisgamestest$date[match(dfsg, gtsg)]

    teamname <- tapply(dftable$Team,
                       dftable$ID,
                       function(nn) {tms <- unique(as.character(nn)); paste(tms[nchar(tms)>0],collapse="/")})

    compressor <- function(df)
        summarize(df,
                  CF=sum(CF), CA=sum(CA),
                  FF=sum(FF), FA=sum(FA),
                  TOI=sum(TOI), TOIoff=sum(TOIoff),
                  Games=length(unique(gcode)))
    
    reduced.2 <- dftable %>% group_by(ID) %>% do(compressor(.))

    out <- cbind(teamname, reduced.2)

    out$CorsiPct <- out$CF/(out$CF + out$CA)
    out$FenwickPct <- out$FF/(out$FF + out$FA)
    out$TOIPct <- out$TOI/(out$TOI + out$TOIoff)
    out$TOIgame <- out$TOI/out$Games
    
    ##out$Name <- roster.this$firstlast[match(out$ID]
    
    return(out)
}




