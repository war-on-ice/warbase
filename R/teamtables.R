
# getTeamTable
# Author:  Sam Ventura
# Date:  20 August 2014
# Description:  gets team data for a specified subset of grand.data
# sub:  subset of grand.data (all columns) for a given period, game, timeframe, season, etc
# id:  the team ID / abbreviation


## Extra compression of the existing tables. Used to get composite scoring modes.
collapse.all.team.base <- function (sub.p, score.diff.cat.temp=7)
    summarize (sub.p, 
               TOI=sum(TOI), ZSO=sum(ZSO), ZSN=sum(ZSN), ZSD=sum(ZSD),
               GF=sum(GF),                 GA=sum(GA),                 SF=sum(SF),                 SA=sum(SA),
               FF=sum(FF),                 FA=sum(FA),                 CF=sum(CF),                 CA=sum(CA),
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

               FAC_WIN=sum( FAC_WIN),                    FAC_LOSE=sum(FAC_LOSE),
               PENL_TAKEN=sum(PENL_TAKEN),               PENL_DRAWN=sum(PENL_DRAWN),          
               HIT=sum(HIT),                             HIT_TAKEN =sum(HIT_TAKEN))

collapse.all.team.scdiff <- function (sub.p) group_by (sub.p, gamestate, period, home, Team, Opponent) %>% collapse.all.team.base
collapse.all.team.gmst <- function (sub.p) group_by (sub.p, score.diff.cat, period, home, Team, Opponent) %>% collapse.all.team.base
collapse.all.team.period <- function (sub.p) group_by (sub.p, score.diff.cat, gamestate, home, Team, Opponent) %>% collapse.all.team.base




getTeamTable.evsub <- function(sub.p, home=1)
    summarize(sub.p,
              home=home,
              Team=if (home==1) hometeam[1] else awayteam[1],
              Opponent=if (home==1) awayteam[1] else hometeam[1],
              
              GF=sum(etype == "GOAL" & ev.team == Team),    GA=sum(etype == "GOAL" & ev.team != Team),
              SF=GF+sum(etype == "SHOT" & ev.team == Team), SA=GA+sum(etype == "SHOT" & ev.team != Team),
              FF=SF+sum(etype == "MISS" & ev.team == Team), FA=SA+sum(etype == "MISS" & ev.team != Team),
              CF=FF+sum(etype == "BLOCK" & ev.team == Team),CA=FA+sum(etype == "BLOCK" & ev.team != Team),
              
              OZF=CF+sum(etype %in% c("HIT","GIVE","TAKE") & ((homezone == "Off" & home==1) | (homezone == "Def" & home==0))),
              OZA=CA+sum(etype %in% c("HIT","GIVE","TAKE") & ((homezone == "Off" & home==0) | (homezone == "Def" & home==1))),
                            
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
              ExpGA=sum(expG[ev.team != Team]),

              #SCF=sum(scoring.chance & ev.team == Team),
              #SCA=sum(scoring.chance & ev.team != Team),
             
              TOI=sum(event.length),
              ZSO=sum(etype == "FAC" & (homezone == "Off" & home > 0 | homezone == "Def" & home == 0)),
              ZSN=sum(etype == "FAC" & homezone == "Neu"),
              ZSD=sum(etype == "FAC" & (homezone == "Def" & home > 0 | homezone == "Off" & home == 0)),
              
              FAC_WIN=sum(etype == "FAC" & ev.team == Team),
              FAC_LOSE=sum(etype == "FAC" & ev.team != Team),
              PENL_TAKEN=sum(etype == "PENL" & ev.team == Team),
              PENL_DRAWN=sum(etype == "PENL" & ev.team != Team),
              HIT=sum(etype == "HIT" & ev.team == Team),
              HIT_TAKEN=sum(etype == "HIT" & ev.team != Team)
              )

getTeamTable <- function (subgame) {
    rbind(getTeamTable.evsub(subgame, 1),
          getTeamTable.evsub(subgame, 0))
}



team.table.sub <- function(sub) {
## load ("common-data/games/2014201530244.RData"); sub=playbyplay
    season <- as.character(sub$season[1])
    gcode <- as.character(sub$gcode[2])
    message("Team ",sub$season[1], sub$gcode[1])
    
    #sub <- subset(sub, home.skaters > 2 & away.skaters > 2)
    ## Screw the shootout!
    
    if (substr(sub$gcode[1],1,1) == "2") sub <- sub[sub$period < 5,]   #home.skaters > 2 & away.skaters > 2)
    #sub$score.diff.cat <- with(sub,
    #                           0*(home.score - away.score <= -3) +
    #                           1*(home.score - away.score == -2) +
    #                           2*(home.score - away.score == -1) +
    #                           3*(home.score - away.score == 0) +
    #                           4*(home.score - away.score == 1) +
    #                           5*(home.score - away.score == 2) +
    #                           6*(home.score - away.score >= 3))
    
    sub$gamestate <- with(sub,
                          1*(home.skaters == 6 & away.skaters == 6 & home.G > 1 & away.G > 1) +
                          2*(home.skaters > away.skaters & away.skaters > 2 & home.G > 1 & away.G > 1) +
                          3*(home.skaters < away.skaters & home.skaters > 2 & home.G > 1 & away.G > 1) +
                          4*(home.skaters == 5 & away.skaters == 5 & home.G > 1 & away.G > 1) +
                          5*(home.skaters > 2 & away.skaters > 2 & home.G > 1 & away.G == 1) +
                                        # the team with their goalie in
                          6*(home.skaters > 2 & away.skaters > 2 & home.G == 1 & away.G > 1) +
                          9*(home.skaters == 4 & away.skaters == 4 & home.G > 1 & away.G > 1))
                                        # the team with their goalie pulled
    
#      if(is.null(team.ids))  team.ids <- sort(unique(unlist(sub[,c("hometeam","awayteam")])))
    hometeam <- sub$hometeam[1]; awayteam <- sub$awayteam[1]
    sub$danger.zone <- danger.zone[sub$new.loc.section + 1] + 1*(sub$shot.feature %in% higher.prob.events)

    sub$homeevent <- 1*(sub$ev.team == sub$hometeam)
    sub <- left_join(sub, shot.results %>% mutate (season = anac(season)))
    sub$expG[!(sub$etype %in% c("SHOT","GOAL"))] <- 0

    
    ## clean.
    sub$homezone[is.na(sub$homezone)] <- "Neu"
    
    #getTeamTable ("TOR", sub)
    #f1 <- function(dd, team) {print(dim(dd)); as.data.frame(rbind( (team, dd)))}
    team.table <- 
        sub %>% group_by(score.diff.cat, gamestate, period) %>% do(getTeamTable(.))
       
    team.table <- augmentflip (team.table)

    tt.new <- rbind_list(team.table %>% collapse.all.team.scdiff %>% mutate(score.diff.cat=7),  ## all
                         team.table %>% make.count.adjustments.team(subseason=season) %>%
                           collapse.all.team.scdiff %>% mutate(score.diff.cat=8),  ## score-adjusted
                         team.table %>% filter (score.diff.cat %in% 2:4) %>% collapse.all.team.scdiff %>%
                           mutate(score.diff.cat=9),
                         team.table %>% filter ((score.diff.cat %in% 2:4 & period %in% 1:2) | score.diff.cat == 3) %>%
                           collapse.all.team.scdiff %>% mutate(score.diff.cat=10),
                         
                         team.table %>% filter (score.diff.cat %in% 4:6) %>%
                           collapse.all.team.scdiff %>% mutate(score.diff.cat=11),
                         team.table %>% filter (score.diff.cat %in% 5:6) %>%
                           collapse.all.team.scdiff %>% mutate(score.diff.cat=12),
                         team.table %>% filter (score.diff.cat %in% 0:1) %>%
                           collapse.all.team.scdiff %>% mutate(score.diff.cat=13),
                         team.table %>% filter (score.diff.cat %in% 0:2) %>%
                           collapse.all.team.scdiff %>% mutate(score.diff.cat=14))
    
    team.table <- rbind(team.table, tt.new)
    
    tt.all <- team.table %>% collapse.all.team.gmst %>% mutate(gamestate=7)
    team.table <- rbind(team.table, tt.all)

    tt.all.2 <- team.table %>% collapse.all.team.period %>% mutate (period=0)
    team.table <- rbind(team.table, tt.all.2)

    team.table$season <- season
    team.table$gcode <- gcode

    return(team.table)
}


collapse.all.team.game.scdiff <- function (sub.p) sub.p %>% group_by (gcode, gamestate, period, home, Team, Opponent) %>% collapse.all.team.base
collapse.all.team.game.gmst <- function (sub.p) sub.p %>% group_by (gcode, score.diff.cat, period, home, Team, Opponent) %>% collapse.all.team.base

team.table.allseason <- function(sub) {
## load ("source-data/nhlscrapr-20142015.RData"); sub=subset(grand.data, gcode==20001)
    season <- as.character(sub$season[1])
    ##gcode <- as.character(sub$gcode[2])
    message("Team All",season)
    
    #sub <- subset(sub, home.skaters > 2 & away.skaters > 2)
    ## Screw the shootout!
    
    sub <- sub[substr(sub$gcode,1,1) == "3" | sub$period < 5,]   #home.skaters > 2 & away.skaters > 2)
    #sub$score.diff.cat <- with(sub, 0*(home.score - away.score <= -3) + 1*(home.score - away.score == -2) + 2*(home.score - away.score == -1) + 3*(home.score - away.score == 0) + 4*(home.score - away.score == 1) + 5*(home.score - away.score == 2) + 6*(home.score - away.score >= 3))
    
    sub$gamestate <- with(sub,
                          1*(home.skaters == 6 & away.skaters == 6 & home.G > 1 & away.G > 1) +
                          2*(home.skaters > away.skaters & away.skaters > 2 & home.G > 1 & away.G > 1) +
                          3*(home.skaters < away.skaters & home.skaters > 2 & home.G > 1 & away.G > 1) +
                          4*(home.skaters == 5 & away.skaters == 5 & home.G > 1 & away.G > 1) +
                          5*(home.skaters > 2 & away.skaters > 2 & home.G > 1 & away.G == 1) +
                                        # the team with their goalie in
                          6*(home.skaters > 2 & away.skaters > 2 & home.G == 1 & away.G > 1) +
                          9*(home.skaters == 4 & away.skaters == 4 & home.G > 1 & away.G > 1))
                                        # the team with their goalie pulled
    
#      if(is.null(team.ids))  team.ids <- sort(unique(unlist(sub[,c("hometeam","awayteam")])))
    hometeam <- sub$hometeam[1]; awayteam <- sub$awayteam[1]
    sub$danger.zone <- danger.zone[sub$new.loc.section + 1] + 1*(sub$shot.feature %in% higher.prob.events)
#    sub$scoring.chance <- scoring.chance(sub)

    #getTeamTable ("TOR", sub)
    #f1 <- function(dd, team) {print(dim(dd)); as.data.frame(rbind( (team, dd)))}
    team.table <- 
        sub %>% group_by(score.diff.cat, gamestate, gcode, period) %>% do(getTeamTable(.))
       
    team.table <- augmentflip (team.table)

    tt.new <- rbind_list(team.table %>% collapse.all.team.game.scdiff %>% mutate(score.diff.cat=7),  ## all
                         team.table %>% make.count.adjustments.team(subseason=season) %>%
                           collapse.all.team.game.scdiff %>% mutate(score.diff.cat=8),  ## score-adjusted
                         team.table %>% filter (score.diff.cat %in% 2:4) %>% collapse.all.team.game.scdiff %>%
                           mutate(score.diff.cat=9),
                         team.table %>% filter ((score.diff.cat %in% 2:4 & period %in% 1:2) | score.diff.cat == 3) %>%
                           collapse.all.team.game.scdiff %>% mutate(score.diff.cat=10),
                         
                         team.table %>% filter (score.diff.cat %in% 4:6) %>%
                           collapse.all.team.game.scdiff %>% mutate(score.diff.cat=11),
                         team.table %>% filter (score.diff.cat %in% 5:6) %>%
                           collapse.all.team.game.scdiff %>% mutate(score.diff.cat=12),
                         team.table %>% filter (score.diff.cat %in% 0:1) %>%
                           collapse.all.team.game.scdiff %>% mutate(score.diff.cat=13),
                         team.table %>% filter (score.diff.cat %in% 0:2) %>%
                           collapse.all.team.game.scdiff %>% mutate(score.diff.cat=14))
    
    team.table <- rbind(team.table, tt.new)
    tt.all <- team.table %>% collapse.all.team.game.gmst %>% mutate(gamestate=7)

    team.table <- rbind(team.table, tt.all)
    
    team.table$season <- season
    #team.table$gcode <- gcode

    return(team.table)
}



