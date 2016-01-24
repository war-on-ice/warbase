
##
goalie.sum <- function (sub, thisishome=1)
    summarize(sub,
              home=thisishome,
              Team=if (home==1) hometeam[1] else awayteam[1],
              Opponent=if (home==0) hometeam[1] else awayteam[1],
              goals.0=sum(etype=="GOAL" & ev.team==Opponent & sub$danger.zone == 0),
              shots.0=sum(etype=="SHOT" & ev.team==Opponent & sub$danger.zone == 0),
              goals.1=sum(etype=="GOAL" & ev.team==Opponent & sub$danger.zone == 1),
              shots.1=sum(etype=="SHOT" & ev.team==Opponent & sub$danger.zone == 1),
              goals.2=sum(etype=="GOAL" & ev.team==Opponent & sub$danger.zone == 2),
              shots.2=sum(etype=="SHOT" & ev.team==Opponent & sub$danger.zone == 2),
              goals.3=sum(etype=="GOAL" & ev.team==Opponent & sub$danger.zone == 3),
              shots.3=sum(etype=="SHOT" & ev.team==Opponent & sub$danger.zone == 3),
              goals.4=sum(etype=="GOAL" & ev.team==Opponent & sub$danger.zone == 4),
              shots.4=sum(etype=="SHOT" & ev.team==Opponent & sub$danger.zone == 4),
              ExpGA=sum(expG[ev.team == Opponent]),
              TOI=sum(event.length))

collapse.all.G.base <- function (sub)
    summarize (sub,
               goals.0=sum(goals.0), shots.0=sum(shots.0),
               goals.1=sum(goals.1), shots.1=sum(shots.1),
               goals.2=sum(goals.2), shots.2=sum(shots.2),
               goals.3=sum(goals.3), shots.3=sum(shots.3),
               goals.4=sum(goals.4), shots.4=sum(shots.4),
               ExpGA=sum(ExpGA),
               TOI=sum(TOI))
               
collapse.all.G.scdiff <- function (sub)
    group_by (sub, ID, gamestate, period, home, Team, Opponent) %>% collapse.all.G.base
collapse.all.G.gmst <- function (sub)
    group_by (sub, ID, score.diff.cat, period, home, Team, Opponent) %>% collapse.all.G.base
collapse.all.G.period <- function (sub)
    group_by (sub, ID, score.diff.cat, gamestate, home, Team, Opponent) %>% collapse.all.G.base




getGoaltenderTable.one <- function (sub) {
    ## load ("source-data/nhlscrapr-20142015.RData"); sub=subset(grand.data, gcode==20001)
    ## sub = playbyplay
    sub$eventgoalie <- sub$home.G
    sub$eventgoalie[sub$ev.team == sub$hometeam] <- sub$away.G[sub$ev.team == sub$hometeam]
    sub$home <- 1*(sub$eventgoalie == sub$home.G)
    
    sub$danger.zone <- danger.zone[sub$new.loc.section + 1] + 1*(sub$shot.feature %in% higher.prob.events)
    sub$danger.zone[is.na(sub$danger.zone)] <- 1
    sub$danger.zone[sub$danger.zone == 0] <- 1
    if (substr(sub$gcode[1],1,1) == "2") sub <- sub[sub$period < 5,]   #home.skaters > 2 & away.skaters > 2)

    #sub$score.diff.cat <- with(sub,  ## Just upped to 3 goal splits.
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

    sub$homeevent <- 1*(sub$ev.team == sub$hometeam)
    sub <- left_join(sub, shot.results %>% mutate (season = anac(season)))
    sub$expG[!(sub$etype %in% c("SHOT","GOAL"))] <- 0

    
    ##, eventgoalie, score.diff.cat
    hometable <- sub %>% group_by(home.G, score.diff.cat, gamestate, period) %>% do(goalie.sum(., 1)) %>% filter(home.G > 1)
    awaytable <- sub %>% group_by(away.G, score.diff.cat, gamestate, period) %>% do(goalie.sum(., 0)) %>% filter(away.G > 1) %>% augmentflip
    colnames(hometable)[1] <- "ID"; colnames(awaytable)[1] <- "ID"

    goalie.table <- rbind_list(hometable, awaytable)
    
    gt.new <- rbind_list(goalie.table %>% collapse.all.G.scdiff %>% mutate(score.diff.cat=7),  ## all
                         goalie.table %>% collapse.all.G.scdiff %>% mutate(score.diff.cat=8),     ## score-adjusted
                         goalie.table %>% filter (score.diff.cat %in% 2:4) %>%
                           collapse.all.G.scdiff %>% mutate(score.diff.cat=9),  ## within 1
                         goalie.table %>% filter ((score.diff.cat %in% 2:4 & period %in% 1:2) |
                                                  score.diff.cat == 3) %>% collapse.all.G.scdiff %>%
                           mutate(score.diff.cat=10), ## close
                         
                         goalie.table %>% filter (score.diff.cat %in% 4:6) %>%
                           collapse.all.G.scdiff %>% mutate(score.diff.cat=11), ## leading 
                         goalie.table %>% filter (score.diff.cat %in% 5:6) %>%
                           collapse.all.G.scdiff %>% mutate(score.diff.cat=12), ## leading 2+
                         goalie.table %>% filter (score.diff.cat %in% 0:1) %>%
                           collapse.all.G.scdiff %>% mutate(score.diff.cat=13), ## trailing 2+
                         goalie.table %>% filter (score.diff.cat %in% 0:2) %>%
                           collapse.all.G.scdiff %>% mutate(score.diff.cat=14)) ## trailing

    goalie.table <- rbind(goalie.table, gt.new)

    gt.all <- goalie.table %>% collapse.all.G.gmst %>% mutate(gamestate=7)
    goalie.table <- rbind(goalie.table, gt.all)

    gt.all.2 <- goalie.table %>% collapse.all.G.period %>% mutate(period=0)
    goalie.table <- rbind(goalie.table, gt.all.2)

   
    return(goalie.table)

}

getGoaltenderTable.object <- function(sub, roster.this=roster.unique){
    ##season="20102011"; gcode="20556"
    message("OGoalie", sub$season[1], sub$gcode[1])
    ## take appropriate subset of grand.data and find all unique players in the subset
  
    ##sub <- grand.data[grand.data$season == season & grand.data$gcode == gcode, ]
    goal.table <- cbind(season=sub$season[1],
                        gcode=sub$gcode[1],
                        getGoaltenderTable.one (sub))

    goal.table$ID <- roster.this$woi.id[goal.table$ID]
    

    return(goal.table)

}

      




## old news. 10-24-2014

getGoaltenderTable <- function(season = "20132014", gcode = "30415"){
    #season="20102011"; gcode="20556"
    message("Goalie", season, gcode)
  # take appropriate subset of grand.data and find all unique players in the subset
  
  #sub <- grand.data[grand.data$season == season & grand.data$gcode == gcode, ]
  if (any(this.gametable[,1]==season & this.gametable[,2]==gcode)) {

      sub <- split.data[[which(this.gametable[,1]==season & this.gametable[,2]==gcode)]]
      goal.table <- cbind(season=sub$season[1],
                          gcode=sub$gcode[1],
                          getGoaltenderTable.one (sub))

  } else goal.table <- NULL

  return(goal.table)

}


testers <- function () {

    g1 <- by (goalietable[,7:12], list(goalietable$mansit, goalietable$scorediffcat), colSums)
    g2 <- lapply(g1, function(gg) c(gg[1]/sum(gg[1:2]), gg[3]/sum(gg[3:4]), gg[5]/sum(gg[5:6])))

}
