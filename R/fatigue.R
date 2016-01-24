

## load ("../../common-data/woi-common.RData"); games=gamestest
add.fatigue <- function (games) {

    games$homeafterhome <- games$homeafteraway <- games$awayafterhome <- games$awayafteraway <- 0
    teams <- unique(c(games$hometeam, games$awayteam)); teams <- teams[nchar(teams)>0]
    lastgame <- rep(as.Date("2000-01-01"), length(teams))
    last.home <- rep(0, length(teams))

##    dofnh <- function (tt) data.frame(season=tt$season, gcode=tt$gcode, date=tt$date, team=tt$hometeam, home=1)
##    schedule.h <- games %>% group_by (hometeam) %>% do(dofnh(.))
##    dofna <- function (tt) data.frame(season=tt$season, gcode=tt$gcode, date=tt$date, team=tt$hometeam, home=0)
##    schedule.a <- games %>% group_by (hometeam) %>% do(dofna(.))
##    schedule.full <- rbind_list(schedule.h, schedule.a)
    
    for (kk in which(nchar(games$hometeam) > 0 & nchar(games$awayteam) > 0 & !is.na(games$date))) {
        if (games$date[kk] == lastgame[which(teams == games$hometeam[kk])] + 1) {
            games$homeafterhome[kk] <- last.home[which(teams == games$hometeam[kk])]
            games$homeafteraway[kk] <- 1-last.home[which(teams == games$hometeam[kk])]
        }
        if (games$date[kk] == lastgame[which(teams == games$awayteam[kk])] + 1) {
            games$awayafterhome[kk] <- last.home[which(teams == games$awayteam[kk])]
            games$awayafteraway[kk] <- 1-last.home[which(teams == games$awayteam[kk])]
        }
        lastgame[which(teams == games$hometeam[kk])] <- lastgame[which(teams == games$awayteam[kk])] <- games$date[kk]
        last.home[which(teams == games$hometeam[kk])] <- 1
        last.home[which(teams == games$awayteam[kk])] <- 0
    }
    return(games)
    
}
