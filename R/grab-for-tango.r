
tango.grab <- function () {

    ##library(dplyr)
    load ("../../common-data/woi-common.RData")
    
    biggametable <- rbind_all(lapply (seasons[3:12], function (ss) {
        load (paste0("../../source-data/nhlscrapr-",ss,".RData"))
        
        grand.data <- subset(grand.data, ((substr(gcode,1,1)==2 & period < 5) | substr(gcode,1,1)==3) &
                             home.skaters == 6 & away.skaters == 6 & home.G > 1 & away.G > 1)
        grand.data %>% group_by(season, gcode) %>%
            summarize (TeamIdHome=hometeam[1],
                       TeamIdAway=awayteam[1],
                       EVgoalsHome=sum(etype=="GOAL" & ev.team==hometeam),
                       EVsavesHome=sum(etype=="SHOT" & ev.team==hometeam),
                       EVshotsWideHome=sum(etype=="MISS" & ev.team==hometeam),
                       EVshotsBlockedHome=sum(etype=="BLOCK" & ev.team==hometeam),
                       EVgoalsAway=sum(etype=="GOAL" & ev.team!=hometeam),
                       EVsavesAway=sum(etype=="SHOT" & ev.team!=hometeam),
                       EVshotsWideAway=sum(etype=="MISS" & ev.team!=hometeam),
                       EVshotsBlockedAway=sum(etype=="BLOCK" & ev.team!=hometeam))
    }
                                      ))
    
    write.csv (biggametable, "biggametable5v5ES.csv")
}



