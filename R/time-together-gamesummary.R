
##################################################################################
##
## Pieces needed for wowy-like stats.


## get time on ice.
## playerblock: a1:a6 or h1:h6 from playbyplay
## event.length, from playbyplay

get.TOI <- function (playerblock, event.length) {
    plbl <- unlist(playerblock)
    v1 <- tapply(rep(event.length, length(plbl)/length(event.length)), c(plbl), sum)
    return(sort(v1[names(v1) != 1]))
}


## Positions for the shift blocks.

make.shift.pieces <- function (playbyplay, rosters=roster.unique) {
    ## playbyplay=retrieve.game("20132014","20009")$playbyplay
    playbyplay <- filter (playbyplay, etype != "GOFF")
    
    home.pl <- c("h1","h2","h3","h4","h5","h6")
    away.pl <- c("a1","a2","a3","a4","a5","a6")
    
    home.players <- unique(unlist(playbyplay[,home.pl])); home.players <- sort(home.players[home.players != "xxxxxxxNA"])
    away.players <- unique(unlist(playbyplay[,away.pl])); away.players <- away.players[away.players != "xxxxxxxNA"]
    
    home.out <- matrix (0, ncol=length(home.players), nrow=nrow(playbyplay))
    for (kk in 1:nrow(home.out)) home.out[kk, match(unlist(playbyplay[kk, home.pl]),
                                                    home.players)] <- 1
    colnames(home.out) <- rosters$firstlast[match(home.players, rosters$woi.id)]
    rownames(home.out) <- playbyplay$seconds
    
    pos.home <- rosters$pos[match(home.players, rosters$woi.id)]
    toi.home <- get.TOI(playbyplay[,home.pl], playbyplay$event.length)
    toi.home <- toi.home[match(as.character(home.players), names(toi.home))]
    ## print(toi)
  
    home.out <- home.out[,rev(order(1*!grepl("D", pos.home), toi.home))]


  
    away.out <- matrix (0, ncol=length(away.players), nrow=nrow(playbyplay))
    for (kk in 1:nrow(away.out)) away.out[kk, match(unlist(playbyplay[kk, away.pl]),
                                                    away.players)] <- 1
    
    colnames(away.out) <- rosters$firstlast[match(away.players, rosters$woi.id)]
    rownames(away.out) <- playbyplay$seconds
    
    pos.away <- rosters$pos[match(away.players, rosters$woi.id)]
    toi.away <- get.TOI(playbyplay[,away.pl], playbyplay$event.length)
    toi.away <- toi.away[match(as.character(away.players), names(toi.away))]
    
    away.out <- away.out[,rev(order(1*!grepl("D", pos.away), toi.away))]
    
    out <- list(away.out=away.out,
                home.out=home.out)
    return(out)
  
}

make.shift.pieces.new <- function (playbyplay, rosters=roster.unique) {
  ## playbyplay=retrieve.game("20132014","20009")$playbyplay
  playbyplay <- filter (playbyplay, etype != "GOFF")
  
  home.pl <- c("h1","h2","h3","h4","h5","h6")
  #away.pl <- c("a1","a2","a3","a4","a5","a6")
  
  home.players <- unique(unlist(playbyplay[,home.pl])); 
  home.players <- sort(home.players[home.players != "xxxxxxxNA"])
  #away.players <- unique(unlist(playbyplay[,away.pl])); away.players <- away.players[away.players != "xxxxxxxNA"]
  
  #home.out <- matrix (0, ncol=length(home.players), nrow=nrow(playbyplay))
  for (kk in 1:nrow(home.out)) 
    home.out[kk, match(unlist(playbyplay[kk, home.pl]), home.players)] <- 1
  colnames(home.out) <- c("Player", "Shift", "ShiftStart", "ShiftEnd", "Team")
  
  #pos.home <- rosters$pos[match(home.players, rosters$woi.id)]
  #toi.home <- get.TOI(playbyplay[,home.pl], playbyplay$event.length)
  #toi.home <- toi.home[match(as.character(home.players), names(toi.home))]
  ## print(toi)
  
  #home.out <- home.out[,rev(order(1*!grepl("D", pos.home), toi.home))]
  
  
  
#   away.out <- matrix (0, ncol=length(away.players), nrow=nrow(playbyplay))
#   for (kk in 1:nrow(away.out)) away.out[kk, match(unlist(playbyplay[kk, away.pl]),
#                                                   away.players)] <- 1
#   
#   colnames(away.out) <- rosters$firstlast[match(away.players, rosters$woi.id)]
#   rownames(away.out) <- playbyplay$seconds
#   
#   pos.away <- rosters$pos[match(away.players, rosters$woi.id)]
#   toi.away <- get.TOI(playbyplay[,away.pl], playbyplay$event.length)
#   toi.away <- toi.away[match(as.character(away.players), names(toi.away))]
#   
#   away.out <- away.out[,rev(order(1*!grepl("D", pos.away), toi.away))]
#   
#   out <- list(away.out=away.out,
#               home.out=home.out)
  return(home.out)
  
}

## output: n-by-5 edge list from play by play:
## player 1, player 2, time, events for, events against

make.coplay.edge.lists <- function (pbp, roster.this=roster.unique, woiid=FALSE) {

    message ("Coplay edge list ",pbp$season[1]," ",pbp$gcode[1])
    pbp <- filter (pbp, etype != "GOFF")
    
    
    if (!any(colnames(pbp) == "event.value"))
        pbp$event.value <- 1*(pbp$etype %in% c("GOAL","SHOT","MISS","BLOCK"))
    
    pbp$fiver <- if (!woiid) (pbp$home.skaters == 6 & pbp$away.skaters == 6 & pbp$home.G > 1 & pbp$away.G > 1) else (pbp$home.skaters == 6 & pbp$away.skaters == 6 & pbp$home.G != "xxxxxxxNA" & pbp$away.G != "xxxxxxxNA")
    total.time.all <- sum(pbp$event.length)
    total.time.five <- sum(pbp$event.length*pbp$fiver)

    
    homehome.prime <- rbind_all(
        lapply(1:6, function (ff)
               rbind_all(
                   lapply(1:6, function (gg)
                          data.frame(p1=pbp[[paste0("h",ff)]],
                                     p2=pbp[[paste0("h",gg)]],
                                     el=pbp$event.length,
                                     evf=pbp$event.value*(pbp$ev.team == pbp$hometeam),
                                     eva=pbp$event.value*(pbp$ev.team == pbp$awayteam),
                                     el2=pbp$event.length*pbp$fiver,
                                     evf2=pbp$event.value*(pbp$ev.team == pbp$hometeam)*pbp$fiver,
                                     eva2=pbp$event.value*(pbp$ev.team == pbp$awayteam)*pbp$fiver
                                     )))))
    homehome <- homehome.prime %>% group_by(p1,p2) %>%
        summarize (el=sum(el), eloff= total.time.all - el,
                   evf=sum(evf), eva=sum(eva),
                   el2=sum(el2), el2off= total.time.five - el2,
                   evf2=sum(evf2), eva2=sum(eva2)) %>% mutate(gr=1)
    
    
    awayaway.prime <- rbind_all(
        lapply(1:6, function (ff)
               rbind_all(
                   lapply(1:6, function (gg)
                          data.frame(p1=pbp[[paste0("a",ff)]],
                                     p2=pbp[[paste0("a",gg)]],
                                     el=pbp$event.length,
                                     evf=pbp$event.value*(pbp$ev.team == pbp$awayteam),
                                     eva=pbp$event.value*(pbp$ev.team == pbp$hometeam),
                                     el2=pbp$event.length*pbp$fiver,
                                     evf2=pbp$event.value*(pbp$ev.team == pbp$awayteam)*pbp$fiver,
                                     eva2=pbp$event.value*(pbp$ev.team == pbp$hometeam)*pbp$fiver
                                     )))))
    awayaway <- awayaway.prime %>% group_by(p1,p2) %>%
        summarize (el=sum(el), eloff= total.time.all - el,
                   evf=sum(evf), eva=sum(eva),
                   el2=sum(el2),el2off= total.time.five - el2,
                   evf2=sum(evf2), eva2=sum(eva2)) %>%
            mutate(gr=2)
    
    
    homeaway.prime <- rbind_all(
        lapply(1:6, function (ff)
               rbind_all(
                   lapply(1:6, function (gg)
                          data.frame(p1=pbp[[paste0("h",ff)]],
                                     p2=pbp[[paste0("a",gg)]],
                                     el=pbp$event.length,
                                     evf=pbp$event.value*(pbp$ev.team == pbp$hometeam),
                                     eva=pbp$event.value*(pbp$ev.team == pbp$awayteam),
                                     el2=pbp$event.length*pbp$fiver,
                                     evf2=pbp$event.value*(pbp$ev.team == pbp$hometeam)*pbp$fiver,
                                     eva2=pbp$event.value*(pbp$ev.team == pbp$awayteam)*pbp$fiver
                                     )))))
    homeaway <- homeaway.prime %>% group_by(p1,p2) %>%
        summarize (el=sum(el), eloff= total.time.all - el,
                   evf=sum(evf), eva=sum(eva),
                   el2=sum(el2), el2off= total.time.five - el2,
                   evf2=sum(evf2), eva2=sum(eva2)) %>%
            mutate(gr=3)

    
    
    
    awayhome.prime <- rbind_all(
        lapply(1:6, function (ff)
               rbind_all(
                   lapply(1:6, function (gg)
                          data.frame(p1=pbp[[paste0("a",ff)]],
                                     p2=pbp[[paste0("h",gg)]],
                                     el=pbp$event.length,
                                     evf=pbp$event.value*(pbp$ev.team == pbp$awayteam),
                                     eva=pbp$event.value*(pbp$ev.team == pbp$hometeam),
                                     el2=pbp$event.length*pbp$fiver,
                                     evf2=pbp$event.value*(pbp$ev.team == pbp$awayteam)*pbp$fiver,
                                     eva2=pbp$event.value*(pbp$ev.team == pbp$hometeam)*pbp$fiver
                                     )))))
    awayhome <- awayhome.prime %>% group_by(p1,p2) %>%
        summarize (el=sum(el), eloff= total.time.all - el,
                   evf=sum(evf), eva=sum(eva),
                   el2=sum(el2), el2off= total.time.five - el2,
                   evf2=sum(evf2), eva2=sum(eva2)) %>%
            filter(p1 > 1, p2 > 1) %>% mutate(gr=3)
    
    output <- rbind_list(homehome,
                         awayaway,
                         homeaway,
                         awayhome) %>% mutate (season=pbp$season[1], gcode=pbp$gcode[1])
    
    if (!woiid) {
        output[["p1"]] <- roster.this$woi.id[output[["p1"]]]
        output[["p2"]] <- roster.this$woi.id[output[["p2"]]]
    }

    output <- filter(output, p1 != "xxxxxxxNA", p2 != "xxxxxxxNA")

    return(output)
    
}




augment.playerrun.teamcomp <- function (playerrun, coplayer) {
    ## load ("common-data/games/2015201620172.RData")
    
    ## first, use coplayer to get the actual players involved.
    message ("Augment T/C ",playerrun$season[1]," ",playerrun$gcode[1])
    
    player.set <- unique(coplayer$p1)

    reduced.connect <- src_sqlite("common-data/player-summary.sqlite")
    statement <- paste0("SELECT * FROM reducedplayer WHERE season = ", playerrun$season[1])
    primer <- tbl(reduced.connect, sql(statement)) %>% filter (p1 %in% player.set)

    maxpiece <- function (subdf) subdf[nrow(subdf),]

    ## can we find the game they played in? Yes:
    primer1 <- as.data.frame(filter (primer, gcode == playerrun$gcode[1]))
    ## No:
    if (nrow(primer1) == 0) primer1 <- primer %>% group_by (p1) %>% do(maxpiece(.)) %>% as.data.frame
    primer1 <- select (primer1, -p1) ##rename (primer1, p2 = p1)

    coplayer.augmented <- filter(coplayer, p1 != p2) %>% mutate (opp = 1*(gr==3)) %>% left_join (primer1 %>% select (p2, lCF60, lCA60, lTOI60), by="p2") %>% filter (!is.na(lCF60))
    coplayer.reduced <- coplayer.augmented %>% group_by (p1, opp) %>%
        summarize (xTOI60 = sum(lTOI60 * el2)/sum(el2),
                   xCF60 = sum(lCF60 * el2)/sum(el2),
                   xCA60 = sum(lCA60 * el2)/sum(el2))
                   
    playerrun$tCF60 = coplayer.reduced$xCF60[match(paste("0",playerrun$ID),
        paste(coplayer.reduced$opp, coplayer.reduced$p1))]
    playerrun$cCF60 = coplayer.reduced$xCF60[match(paste("1",playerrun$ID),
        paste(coplayer.reduced$opp, coplayer.reduced$p1))]

    playerrun$tCA60 = coplayer.reduced$xCA60[match(paste("0",playerrun$ID),
        paste(coplayer.reduced$opp, coplayer.reduced$p1))]
    playerrun$cCA60 = coplayer.reduced$xCA60[match(paste("1",playerrun$ID),
        paste(coplayer.reduced$opp, coplayer.reduced$p1))]

    playerrun$tTOI60 = coplayer.reduced$xTOI60[match(paste("0",playerrun$ID),
        paste(coplayer.reduced$opp, coplayer.reduced$p1))]
    playerrun$cTOI60 = coplayer.reduced$xTOI60[match(paste("1",playerrun$ID),
        paste(coplayer.reduced$opp, coplayer.reduced$p1))]
    
    ## teammate stuff:

    return(playerrun)
}



switch.number.woiid <- function (playbyplay, roster.unique) {

    mutate (playbyplay,
            
            a1 = roster.unique$woi.id[a1],
            a2 = roster.unique$woi.id[a2],
            a3 = roster.unique$woi.id[a3],
            a4 = roster.unique$woi.id[a4],
            a5 = roster.unique$woi.id[a5],
            a6 = roster.unique$woi.id[a6],
            
            h1 = roster.unique$woi.id[h1],
            h2 = roster.unique$woi.id[h2],
            h3 = roster.unique$woi.id[h3],
            h4 = roster.unique$woi.id[h4],
            h5 = roster.unique$woi.id[h5],
            h6 = roster.unique$woi.id[h6],
            
            ev.player.1 = roster.unique$woi.id[ev.player.1],
            ev.player.2 = roster.unique$woi.id[ev.player.2],
            ev.player.3 = roster.unique$woi.id[ev.player.3],

            away.G = roster.unique$woi.id[away.G],
            home.G = roster.unique$woi.id[home.G])
            
}

