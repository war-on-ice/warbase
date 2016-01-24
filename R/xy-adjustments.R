
###########################################################################
##
## First up: add distances to those with coordinates. We're fine with long shots.
library(nhlscrapr)

create.distance <- function (seasons=3:12, direc="source-data/", common="common-data/", verbose=TRUE) {
    load(paste0(common,"woi-common.RData"))
    for (ss in seasons) {
        message ("Create shot distance ",ss)
        load(paste0(direc, "nhlscrapr-",ss,".RData"))
        shot.events <- c("GOAL","SHOT","MISS","BLOCK")
        new.distance <- round(sqrt((89-abs(grand.data$xcoord))^2 + grand.data$ycoord^2))
        grand.data$distance[is.na(grand.data$distance) & grand.data$etype %in% shot.events] <-
            new.distance[is.na(grand.data$distance) & grand.data$etype %in% shot.events]
        save(grand.data, file=paste0(direc, "nhlscrapr-",ss,".RData"))
    }
}

compare.CDF <- function (original, target, range=1:200) {
    ## range=1:200

    original <- sort(original); target <- sort(target)
    oq <- quantile(original, seq(0,1,by=0.001), na.rm=TRUE)
    tq <- quantile(target, seq(0,1,by=0.001), na.rm=TRUE)

    values <- sapply(range, function(rr) mean(tq[oq==rr]))
    if (is.na(values[1])) values[1] <- range[1]
    
    picks <- 2:length(values)
    for (kk in picks[is.na(values[-1])]) values[kk] <- values[kk-1] + 1
    values[values > max(range)] <- max(range)
    names(values) <- range
    values
    
}

create.adjusted.distance <- function (direc="source-data/", common="common-data/", verbose=TRUE) {

    load(paste0(common,"woi-common.RData"))
    season.blocks <- c(sort(rep(1:6,2)), 6)

    elements <- expand.grid (unique(season.blocks),
                             is.slap=c(FALSE,TRUE),
                             team=unique(gamestest$hometeam[gamestest$hometeam != ""]),
                             stringsAsFactors = FALSE)

    shot.events <- c("GOAL","SHOT","MISS","BLOCK")
    bigload <- do.call(rbind, lapply(seasons, function(ss) {
        load (paste0(direc,"nhlscrapr-",ss,".RData"))
        grand.data[grand.data$etype %in% shot.events, c("season", "type", "ev.team", "hometeam", "awayteam", "distance")]
    }))
    
    quantile.adjust <- sapply (1:nrow(elements), function(rr) {
        prop <- elements[rr,];  message (paste (unlist(prop)))
        original <- bigload$distance[season.blocks[match(bigload$season, seasons)] == prop[[1]] &
                                     (bigload$type == "Slap") == prop[[2]] &
                                     bigload$hometeam == prop[[3]]]
        target <- bigload$distance[season.blocks[match(bigload$season, seasons)] == prop[[1]] &
                                   (bigload$type == "Slap") == prop[[2]] &
                                   bigload$awayteam == prop[[3]]]
        compare.CDF (original, target)
    })

    save (elements, quantile.adjust, file=paste0(direc, "distance-adjust.RData"))
    return(TRUE)
    ##    return (list(elements=elements,
    ##                 quantile.adjust=quantile.adjust))
    
}


project.xy.on.line <- function (baby.data, full=FALSE) {
    #baby.data=grand.data

    shot.events <- c("GOAL","SHOT","MISS","BLOCK")
    subrows <- if (full) which(!is.na(baby.data$xcoord) & baby.data$etype %in% shot.events) else which(is.na(baby.data$newxc) & !is.na(baby.data$xcoord) & baby.data$etype %in% shot.events)
    
    sub.coord <- baby.data[subrows, c("xcoord","ycoord")]
    dist.ratio <- baby.data$adjusted.distance[subrows]/baby.data$distance[subrows]
    
    xsign <- -1*(sub.coord$xcoord < 0) + 1*(sub.coord$xcoord >= 0)
    flip <- which(xsign < 0)
    
    sub.coord$xcoord[flip] <- -sub.coord$xcoord[flip]; sub.coord$ycoord[flip] <- -sub.coord$ycoord[flip]

    sub.coord$xcoord <- 89 - sub.coord$xcoord
    sub.coord$xcoord <- round(dist.ratio*sub.coord$xcoord)
    sub.coord$ycoord <- round(dist.ratio*sub.coord$ycoord)
    sub.coord$xcoord <- 89 - sub.coord$xcoord
    #sub.coord$xcoord[flip] <- -sub.coord$xcoord[flip]
    #sub.coord$ycoord[flip] <- -sub.coord$ycoord[flip]
    
    baby.data[subrows,c("newxc","newyc")] <- sub.coord
    return(baby.data)
}


update.adjusted.distance <- function (seas, direc="source-data/", common="common-data/", verbose=TRUE) {
    load(paste0(common,"woi-common.RData"))
    #seas = seasons
    season.blocks <- c(sort(rep(2:6,2)), 6)
    shot.events <- c("GOAL","SHOT","MISS","BLOCK")
    load (file=paste0(direc, "distance-adjust.RData"))  #elements, quantile.adjust, 

    for (ss in seas) {
        message ("update.adjusted.distance ",ss)
        
        load (paste0(direc,"nhlscrapr-",ss,".RData"))

        ## does the distance exist?
        pix <- is.na(grand.data$distance) & grand.data$etype %in% c("GOAL","SHOT","MISS","BLOCK")
        grand.data$distance[pix] <- sqrt((89-abs(grand.data$xcoord[pix]))^2 + grand.data$ycoord[pix]^2)
        
        this.block <- season.blocks[match(ss, seasons)]
        for (rr in which(elements[,1] == this.block)) {
            prop <- elements[rr,];  ##  message (paste (unlist(prop)))
            these.rows <- which (grand.data$etype %in% shot.events &
                                 is.na(grand.data$adjusted.distance) &
                                 (grand.data$type == "Slap") == prop[[2]] &
                                 grand.data$hometeam == prop[[3]])

        #    print(c(rr, table(grand.data$distance[these.rows])))
        #    print(c(rr, table(quantile.adjust[grand.data$distance[these.rows]])))
            
            if (length(these.rows)>0) {
                hold1 <- round(grand.data$distance[these.rows]); hold1[hold1==0] <- 1; hold1[hold1>200] <- 200
                grand.data$adjusted.distance[these.rows] <-
                    quantile.adjust[hold1, rr]
            }
        }
        message ("update.adjusted.distance block element ",ss)
        
        grand.data <- project.xy.on.line (grand.data)

        message ("update.adjusted.distance block element done ",ss)

        grand.data$loc.section <- pick.section(grand.data[,c("xcoord","ycoord")])
        grand.data$new.loc.section <- pick.section(grand.data[,c("newxc","newyc")])
        save (grand.data, file=paste0(direc,"nhlscrapr-",ss,".RData"))
    }

    
}

update.section <- function (seas, direc="source-data/", common="common-data/", verbose=TRUE) {
    load(paste0(common,"woi-common.RData"))
    #seas = seasons[3:12]
    season.blocks <- sort(rep(1:6,2))
    shot.events <- c("GOAL","SHOT","MISS","BLOCK")

    for (ss in seas) {
        load (paste0(direc,"nhlscrapr-",ss,".RData"))
        grand.data$loc.section <- pick.section(grand.data[,c("xcoord","ycoord")])
        grand.data$new.loc.section <- pick.section(grand.data[,c("newxc","newyc")])
        save (grand.data, file=paste0(direc,"nhlscrapr-",ss,".RData"))
    }

    
}

