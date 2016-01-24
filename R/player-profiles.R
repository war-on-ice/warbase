
########################################################################################
##
## Marge TSN player info with our roster info.

redo.roster <- function () {
    load ("source-data/nhlscrapr-core.RData")

    tsndata <- read.csv("source-data/playerfacts-20141130.csv")
    load ("source-data/newtsntable.RData")

    matcher <- cbind(roster.unique$firstlast,
                     match(tolower(roster.unique$firstlast), tolower(tsndata$Name)),
                     match(tolower(roster.unique$firstlast), tolower(table.roster.2$Name)))
#write.csv (matcher[order(is.na(matcher[,2])),], "matchtest.csv")
    tsndata$pos <- gsub("[W/]","",tsndata$pos)
    table.roster.2$pos <- gsub("[W/]","",table.roster.2$pos)
    
    roster.unique$newpos <- tsndata$pos[match(tolower(roster.unique$firstlast), tolower(tsndata$Name))]
    roster.unique[,c("DOB","Height","Weight","Shoots")] <-
        tsndata[match(tolower(roster.unique$firstlast), tolower(tsndata$Name)),
                c("DOB","Height","Weight","Shoots")]
    
    roster.unique[is.na(roster.unique$DOB),c("DOB","Height","Weight","Shoots")] <-
        tsndata[match(tolower(roster.unique$firstlast), tolower(tsndata$Name)),
                c("DOB","Height","Weight","Shoots")][is.na(roster.unique$DOB),]
    roster.unique$newpos[is.na(roster.unique$newpos)] <-
        table.roster.2$pos[match(tolower(roster.unique$firstlast), tolower(table.roster.2$Name))][is.na(roster.unique$newpos)]
    roster.unique$pos[!is.na(roster.unique$newpos)] <- roster.unique$newpos[!is.na(roster.unique$newpos)]
    roster.unique <- roster.unique[,-ncol(roster.unique)]
    
    save(games, roster.master, roster.unique, file="source-data/nhlscrapr-core.RData")
}

## subset(roster.unique, pos=="C" & !grepl("C",newpos))





#"games"         "roster.master" "roster.unique"
