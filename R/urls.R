
## URLs for teams and players: primary link

un.url <- function (url)
    gsub("<.*>(.*)<.*>", "\\1", url)


teamtime.url <- function (team) gsub("([A-Z\\.]{3})","<a href=\"http://www.war-on-ice.com/teambygame.html?team=\\1\" target=\"_blank\">\\1</a>", team)
##    paste0 ("<a href=\"http://www.war-on-ice.com/teambygame.html?team=",team,"\" target=\"_blank\">",team,"</a>")

playerseason.url.old <- function (player)
    paste0 ("<a href=\"http://www.war-on-ice.com/playerseason.html?name1=",
            gsub("[ \\.]","0",player),"\" target=\"_blank\">",player,"</a>")
goalieseason.url.old <- function (player)
    paste0 ("<a href=\"http://www.war-on-ice.com/goalieseason.html?name1=",
            gsub("[ \\.]","0",player),"\" target=\"_blank\">",player,"</a>")


playerseason.url <- function (player, woiid)
    paste0 ("<a href=\"http://www.war-on-ice.com/playerseason.html?woiid=",
            woiid,"\" target=\"_blank\">",player,"</a>")
goalieseason.url <- function (player, woiid)
    paste0 ("<a href=\"http://www.war-on-ice.com/goalieseason.html?woiid=",
            woiid,"\" target=\"_blank\">",player,"</a>")



teamtime.url.raw <- function (team)
    paste0 ("http://www.war-on-ice.com/teambygame.html?team=",team)

playerseason.url.raw <- function (player)
    paste0 ("http://www.war-on-ice.com/playerseason.html?name1=",
            gsub("[ \\.]","0",player))
    
goalieseason.url.raw <- function (player)
    paste0 ("http://www.war-on-ice.com/goalieseason.html?name1=",
            gsub("[ \\.]","0",player))



nhlgame.url <- function (date, seasongcode) 
    paste0 ("<a href=\"http://www.nhl.com/gamecenter/en/boxscore?id=",
            seasongcode, "\" target=\"_blank\">", date,"</a>")

woigame.url <- function (date, seasongcode, sub=NULL) 
    paste0 ("<a href=\"http://war-on-ice.com/game",sub,".html?seasongcode=",
            seasongcode, "\" target=\"_blank\">", date,"</a>")



playersalary.url <- function (woiid, Name)
    paste0 ('<a href="http://www.war-on-ice.com/cap/',woiid,'.html" target="_blank">',Name,'</a>')

teamsalary.url <- function (Team, Name)
    paste0 ('<a href="http://www.war-on-ice.com/cap/',Team,'.html" target="_blank">',Name,'</a>')    








## GET commands

split.GET.to.frame <- function (instring) {
    #instring="team=L.A&secondteam=&yaxis="
    instring <- gsub("\\?", "", instring)
    if (length(instring) == 1) {
        if (grepl("=", instring)) {
            m1 <- sapply(unlist(strsplit(instring, "&")), function (arg) unlist({
                splitsies <- strsplit(arg, "=")
                if (length(splitsies[[1]]) == 1) splitsies <- c(splitsies,"")
                splitsies
            }))
            m2 <- as.list(m1[2,]); names(m2) <- m1[1,]; return(m2)
        } else return(list())
    } else return(list())
}

## Currency out.
currform <- function (input) {
    input[is.na(input)] <- 0
    output <- input
    output[input == 0] <- "--"
    output[input > 0] <- paste("$",
                               formatC(input[input>0], format = "f", big.mark = ",", digits=0, decimal.mark=""),
                               sep="")
    output[input < 0] <- paste("-$",
                               formatC(-input[input<0], format = "f", big.mark = ",", digits=0, decimal.mark=""),
                               sep="")
    output
}
currform.twoway <- function (input) {
    input[is.na(input)] <- 0
    output <- input
    output[input == 0] <- "--"
    output[input > 0] <- paste("$",
                               formatC(input[input>0], format = "f", big.mark = ",", digits=0, decimal.mark=""),
                               sep="")
    output[input < 0] <- paste("<div class=\"twoway\">$",
                               formatC(-input[input<0], format = "f", big.mark = ",", digits=0, decimal.mark=""),
                               "</div>",
                               sep="")
    output
}
