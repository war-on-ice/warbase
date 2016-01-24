
## library(warbase)

make.short.gametable.orig <- function (todayfile="common-data/today-games.RData", outfile="gamestoday.html") {

    primer <- "<!doctype html>
<html>
  <head>
    <meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\">
    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
    
    <title></title>
    
    <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.war-on-ice.com/www2/site2.css?_=d6a7bb39f25b4e29106307c90f6bcd3c\">     
    <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.war-on-ice.com/www2/dataTables.responsive.css\">
    
    <script type=\"text/javascript\" src=\"http://www.war-on-ice.com/www2/site.js?_=a6f2d4070a85695086408867f18cb5a4\"></script>
    <script type=\"text/javascript\" src=\"http://www.war-on-ice.com/www2/dynamic.php\" async></script>
    <script type=\"text/javascript\" language=\"javascript\" src=\"http://www.war-on-ice.com/www2/dataTables.responsive.min.js\"></script>
    <script type=\"text/javascript\">
      
      $(document).ready( function () {
      $('#example')
      .addClass( 'nowrap' )
      .dataTable( {

      responsive: true,
      pageLength: 15, 
      searching: false,
      info: false,
      lengthChange: false,
      paging: false,
      \"aaSorting\": [],

      columnDefs: [
      { targets: [-1, -2, -3, -4, -5, -6, -7, -8, -9], className: 'dt-body-center' }
      ]
      } );
      
      } );
      
      
    </script>
  </head>
  <body class=\"wide\">  
		

	  <p>
	    <table id=\"example\" class=\"display\" cellspacing=\"0\" width=\"100%\">
"
    
    output <- get(load(todayfile))
    output$status[is.na(output$status)] <- "NotStarted"

    output[["Score"]] <- paste(output[["Away Score"]],"  -  ", output[["Home Score"]])
    output[["Corsi"]] <- paste(output[["awaycorsi"]],"  /  ", output[["homecorsi"]])
    
    sec <- output$seconds;  pd <- output$periods
    secrnd <- paste0((60 - (output$seconds %% 60)) %% 60)
    secrnd[nchar(secrnd)<2] <- paste0("0", secrnd[nchar(secrnd)<2])
    
                                        #if (substr(input$seasongcode,9,9) == 2) {
    sec.gone <- c(0, 1200, 2400,3600,3900)[pmax(pd,1)]
    mintot <- c(20, 20, 20, 5, 0)[pmax(pd,1)]
    if (any(output$Session == "Playoffs")) {
        sec.gone[output$Session == "Playoffs"] <- (pd[output$Session == "Playoffs"]-1)*1200
        mintot[output$Session == "Playoffs"] <- 20
    }
    
    minutes <- floor(mintot - (sec-sec.gone)/60)
    pdprint <- c("1st","2nd","3rd","OT", "2OT", "3OT", "4OT", "5OT", "6OT")[pmax(pd,1)]
    pdprint[output$periods == 5 & substr(output$gcode,1,1) == "2"] <- "SO"
    reporttime <- paste0 (minutes,":",secrnd,", ",pdprint)
    
    output[["Status"]] <- ""
    output[["Status"]][output$status == "NotStarted"] <- "NotStarted"
    output[["Status"]][output$status == "Complete"] <- "Complete"
    output[["Status"]][output$status == "Complete" & output$period >= 4] <- "Complete(OT)"
    output[["Status"]][output$status == "Complete" & output$period == 5 & output$Session == "Regular"] <- "Complete(SO)"
    output[["Status"]][output$status == "InProgress"] <- reporttime[output$status == "InProgress"]
    
    output[["Home Team"]] <- teamtime.url(output$hometeam)
    output[["Away Team"]] <- teamtime.url(output$awayteam)
    inrec <- which(output$status %in% c("InProgress", "Complete"))
    output[["Game Date"]] <- output$GameDate
    output[["Game Date"]][inrec] <- woigame.url(output$GameDate, paste0(output$season, output$gcode), rep(1:5, 3))[inrec]
    ##output[["Game Date"]][!inrec] <- un.url(output[["Game Date"]][!inrec])
    output[["Status"]][inrec] <- woigame.url(output[["Status"]], paste0(output$season, output$gcode), rep(1:5, 3))[inrec]
    ##output[["Status"]][!inrec] <- un.url(output[["Status"]][!inrec])
    
    ##print(head(output))
    output <- output[,c("Game Date", "Session", "Away Team", "Home Team", "Score", "Corsi", "Status", "game.start","game.end")]
    colnames(output)[colnames(output) == "game.start"] <- 'Start Time'
    colnames(output)[colnames(output) == "game.end"] <- 'Finished'
    
    colnames(output)[colnames(output) == "Corsi"] <- '<div title="Shot Attempts By Team, All Situations">Corsi</div>'
    colnames(output)[colnames(output) == "Status"] <- '<div title="Time Remaining In Period">Status</div>'
    colnames(output)[colnames(output) == "Session"] <- '<div title="Regular Season or Playoffs">Session</div>'
    rownames(output) <- NULL
    
    x1 <- c(paste0("<thead><tr><th>", paste(colnames(output), collapse="</th><th>"), "</th></tr></thead>"),
            paste0("<tr><td>", apply(output, 1, paste, collapse="</td><td>"), "</td></tr>"),
            "</tbody></table></p></body></html>")
            
    writeLines(c(primer, x1), outfile)
    
    return(1)
    
}




make.datatable.test <- function (output,
                                  outfile="this.dt.html",
                                  searching=TRUE,
                                  pageLength=30,
                                  paging=TRUE,
                                  roundnum=2
                                  ) {

    num.seq <- paste(-1:-ncol(output), collapse=", ")
    
    primer <- paste0("<!doctype html>
<html>
  <head>
    <meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\">
    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
    
    <title></title>
    
    <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.war-on-ice.com/www2/site2.css?_=d6a7bb39f25b4e29106307c90f6bcd3c\">     
    <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.war-on-ice.com/www2/dataTables.responsive.css\">
    <link rel=\"stylesheet\" href=\"http://war-on-ice.com/css/capcheck.css\">
    <link rel='stylesheet' id='twentytwelve-fonts-css'  href='http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext' type='text/css' media='all' />

    
    <script type=\"text/javascript\" src=\"http://www.war-on-ice.com/www2/site.js?_=a6f2d4070a85695086408867f18cb5a4\"></script>
    <script type=\"text/javascript\" src=\"http://www.war-on-ice.com/www2/dynamic.php\" async></script>
    <script type=\"text/javascript\" language=\"javascript\" src=\"http://www.war-on-ice.com/www2/dataTables.responsive.min.js\"></script>
    <script type=\"text/javascript\">
      
      $(document).ready( function () {

      var table = $('#example').dataTable()
          .columnFilter();
      } );
      
      
    </script>
  </head>
  <body class=\"wide\">  

	  <p>
	    <table id=\"example\" class=\"display\" cellspacing=\"0\" width=\"100%\">
")
    

    if (!is.null(roundnum)) for (cc in 1:ncol(output)) if (is.numeric (output[[cc]])) output[[cc]] <- round(output[[cc]], 2)
    
    x1 <- c(paste0("<thead><tr><th align=\"center\">", paste(colnames(output), collapse="</th><th align=\"center\">"), "</th></tr></thead>"),
            paste0("<tr><td>", apply(output, 1, paste, collapse="</td><td>"), "</td></tr>"),
            
            "</tbody>",
            paste0("<tfoot><tr><th align=\"center\">", paste(colnames(output), collapse="</th><th align=\"center\">"), "</th></tr></tfoot>"),
            "</table></p></body></html>")
            
    writeLines(c(primer, x1), outfile)
    
    return(1)
    
}






make.short.gametable <- function (output, outfile="gamestoday.html") {

    output$status[is.na(output$status)] <- "NotStarted"

    output[["Away Score"]][is.na(output[["Away Score"]])] <- ""
    output[["Home Score"]][is.na(output[["Home Score"]])] <- ""
    output[["awaycorsi"]][is.na(output[["awaycorsi"]])] <- ""
    output[["homecorsi"]][is.na(output[["homecorsi"]])] <- ""
    
    output[["Score"]] <- paste(output[["Away Score"]],"  -  ", output[["Home Score"]])
    output[["Corsi"]] <- paste(output[["awaycorsi"]],"  /  ", output[["homecorsi"]])
    
    sec <- output$seconds;  pd <- output$periods
    secrnd <- paste0((60 - (output$seconds %% 60)) %% 60)
    secrnd[nchar(secrnd)<2] <- paste0("0", secrnd[nchar(secrnd)<2])
    
                                        #if (substr(input$seasongcode,9,9) == 2) {
    sec.gone <- c(0, 1200, 2400,3600,3900)[pmax(pd,1)]
    mintot <- c(20, 20, 20, 5, 0)[pmax(pd,1)]
    if (any(output$Session == "Playoffs")) {
        sec.gone[output$Session == "Playoffs"] <- (pd[output$Session == "Playoffs"]-1)*1200
        mintot[output$Session == "Playoffs"] <- 20
    }
    
    minutes <- floor(mintot - (sec-sec.gone)/60)
    pdprint <- c("1st","2nd","3rd","OT", "2OT", "3OT", "4OT", "5OT", "6OT")[pmax(pd,1)]
    pdprint[output$periods == 5 & substr(output$gcode,1,1) == "2"] <- "SO"
    reporttime <- paste0 (minutes,":",secrnd,", ",pdprint)
    
    output[["Status"]] <- ""
    output[["Status"]][output$status == "NotStarted"] <- "NotStarted"
    output[["Status"]][output$status == "Complete"] <- "Complete"
    output[["Status"]][output$status == "Complete" & output$period >= 4] <- "Complete(OT)"
    output[["Status"]][output$status == "Complete" & output$period == 5 & output$Session == "Regular"] <- "Complete(SO)"
    output[["Status"]][output$status == "InProgress"] <- reporttime[output$status == "InProgress"]
    

    b2btext <- '<div style="float:left" title="Second Half of Back-To-Back"><img height=7 src="http://war-on-ice.com/images/B2B.png"></div>'
    b2empty <- '<div style="float:left" title="Rested"><img height=7 src="http://war-on-ice.com/images/B2blank.png"></div>'
    output[["AwayB2B"]] <- b2empty; output[["AwayB2B"]][which(output$awayafteraway + output$awayafterhome > 0)] <- b2btext
    output[["HomeB2B"]] <- b2empty; output[["HomeB2B"]][which(output$homeafteraway + output$homeafterhome > 0)] <- b2btext

    output[["Home"]] <- paste0 (output[["HomeB2B"]], '  <img style="float:left" height=17 width=17 src="http://war-on-ice.com/images/logo/',tolower(output$hometeam),'.png">', '<div style="float:left">',teamtime.url(output$hometeam), '</div>')
    output[["Away"]] <- paste0 (output[["AwayB2B"]], '  <img style="float:left" height=17 width=17 src="http://war-on-ice.com/images/logo/',tolower(output$awayteam),'.png">', '<div style="float:left">',teamtime.url(output$awayteam), '</div>')

    
    inrec <- which(output$status %in% c("InProgress", "Complete"))
    output[["Game Date"]] <- output$GameDate
    output[["Game Date"]][inrec] <- woigame.url(output$GameDate, paste0(output$season, output$gcode), rep(1:5, 3))[inrec]
    ##output[["Game Date"]][!inrec] <- un.url(output[["Game Date"]][!inrec])
    output[["Status"]][inrec] <- woigame.url(output[["Status"]], paste0(output$season, output$gcode), rep(1:5, 3))[inrec]
    ##output[["Status"]][!inrec] <- un.url(output[["Status"]][!inrec])


    
    ##print(head(output))
    output <- output[,c("Game Date", "Session", "Away", "Home", "Score", "Corsi", "Status", "game.start","game.end")]
    colnames(output)[colnames(output) == "game.start"] <- 'Start Time'
    colnames(output)[colnames(output) == "game.end"] <- 'Finished'
    colnames(output)[colnames(output) %in% c("AwayB2B","HomeB2B")] <- ''
    
    colnames(output)[colnames(output) == "Corsi"] <- '<div title="Shot Attempts By Team, All Situations">Corsi</div>'
    colnames(output)[colnames(output) == "Status"] <- '<div title="Time Remaining In Period">Status</div>'
    colnames(output)[colnames(output) == "Session"] <- '<div title="Regular Season or Playoffs">Session</div>'
    rownames(output) <- NULL
        
    make.datatable.whole (output, outfile, pageLength=15)
    
    return(1)
    
}







