

make.datatable.whole <- function (output,
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

//	$('#example tfoot th').each( function () {
//		var title = $('#example thead th').eq( $(this).index() ).text();
//		$(this).html( '<input type=\"text\" placeholder=\"Search '+title+'\" />' );
//	} );


      var table = $('#example')
        .addClass( 'nowrap' )
        .dataTable( {

          responsive: true,
          pageLength: ",pageLength,", 
          searching: ",tolower(searching),",
          info: false,
          lengthChange: false,
          paging: ",tolower(paging),",
          \"aaSorting\": [],

          columnDefs: [
            { targets: [",num.seq,"], className: 'dt-center' }
          ]

        } );

      // Apply the search
//      table.columns().eq( 0 ).each( function ( colIdx ) {
//	$( 'input', table.column( colIdx ).footer() ).on( 'keyup change', function () {
//		table
//			.column( colIdx )
//			.search( this.value )
//			.draw();
//	} );
  //    } );

      
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
            "</table></p></body></html>")

            
    writeLines(c(primer, x1), outfile)
    
    return(1)
    
}





make.WOI.page <- function (filename,
                           title,
                           appLocation,
                           content='<iframe id="vincerlink" src="" style="border: none; width: 960px; height: 2100px"></iframe>',
                           cap=FALSE,
                           urlquote=TRUE) {

    writeLines(paste0(pagebase1,
                      title,
                      pagebase1a,
                      ##if (cap) "cap" else "",
                      pagebase1b,
                      ifelse (urlquote,"\"",""),
                      appLocation,
                      ifelse (urlquote,"\"",""),
                      pagebase2,
                      content,
                      pagebase3, collapse=""),
               filename)
    return(TRUE)

}



make.datatable.part <- function (output,
                                 searching=TRUE,
                                 pageLength=30,
                                 paging=TRUE,
                                 roundnum=2) {
    #searching=TRUE; pageLength=30; paging=TRUE; roundnum=2
    num.seq <- paste(-1:-ncol(output), collapse=", ")
    tableid <- paste0("woi",round(runif(1,1,1000000)))
    
    js.header <- paste0(" <script type=\"text/javascript\">
      
      $(document).ready( function () {

//	$('#example tfoot th').each( function () {
//		var title = $('#example thead th').eq( $(this).index() ).text();
//		$(this).html( '<input type=\"text\" placeholder=\"Search '+title+'\" />' );
//	} );


      var table = $('#",tableid,"')
        .addClass( 'nowrap' )
        .dataTable( {

          responsive: true,
          pageLength: ",pageLength,", 
          searching: ",tolower(searching),",
          info: false,
          lengthChange: false,
          paging: ",tolower(paging),",
          \"aaSorting\": [],

          columnDefs: [
            { targets: [",num.seq,"], className: 'dt-center' }
          ]

        } );

      } );
      
      
    </script><p><table id=\"",tableid,"\" class=\"display\" cellspacing=\"0\" width=\"100%\">")

    if (!is.null(roundnum)) for (cc in 1:ncol(output)) if (is.numeric (output[[cc]])) output[[cc]] <- round(output[[cc]], 2)
    x1 <- c(paste0("<thead><tr><th align=\"center\">", paste(colnames(output), collapse="</th><th align=\"center\">"), "</th></tr></thead>"),
            paste0("<tr><td>", apply(output, 1, paste, collapse="</td><td>"), "</td></tr>"),
            "</tbody>",
            ##paste0("<tfoot><tr><th align=\"center\">", paste(colnames(output), collapse="</th><th align=\"center\">"), "</th></tr></tfoot>"),
            "</table></p>")

    paste0(js.header, paste0(x1, collapse=""))

}

