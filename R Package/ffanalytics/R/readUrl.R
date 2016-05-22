#' Read data from a URL
#'
#' The task for this function is to read the data from the URL location and assign
#' appropriate column names. The function will throw a warning if there are
#' more columns in the data table than expected. If there are fewer columns than
#' expected the function will retry up to 10 times to get the number of columns
#' correct. If it fails after 10 tries then an error will be thrown.
#' @param inpUrl The URL to get data from
#' @param columnTypes A character vector describing the types of columns in the
#' data. Note: \code{length(columnTypes) == length(columnNames)}.
#' @param columnNames A character vector describing the names of the columns in
#' the data. Note: \code{length(columnTypes) == length(columnNames)}.
#' @param whichTable A number or character describing the table to get. This
#' can be leveraged for HTML tables and spreadsheet files.
#' @param removeRow A numeric vector indicating rows to skip at the top of the data.
#' For exampe \code{c(1,2)} will skip the first two rows of data.
#' @param dataType A character indicating the type of data (HTML, XML, file, xls)
#' @return Returns a \link{data.table} with data from URL.
#' @export readUrl
readUrl <- function(inpUrl, columnTypes, columnNames, whichTable, removeRow,
                    dataType, idVar, playerLinkString){
  if(length(columnNames) > 0){
    srcData <- data.table::data.table(t(rep(NA, length(columnNames))))[0]
    data.table::setnames(srcData, columnNames)
  } else {
    srcData <- data.table::data.table()
  }
  emptyData <- srcData
  # Determining the site that the urlAddress belongs to
  urlSite <- websites[sapply(websites,
                             function(ws)(length(grep(ws, tolower(inpUrl),
                                                      fixed = TRUE)) >0))]
  if(urlSite == "rtsports"){
    rtColTypes <- columnTypes
    columnTypes <- rep("character", length(rtColTypes))
  }

  if(urlSite == "footballguys"){
    inpUrl <- fbgUrl(inpUrl, fbgUser, fbgPwd)
  }
  print(urlSite)
  if(urlSite == "fantasypros")
    inpUrl <- RCurl::getURL(inpUrl)

  if(length(removeRow) == 0){
    removeRow <- NULL
  }
  if(dataType == "file" & urlSite != "walterfootball"){
    whichTable <- 1
  }
  if(urlSite == "walterfootball"){
    whichTable <- names(position.Id)[which(position.Id == as.numeric(whichTable))]
  }
  read.try = 0
  stRow <- 1
  if(urlSite == "numberfire"){
    stRow <- 2
  }
  # Will try up to 10 times to get data from the source
  while(read.try <= 10 ){
    srcData <-tryCatch(
      switch(dataType,
             "html" = XML::readHTMLTable(inpUrl, stringsAsFactors = FALSE,
                                         skip.rows = removeRow,
                                         colClasses = columnTypes,
                                         which = as.numeric(whichTable)),
             "csv" = read.csv(inpUrl, stringsAsFactors = FALSE),
             # This only works for fantasyfootballnerd
             "xml" = scrapeXMLdata(inpUrl),
             "xls" = XLConnect::readWorksheetFromFile(file = dataFile,
                                                      sheet = whichTable
                                                      , header = TRUE),
             "file" = XLConnect::readWorksheetFromFile(file = inpUrl,
                                                       sheet = whichTable,
                                                       header = TRUE,
                                                       startRow = stRow),
             "json" = httr::content(httr::GET(inpUrl))
      ),
      error = function(e)data.table::data.table())
    if(dataType != "json")
      srcData <- data.table::data.table(srcData)
    if((length(srcData) > 1 &  length(columnNames) <= length(srcData)) | dataType == "json")
      break
    read.try = read.try + 1
  }


  if(dataType == "json" & urlSite == "nfl"){

    nflProjection <- srcData$players
    srcData <- data.table::rbindlist(lapply(nflProjection, function(np){
      statTable <- data.table::as.data.table(lapply(np$stats, as.numeric))
      names(statTable) <- nflstats$ffanalytics[nflstats$id %in% names(statTable)]
      dt <- data.table::as.data.table(t(np))
      dt[, stats := NULL]
      data.table::setnames(dt, c("id", "name", "teamAbbr"), c("playerId", "player", "team"))
      dt <- dt[, c("playerId", "player", "team"), with = FALSE]
      dt[, c("playerId", "player", "team") := list(as.numeric(playerId), as.character(player),
                                                   as.character(team))]
      return(cbind(dt, statTable))

    }
    ), fill = TRUE)
    if(length(srcData) == 0 | is.null(srcData) ){
      warning(cat("Empty data table retrieved from\n", inpUrl, "\n"),
              call. = FALSE)
      return(emptyData)
    }

    if(nrow(srcData) == 0 ){
      warning(cat("Empty data table retrieved from\n", inpUrl, "\n"),
              call. = FALSE)
      return(emptyData)
    }
  } else {
    # Chekcing for matches between number of columns in dataTable and
    # number of columns specified by names
    if(length(srcData) > 1 & (length(columnNames) > length(srcData)) & read.try > 10){
      warning(cat("Got fewer columns in data than number of columns",
                  "specified by names from:\n", inpUrl,
                  "\nNOTE: Returning empty dataset"))
      return(emptyData)
    }

    # Checking for empty dataTables
    if(length(srcData) == 0 | is.null(srcData) ){
      warning(cat("Empty data table retrieved from\n", inpUrl, "\n"),
              call. = FALSE)
      return(emptyData)
    }

    if(nrow(srcData) == 0 ){
      warning(cat("Empty data table retrieved from\n", inpUrl, "\n"),
              call. = FALSE)
      return(emptyData)
    }

    if(length(srcData) > 1 & (length(columnNames) < length(srcData))){
      warning(cat("Got more columns in data than number of columns",
                  "specified by names from\n", inpUrl,
                  "\nRemoving columns after column", length(columnNames)))
      srcData <- srcData[, seq_along(columnNames), with = FALSE]
    }

    data.table::setnames(srcData, columnNames)
  }
  # On CBS.com the last rows of the table includes links to
  # additional pages, so we remove those:
  if(length(grep("Pages:", srcData$player, fixed = TRUE))>0){
    srcData <- srcData[-grep("Pages:", srcData$player, fixed = TRUE),]
  }

  if(urlSite == "rtsports"){
    srcData <- data.table::data.table(
      sapply(srcData, function(x)gsub("[^A-Za-z0-9,. ]", "", x) ))
  }

  if(exists("position", srcData)){
    srcData[position %in% c("Def", "D", "DEF"), position := "DST"]
    srcData[position == "PK", position := "K"]
    srcData[position %in% c("CB", "S"), position := "DB"]
    srcData[position %in% c("DE", "DT"), position := "DL"]
  }

  if(exists("firstName", srcData)){
    srcData[, player := paste(firstName, lastName)]
  }

  if(urlSite == "yahoo"){
    srcData[, player := gsub("Sun 10:00 am|Sun 5:30 pm|Mon 4:10 pm|Sun 1:25 pm|Thu 5:30 pm|Mon 7:20 pm|Sun 1:05 pm", "", player)]
  }

  if(urlSite == "fantasysharks"){
    srcData[, player := firstLast(player)]
  }


  srcData[, player := getPlayerName(getPlayerName(getPlayerName(player)))]

  ## Finding playerId for the sources that has that
  if(dataType == "html" & length(idVar) > 0){
    pgeLinks <- XML::getHTMLLinks(inpUrl)
    playerId <- NA
    switch (urlSite,
            "footballguys"= {
              playerId <- gsub("../players/player-all-info.php?id=","",
                               pgeLinks[grep("player-all-info.php?",
                                             pgeLinks)], fixed = TRUE)
            } ,
            "fftoday" = {
              playerId <- as.numeric(unique(gsub("[^0-9]","",
                                                 gsub("LeagueID=[0-9]{1,6}",
                                                      "",
                                                      pgeLinks[grep("/stats/players/",
                                                                    pgeLinks)]))))
            } ,
            {
              playerId <- as.numeric(unique(gsub("[^0-9]", "",
                                                 pgeLinks[grep(playerLinkString,
                                                               pgeLinks)])))
            }
    )

    if(length(playerId) > 1 & length(playerId) != nrow(srcData)){
      warning(paste("Number of playerIds doesn't match number of",
                    "players for \n", inpUrl))
    }

    if(length(playerId) == nrow(srcData)){
      srcData[, (idVar) := as.numeric(playerId)]
    }
  }

  srcData <- srcData[, lapply(.SD, function(col){
    if(class(col) == "factor")
      col <- as.character(col)
    return(as.numeric(col))
  }), by = intersect(names(srcData), c("playerId", "player"))]
  return(srcData)
}
