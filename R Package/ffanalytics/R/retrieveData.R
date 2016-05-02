#' Function to retrieve html data from footballguys.com
#'
#' This function takes the URL for a footballguys.com page behind the paywall
#' along with user name and password for an active account to retrieve data
#' @param inpUrl The URL to retrieve. Could be behind paywall
#' @param userName User name for an active footballguys.com account
#' @param password The password associated with an active footballguys.com account
#' @noRd
#' @import httr
fbgUrl <- function(inpUrl, userName, password){
  # Validating input
  if(length(userName) == 0 | length(password) == 0){
    stop("Please specify your Footballguys.com User Name and password.", call. = FALSE)
  }

  if(missing(userName) | missing(password) | nchar(userName) == 0 | nchar(password) == 0){
    stop("Please specify your Footballguys.com User Name and password.", call. = FALSE)
  }

  if(length(inpUrl) == 0){
    stop("URL not specified", call. = FALSE)
  }

  if(missing(inpUrl) | nchar(inpUrl) == 0){
    stop("URL not specified", call. = FALSE)
  }

  if(length(grep("footballguys", inpUrl)) == 0){
    stop("URL is not a footballguys.com URL", call. = FALSE)
  }

  ## Submitting input to retrieve data
  dataPge <- httr::POST(
    handle = httr::handle("http://subscribers.footballguys.com"),
    path = "amember/login.php",
    body = list(amember_login = userName,
                amember_pass = password,
                amember_redirect_url = inpUrl)
  )
  return(httr::content(dataPge))
}

#' Retrive data from a source table
#'
#' Data can be retrieved from a source table when specified along with a source
#' analyst and a data period. The data scrape will translate the data columns
#' for each source table into a uniform format.
#' @return scrapeData a scrapeData object
#' @param srcTbl A \link{sourceTable} object representing the table to get data from
#' @param srcAnalyst A \link{sourceAnalyst} object representing the analyst
#' to scrape data from
#' @param srcPeriod A \link{dataPeriod} object representing the period to get
#' @param fbgUser User Name for an active footballguys.com account. Needed if
#' data scrape is requested from Footballguys
#' @param fbgPwd Password for an active footballguys.com account. Needed if
#' data scrape is requested from Footballguys
#' @export retrieveData
#' @include dataResult.R
retrieveData <- function(srcTbl, srcPeriod, fbgUser = NULL, fbgPwd = NULL){
  # Check that proper arguments are passed
  if(!is(srcTbl, "sourceTable"))
    stop(cat("Error: srcTbl is not a 'sourceTable' object"), call. = FALSE)


  if(!is(srcPeriod, "dataPeriod"))
    stop(cat("Error: srcPeriod is not a 'sourceTable' object"), call. = FALSE)

  urlAddress <- srcTbl@siteUrl
  tableNum <- srcTbl@urlTable
  dataType <- srcTbl@urlType

  playerLinkString <- srcTbl@playerLink
  if(is.null(playerLinkString))
    playerLinkString <- ""

  if(length(playerLinkString) == 0)
    playerLinkString <- ""

  if(is.na(playerLinkString))
    playerLinkString <- ""

  idVar <- srcTbl@playerId

  if(is.na(idVar))
    idVar <- ""

  if(length(idVar) == 0)
    idVar <- ""

  # Extracting and checking parameters in the URL
  urlParms <- stringr::str_extract_all(urlAddress,
                                       "\\{\\$[:alpha:]+\\}")[[1]]
  if(!all(urlParms %in% urlParameters)){
    stop(paste("Address contains illegal paramaters, please only use:",
               paste(urlParameters, collapse = ", ")))
  }

  # This is special for FantasySharks as they designate the period as
  # a segment.
  if("{$Segment}" %in% urlParms){
    segment <- sharkSegment[[as.character(srcPeriod@season)]] + (srcPeriod@weekNo > 0) * 9
  }



  # Replacing parameters with associated values
  for(p in urlParms[urlParms != "{$PgeID}"]){
    urlAddress <- switch (p,
                          "{$WeekNo}" = gsub(p, srcPeriod@weekNo,
                                             urlAddress, fixed = TRUE),
                          "{$Season}" = gsub(p, srcPeriod@season,
                                             urlAddress, fixed = TRUE),
                          "{$Segment}" = gsub(p, segment,
                                              urlAddress, fixed = TRUE),
                          "{$SrcID}" = gsub(p, srcTbl@sourceId,
                                            urlAddress, fixed = TRUE),
                          "{$YahooLeague}" = gsub(p, yahooLeague,
                                                  urlAddress, fixed = TRUE),
                          "{$FFNKEY}" = gsub(p, ffnAPI,
                                             urlAddress, fixed = TRUE),
                          "{$Pos}"= gsub(p, srcTbl@positionAlias,
                                         urlAddress, fixed = TRUE)
    )
  }

  # Generating urls for each page
  if("{$PgeID}" %in% urlParms){
    pageSeq <- seq(from = as.numeric(srcTbl@startPage),
                   to = as.numeric(srcTbl@endPage),
                   by = as.numeric(srcTbl@stepPage))
    urlAddress <- sapply(pageSeq,
                         function(pg)gsub("{$PgeID}", pg,
                                          urlAddress, fixed = TRUE))
  }

  scrapeTable <- as.numeric(srcTbl@tableId)
  scrapeColumns <- tableColumns[tableId == scrapeTable &
                                  columnPeriod == tolower(periodType(srcPeriod))]

  scrapeColumns <- scrapeColumns[order(columnOrder)]

  removeRows <- tableRowRemove$rowRemove[tableRowRemove$tableId == scrapeTable]



  # Scraping data from the URL
  dataTable <- data.table::rbindlist(
    lapply(urlAddress, function(inpUrl)readUrl(inpUrl,
                                               columnTypes = scrapeColumns$columnType,
                                               columnNames = scrapeColumns$columnName,
                                               whichTable = tableNum,
                                               removeRow = removeRows,
                                               dataType, idVar, srcTbl@playerLink))
    , fill = TRUE)

  # Separate pass completions from attempts
  if(exists("passCompAtt", dataTable)){
    dataTable[, passComp := stringr::str_sub(string = passCompAtt,
                                             end=str_locate(string = passCompAtt, '/')[,1]-1)]
    dataTable[, passAtt :=  stringr::str_sub(string = passCompAtt,
                                             start=str_locate(string = passCompAtt, '/')[,1]+1)]
    dataTable[, passCompAtt := NULL]
  }

  # Calculate pass incompletions and pass completion percentage
  if(exists("passComp", dataTable) & exists("passAtt", dataTable)){
    dataTable[, c("passComp", "passAtt") := list(as.numeric(passComp),
                                                 as.numeric(passAtt))]
    dataTable[!is.na(passAtt) & !is.na(passComp),
              passInc := passAtt - passComp]
    dataTable[!is.na(passAtt) & !is.na(passComp) & passAtt > 0,
              passCompPct := passComp / passAtt * 100]
  }
  if(exists("dstBlkPunt", dataTable)){
    dataTable[, dstBlk := dstBlkFg + dstBlkPunt + dstBlkPAT]
  }

  # Putting punt and kick return TDs under return TDs
  if(exists("dstPuntRetTds", dataTable)){
    dataTable[, dstRetTd := dstPuntRetTds + dstKickRetTds]
  }

  # Combine Kick and Punt Return Yards
  if(exists("kickRetYds", dataTable)){
    dataTable[, returnYds := kickRetYds + puntRetYds]
  }

  # Convert yards allowed per game to total yards allowed
  if(exists("dstPassYdsPerGm", dataTable)){
    dataTable[, dstYdsAllowed := (dstPassYdsPerGm + dstRushYdsPerGm)*16]
  }

  # Convert points per game to total points
  if(exists("dstPtsPerGm", dataTable)){
    dataTable[, c("dstPtsAllow", "dstYdsAllowed") := list(dstPtsPerGm*16, dstYdsPerGm*16)]
  }

  # Adding up Field goals by distance to total field goals
  if(exists("fg0019", dataTable) & exists("fg50", dataTable) & !exists("fg", dataTable)){
    dataTable[, fg := fg0019*!is.na(fg0019) + fg2029*!is.na(fg2029) +
                fg3039*!is.na(fg3039) + fg4049*!is.na(fg4049) +
                fg50*!is.na(fg50)]
  }

  # Calculate field goal attempts if missing
  if(exists("fgMiss", dataTable) & exists("fg", dataTable) & !exists("fgAtt", dataTable)){
    dataTable[, fgAtt := fgMiss + fg]
  }

  if(!exists("fgMiss", dataTable) & exists("fg", dataTable) & exists("fgAtt", dataTable)){
    dataTable[, fgMiss := as.numeric(fgAtt) - as.numeric(fg)]
  }

  dataTable[, analyst := srcTbl@analystId]

  if(idVar != "playerId"){# & exists(ifelse(nchar(idVar) > 0, idVar, "_none_"), dataTable)){

    idTbl <- playerData[position == srcTbl@sourcePosition,c("playerId", "player", "cbsId", "mflId", "yahooId" ), with = FALSE]

    if(length(idVar) > 0 & nchar(idVar) > 0 & srcTbl@sourcePosition != "DST"){
      if(!(idVar %in% names(idTbl))){
        stop(cat(idVar, "is not a column in the player table"), call. = FALSE)
      } else {

        idTbl$cbsId <- as.numeric(idTbl$cbsId)
        idTbl$yahooId <- as.numeric(idTbl$yahooId)
        idTbl$playerId <- as.numeric(idTbl$playerId)

        dataTable <- merge(dataTable, idTbl[, c("playerId", idVar), with = FALSE],
                           by = idVar, all.x = TRUE)
        dataTable <- dataTable[!is.na(playerId)]
        dataTable[, (idVar) := NULL]
      }
    } else {
      # Finding duplicated names in data table and id table
      dupeIdNames <- idTbl$player[duplicated(idTbl$player)]
      dupeNames <- dataTable$player[duplicated(dataTable$player)]

      # First matching with player names that are not duplicated
      if(class(dataTable$player) != "character")
        dataTable[, player := as.character(player)]
      dataTable <- merge(dataTable,
                         idTbl[!(player %in% dupeIdNames), c("player", "playerId"),
                               with = FALSE],
                         by = "player", all.x = TRUE)
      # Removing any playerIds that were assigned to duplicated players in
      # the data table
      dataTable[player %in% dupeNames & !is.na(playerId), playerId := NA]

    }
  }
  if(exists("playerId", dataTable)){
    dataTable <- dataTable[!is.na(playerId)]
    if(exists("player", dataTable))
      dataTable[, player := NULL]
    playerData$playerId <- as.numeric(playerData$playerId)
    dataTable$playerId <- as.numeric(dataTable$playerId)
    dataTable <- merge(dataTable, playerData[, c("playerId", "player"), with = FALSE], by = "playerId")
  } else {

    dataTable <- dataTable[0]
  }
  return(dataResult(resultData = dataTable, position = srcTbl@sourcePosition))
}



