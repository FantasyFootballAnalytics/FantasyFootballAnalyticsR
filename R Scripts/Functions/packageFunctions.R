###########################
# File: packageFunctions.R
# Description: Helper functions for the data scrapes.
# Date: 2/23/2015
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# To do:
###########################
source(paste(getwd(),"/R Scripts/Functions/mySqlDBFunctions.R", sep=""))

## Function to retrieve html data from footballguys.com
fbgUrl <- function(inpUrl, userName, password){
  # Validating input
  if(length(userName) == 0 | length(password) == 0){
    stop("Please specify your Footballguys.com User Name and password.", call. = FALSE)
  }
  
  if(missing(userName) | missing(password) | nchar(userName) == 0 | nchar(password) == 0){
    stop("Please specify your Footballguys.com User Name and password.", call. = FALSE)
  }
  
  if(lenght(inpUrl) == 0){
    stop("URL not specified", call. = FALSE)
  }
  
  if(missing(inpUrl) | nchar(inpUrl) == 0){
    stop("URL not specified", call. = FALSE)
  }
  
  if(length(grep("footballguys", inpUrl)) == 0){
    stop("URL is not a footballguys.com URL", call. = FALSE)
  }
  
  ## Submitting input to retrieve data
  dataPge <- POST(
    handle = handle("http://subscribers.footballguys.com"),
    path = "amember/login.php",
    body = list(amember_login = userName,
                amember_pass = password,
                amember_redirect_url = inpUrl)
  )
  return(content(dataPge))
}


## Function to retrieve data from a website table either as html, csv or xml data
## Returns data.table with data
retrieveData <- function(inpUrl, columnTypes, columnNames, removeRow = integer(), 
                         whichTable, dataType = "html", playerLinkString = "", userName, password){
  
  is.fbg <- length(grep("footballguys", tolower(inpUrl))) > 0 
  is.fft <- length(grep("fftoday", tolower(inpUrl))) > 0 
  
  nameColumn <- which(columnNames == "player")
  
  if(is.fbg){
    inpUrl <- fbgUrl(inpUrl, userName, password)
  }
  
  dataTable <- switch(dataType, 
                      "html" = readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = removeRow, 
                                             colClasses = columnTypes, which = whichTable),
                      "csv" = read.csv(inpUrl),
                      "xml" = t(xpathSApply(xmlParse(inpUrl), "//Player", fun = xmlToList)) # This only works for fantasyfootballnerd
  )
  
  if(length(dataTable) == 0 | is.null(dataTable) ){
    warning(paste("Empty data table retrieved from\n", inpUrl), call. = FALSE)
    return(data.table())
  }
  
  if(nrow(dataTable) == 0 ){
    warning(paste("Empty data table retrieved from\n", inpUrl), call. = FALSE)
    return(dataTable)
  }
  ## On CBS.com the last rows of the table includes links to additional pages, so we remove those:
  if(length(grep("Pages:", dataTable[,nameColumn], fixed = TRUE))>0){
    dataTable <- dataTable[-grep("Pages:", dataTable[,nameColumn], fixed = TRUE),]
  }
  
  ## Setting names on the columns
  dataTable <- data.table(dataTable)
  if(length(columnNames) != length(dataTable)){
    print(paste("Mismatch in columns from \\n", inpUrl))
    print(paste("Table columns:", paste(names(dataTable), collapse = ", ")))
    print(paste("Expected columns:", paste(columnNames, collapse =", ")))
  }
  
  colCount <- min(length(columnNames), length(dataTable))
  setnames(dataTable, 1:colCount, columnNames[1:colCount])
  # Cleaning up player names
  dataTable[, player := getPlayerName(getPlayerName(getPlayerName(player)))]
  
  ## Finding playerId for the sources that has that
  pgeLinks <- getHTMLLinks(inpUrl)
  playerId <- NA
  
  if(is.fbg){
    playerId <- gsub("../players/player-all-info.php?id=","",pgeLinks[grep("player-all-info.php?", pgeLinks)], fixed = TRUE)
  } else if(is.fft){
    playerId <- as.numeric(unique(gsub("[^0-9]","",  gsub("LeagueID=[0-9]{1,6}", "", pgeLinks[grep("/stats/players/", pgeLinks)]))))
  } else if(nchar(playerLinkString) >0){
    playerId <- as.numeric(unique(gsub("[^0-9]", "", pgeLinks[grep(playerLinkString, pgeLinks)])))
  }
  
  
  if(length(playerId) > 1 & length(playerId) != nrow(dataTable)){
    warning(paste("Number of playerIds doesn't match number of players for \n", inpUrl))
  }
  
  if(length(playerId) == nrow(dataTable)){
    dataTable <- data.table(playerId, dataTable)
  }
  
  return(dataTable)
}



# The url list generates a list of urls for each site and position based on the configuatko files
urlList <- function(siteRow, weekNo, season){
  retValue = data.table()  
  for(pg in seq(from= as.numeric(siteRow["startPage"]), to= as.numeric(siteRow["endPage"]), by = as.numeric(siteRow["stepPage"]))){
    tmpUrl <- siteRow["siteUrl"]
    replPar <- c("{$WeekNo}", "{$PgeID}", "{$Season}")
    replVal <- c(weekNo, pg, season)
    for(i in 1:length(replPar)){
      tmpUrl <- gsub(replPar[i], as.character(replVal[i]), tmpUrl, fixed = TRUE)
    }
    retData <- as.data.frame(t(siteRow))
    retValue <- rbind.fill(retValue, data.table(retData[, c("siteTableId", "analystId", "positionId", "urlData", "urlPeriod", "whichTable", "playerLinkString")] , siteUrl = tmpUrl))
  }
  return(retValue)
}

# scrapeUrl calls readUrl to retrieve data and then cleans it up
scrapeUrl <- function(urlData, siteCredentials = list()) {
  if(!exists("siteDb")){
    siteDb <- connectDb("projectionsites")
  }
  
  if(!dbIsValid(siteDb)){
    siteDb <- connectDb("projectionsites")
  }
  
  table <- as.numeric(urlData["siteTableId"])
  analyst <- as.numeric(urlData["analystId"])
  period <- urlData["urlPeriod"]
  posId <- as.numeric(urlData["positionId"])
  
  tblColumns <- dbGetQuery(siteDb, paste("SELECT columnName, columnType, removeColumn from tablecolumns join datacolumns ", 
                                         "on tablecolumns.dataColID = datacolumns.dataColId ", 
                                         "where siteTableID = ", table , " ", 
                                         "and columnPeriod = '", period, "' ",
                                         "ORDER BY columnOrder;", sep = ""))
  if(nrow(tblColumns) == 0){
    return(data.table())
  }
  site <- dbGetQuery(siteDb, paste("SELECT siteId FROM analysts WHERE analystId =", analyst))$siteId
  siteInfo <- dbGetQuery(siteDb, paste("SELECT siteName, subscription, playerIdCol FROM sites WHERE siteId =", site))
  subscription <- as.logical(siteInfo$subscription)
  siteName <- tolower(siteInfo$siteName)
  rows2remove <- dbGetQuery(siteDb, paste("SELECT rowRemove from tablerowremove where sitetableId =", table))$rowRemove
  if(length(rows2remove) == 0){
    rows2remove <- integer()
  }
  
  un <- as.character()
  pw <- as.character()
  
  if(subscription){
    un <- siteCredentials[[siteName]][["user"]]
    pw <- siteCredentials[[siteName]][["pwd"]]
  }
  
  dataTable <- retrieveData(inpUrl = urlData[["siteUrl"]],
                            columnTypes = tblColumns$columnType,
                            columnNames = tblColumns$columnName,
                            removeRow = rows2remove,
                            whichTable = as.numeric(urlData["whichTable"]),
                            dataType = urlData["urlData"],
                            playerLinkString = urlData["playerLinkString"],
                            userName = un, 
                            password = pw) 
  if(nrow(dataTable) == 0){
    return(data.table())
  }
  idCol <- siteInfo$playerIdCol
  
  if(!exists("playerDb")){
    playerDb <- connectDb("ffplayerinfo")
  }
  
  if(!dbIsValid(playerDb)){
    playerDb <- connectDb("ffplayerinfo")
  }
  
  posMap <- dbGetQuery(playerDb, "select positionCode, detailPosition from positionmap;")
  posName <- dbGetQuery(playerDb, paste("SELECT positionCode from playerpositions where positionId =", as.numeric(urlData["positionId"])))$positionCode
  
  players <- data.table(dbReadTable(playerDb, "players"))
  
  # Merging with player data from NFL.com to find the playerId.
  detailPos <- posMap[posMap$positionCode == posName, "detailPosition"]
  
  if(siteInfo$siteName != "NFL"){
    if(idCol %in% names(players) & posId != 6 & "playerId" %in% names(dataTable) ){
      setnames(dataTable, "playerId", idCol)
      dataTable <- merge(dataTable, players[, c("playerId", idCol), with = FALSE], by=idCol)
      dataTable[, c(idCol) := NULL]
    }else{
      dataTable$playerId <- NULL
      dataTable <- merge(dataTable, players[position %in% detailPos, c("playerId", "player"), with = FALSE], by= "player")
    }
  }
  
  #Separate pass completions from attempts
  if(exists("passCompAtt", dataTable)){
    dataTable[, passComp := str_sub(string=passCompAtt, end=str_locate(string=passCompAtt, '/')[,1]-1)]
    dataTable[, passAtt := str_sub(string=passCompAtt, start=str_locate(string=passCompAtt, '/')[,1]+1)]
  }
  
  if(exists("dstBlkPunt", dataTable)){
    dataTable[, dstBlk := dstBlkFg + dstBlkPunt + dstBlkPAT] 
  }
  
  # Putting punt and kick return TDs under return TDs
  if(exists("dstPuntRetTds", dataTable)){
    dataTable[, dstRetTd := dstPuntRetTds + dstKickRetTds]
  }
  
  if(exists("dstPassYdsPerGm", dataTable)){
    dataTable[, dstYdsAllowed := (dstPassYdsPerGm + dstRushYdsPerGm)*16]
  }
  
  if(exists("dstPtsPerGm", dataTable)){
    dataTable[, c("dstPtsAllowed", "dstYdsAllowed") := list(dstPtsPerGm*16, dstYdsPerGm*16)]
  }

  
  dataTable[, analystId := analyst]
  removeCols <- tblColumns$columnName[as.numeric(tblColumns$removeColumn) == 1]
  if(length(removeCols) >0 ){
    dataTable <- dataTable[, !removeCols, with = FALSE]
  }
  dbDisconnect(playerDb)
  dbDisconnect(siteDb)
  
  return(dataTable)  
}

# getPlayerName - cleans the player column for projection data.
getPlayerName <- function(playerCol){
  if(!exists("playerDb")){
    playerDb <- connectDb("ffplayerinfo")
  }
  
  if(!dbIsValid(playerDb)){
    playerDb <- connectDb("ffplayerinfo")
  }
  
  nameCorrect <- dbReadTable(playerDb, "namecorrections")
  teamCorrect <- dbReadTable(playerDb, "nflteams")
  dbDisconnect(playerDb)
  
  playerCol <- gsub("49ers", "Niners", playerCol, fixed = TRUE)
  playerCol <- gsub("New York NYG", "Giants", playerCol, fixed = TRUE)
  playerCol <- gsub("New York NYJ", "Jets", playerCol, fixed = TRUE)
  playerCol <- gsub("New York.+\\(NYG", "Giants", playerCol)
  playerCol <- gsub("New York.+\\(NYJ", "Jets", playerCol)
  playerCol <- gsub("New York Giants", "Giants", playerCol)
  playerCol <- gsub("New York Jets", "Jets", playerCol)
  playerCol <- gsub("New England Patriots", "Patriots", playerCol)
  playerCol <- gsub("New England", "Patriots", playerCol)
  playerCol <- gsub("New Orleans Saints", "Saints", playerCol)
  playerCol <- gsub("New Orleans", "Saints", playerCol)
  
  playerCol <- gsub("Questionable|Probable|Injured Reserve|Out|SSPD|Final|View|Videos|News|Video|(N|n)ote|(N|n)otes|(P|p)layer|^No new|New ", "", playerCol)
  
  playerCol <- gsub("(B(AL|al)|B(UF|uf)|C(HI|hi)|C(IN|in)|C(LE|le)|D(AL|al)|D(EN|en)|D(ET|et)|GB|H(OU|ou)|I(ND|nd)|J(AC|ac)|J(AX|ax)|KC|K(AN|an)|NO|O(AK|ak)|P(IT|it)|P(HI|hi)|NYG|NYJ|NE|S(EA|ea)|A(TL|tl)|A(RI|ri)|M(IA|ia)|SD|S(T|t)(L|l)|C(AR|ar)|SF|T(EN|en)|W(AS|as)|TB|M(IN|in)|W(SH|sh)) ", "", playerCol)
  playerCol <- gsub(",\\s(B(AL|al)|B(UF|uf)|C(HI|hi)|C(IN|in)|C(LE|le)|D(AL|al)|D(EN|en)|D(ET|et)|GB|H(OU|ou)|I(ND|nd)|J(AC|ac)|J(AX|ax)|KC|K(AN|an)|NO|O(AK|ak)|P(IT|it)|P(HI|hi)|NYG|NYJ|NE|S(EA|ea)|A(TL|tl)|A(RI|ri)|M(IA|ia)|SD|S(T|t)(L|l)|C(AR|ar)|SF|T(EN|en)|W(AS|as)|TB|M(IN|in)|W(SH|sh))", "", playerCol)
  playerCol <- gsub("(B(AL|al)|B(UF|uf)|C(HI|hi)|C(IN|in)|C(LE|le)|D(AL|al)|D(EN|en)|D(ET|et)|GB|H(OU|ou)|I(ND|nd)|J(AC|ac)|J(AX|ax)|KC|K(AN|an)|NO|O(AK|ak)|P(IT|it)|P(HI|hi)|NYG|NYJ|NE|S(EA|ea)|A(TL|tl)|A(RI|ri)|M(IA|ia)|SD|S(T|t)(L|l)|C(AR|ar)|SF|T(EN|en)|W(AS|as)|TB|M(IN|in)|W(SH|sh))$", "", playerCol)
  playerCol <- gsub("BAL|BUF|CHI|CIN|CLE|DAL|DEN|DET|GB|HOU|IND|JAC|JAX|KC|KAN|NO|OAK|PIT|PHI|NYG|NYJ|NE|SEA|ATL|ARI|MIA|SD|STL|CAR|SF|TEN|WAS|TB|MIN|WSH", "", playerCol)
  
  playerCol <- gsub("\\s+((P|Q|O|D|S)$|IR|EXE|SUS|PUP|DNP|LP)|\\s(P|Q|O|D|S)\\s|^\\[(Q|P|O|D|S)\\]\\s|(P|Q|O|D|S|IR)$", "", playerCol)
  playerCol <- gsub(" Jr.| Sr.| Jr| Sr| III", "", playerCol)
  playerCol <- gsub("\\sat|\\svs.","", playerCol)
  playerCol <- gsub("[^a-zA-Z \\.\\-]", "", playerCol)
  playerCol <- gsub("Niners", "49ers", playerCol, fixed = TRUE)
  playerCol <- gsub(" {2,99}", "", playerCol)
  playerCol <- gsub("vs$", "", playerCol)
  playerCol <- gsub("(W|L)$", "", playerCol)
  
  playerCol <- gsub("RBTE$|RBWR$|TERB$|WRRB$|WRTE$|TEWR$|QBRB$|RBQB$|QBWR$|WRQB$|TEQB$|QBTE$|QB$|RB$|WR$|TE$|K$|DEF$|DST$|FA$| FA|DST D", "", playerCol)
  playerCol <- gsub("^\\s+|\\s$", "", playerCol)
  
  playerCol <- gsub("\\-$", "", playerCol)
  playerCol <- gsub(" - DEF(W|L)$", "", playerCol)
  for(n in 1:nrow(nameCorrect)){
    playerCol[playerCol == nameCorrect[n,]$nameFrom] <- nameCorrect[n,]$nameTo
  }
  
  for(n in 1:nrow(teamCorrect)){
    
    playerCol[playerCol == teamCorrect[n,]$teamArea] <- teamCorrect[n,]$teamName
    playerCol[playerCol == paste(teamCorrect[n,]$teamArea, teamCorrect[n,]$teamName)] <- teamCorrect[n,]$teamName
  }
  rm(nameCorrect, teamCorrect)
  return(playerCol)
}

createUrls <- function(weekNo = 0, season, analystIds = NULL){
  if(!exists("siteDb")){
    siteDb <- connectDb("projectionsites")
  }
  
  if(!dbIsValid(siteDb)){
    siteDb <- connectDb("projectionsites")
  }
  
  periodSelect <- ifelse(weekNo == 0, "season", "week")

  if(length(analystIds) == 0){
    qryString <- paste("SELECT * FROM urlinfo where urlPeriod = '", periodSelect, "';", sep = "")
  }  
  else {
    qryString <- paste("SELECT * FROM urlinfo where urlPeriod = '", periodSelect, "' and analystId in (", paste(analystIds, collapse = ", "),   ");", sep = "")
  }
  urlTable <- dbGetQuery(siteDb, qryString)
  
  dbDisconnect(siteDb)
  
  siteUrls <- rbindlist(apply(urlTable,1, urlList, weekNo, season))
  return(siteUrls)
}

showAnalysts <- function(){
  if(!exists("siteDb")){
    siteDb <- connectDb("projectionsites")
  }
  
  if(!dbIsValid(siteDb)){
    siteDb <- connectDb("projectionsites")
  }
  
  names <- dbGetQuery(siteDb, "SELECT analystName from analysts")$analystName
  dbDisconnect(siteDb)
  return(names)
}

selectAnalysts <- function(analystNames){
  if(!exists("siteDb")){
    siteDb <- connectDb("projectionsites")
  }
  
  if(!dbIsValid(siteDb)){
    siteDb <- connectDb("projectionsites")
  }
  analystNames <- paste("'", analystNames, "'", sep = "")
  
  qryString <- paste("SELECT analystId from analysts where analystName in (", paste(analystNames, collapse = ", "), ");")
  ids <- dbGetQuery(siteDb, qryString)$analystId
  dbDisconnect(siteDb)
  if(length(ids) != length(analystNames)){
    warning("Some analysts not found", call. = FALSE)
  }
  return(ids)
}