###########################
# File: scrapeFunctions.R
# Description: Helper functions for the data scrapes.
# Date: 2/23/2015
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# To do:
###########################

readUrl <- function(inpUrl, dataSrc, colTypes, nameCol , removeRow, fbgUserName, fbgPassword){
  if(dataSrc == "Footballguys"){
    
    dataPge <- POST(
      handle = handle("http://subscribers.footballguys.com"),
      path = "amember/login.php",
      body = list(amember_login = fbgUserName,
                  amember_pass = fbgPassword,
                  amember_redirect_url = inpUrl)
    )
  }
  dataTbl <- data.table()
  
  tryCatch(
  dataTbl <- switch(dataSrc,
                    "CBS"           = readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = removeRow, colClasses = colTypes)[7]$`NULL`,
                    "FOX"           = readHTMLTable(inpUrl, stringsAsFactors = FALSE, colClasses = colTypes)$playerTable,
                    "ESPN"          = readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = removeRow, colClasses = colTypes)$playertable_0,
                    "NFL"           = readHTMLTable(inpUrl, stringsAsFactors = FALSE, colClasses = colTypes)$`NULL`,
                    "FFToday"       = readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = removeRow, colClasses = colTypes)[11]$`NULL`,
                    "Footballguys"  = readHTMLTable(content(dataPge), stringsAsFactors = FALSE, colClasses = colTypes)$`NULL`,
                    "Yahoo"         = readHTMLTable(inpUrl, stringsAsFactors = FALSE, header = FALSE, colClasses = colTypes)[2]$`NULL`,
                    "NumberFire"    = readHTMLTable(inpUrl, stringsAsFactors = FALSE, header = FALSE, colClasses = colTypes)$`complete-projection`,
                    "FantasyPros"   = readHTMLTable(inpUrl, stringsAsFactors = FALSE, colClasses = colTypes)$data
  ),
  error = function(e){
    print(paste("Error retriving data from", dataSrc, inpUrl))
    return(data.table())
  }
  )
  if(is.null(nrow(dataTbl)) | is.null(dataTbl)){
    print(paste("Empty data table from", dataSrc, inpUrl))
    return(data.table())
  }
  if(dataSrc == "CBS"){
    if(length(grep("Pages:", dataTbl[,nameCol], fixed = TRUE))>0){
      dataTbl <- dataTbl[-grep("Pages:", dataTbl[,nameCol], fixed = TRUE),]
    }
  }
  
  if(dataSrc == "Footballguys"){
    pgeLinks <-  getHTMLLinks(content(dataPge))
  }
  else{
    pgeLinks <- getHTMLLinks(inpUrl)
  }
  
  playerId <- switch(dataSrc,
                     "CBS" = unique(gsub("[^0-9]", "", pgeLinks[grep("/fantasyfootball/players/playerpage/[0-9]{3,6}", pgeLinks)])),
                     "FOX" = unique(gsub("[^0-9]", "", pgeLinks[grep("/fantasy/football/commissioner/Players/Profile.aspx", pgeLinks)])),
                     "NFL" = unique(gsub("[^0-9]", "", pgeLinks[grep("playerId=[0-9]{3,7}$", pgeLinks)])),
                     "FFToday" = unique(gsub("[^0-9]","", gsub("LeagueID=[0-9]{1,6}", "", pgeLinks[grep("/stats/players/", pgeLinks)]))),
                     "Yahoo" = unique(gsub("[^0-9]", "", pgeLinks[grep("http://sports.yahoo.com/nfl/players/[0-9]{3,6}", pgeLinks)])),
                     "Footballguys" = gsub("../players/player-all-info.php?id=","",pgeLinks[grep("player-all-info.php?", pgeLinks)], fixed = TRUE)
                     )
  
  if(length(playerId) > 0 & length(dataTbl[,nameCol]) > 0){  
    dataTbl$playerId <- ifelse(is.na(as.numeric(playerId)), playerId, as.numeric(playerId))
  }
  return(dataTbl)  
}

# The url list generates a list of urls for each site and position based on the configuatko files
urlList <- function(siteRow){
  retValue = data.table()  
  for(pg in seq(from= as.numeric(siteRow["startPage"]), to= as.numeric(siteRow["endPage"]), by = as.numeric(siteRow["stepPage"]))){
    tmpUrl <- siteRow["siteUrl"]
    replPar <- c("{$WeekNo}", "{$PgeID}", "{$Season}")
    replVal <- c(weekNo, pg, season)
    for(i in 1:length(replPar)){
      tmpUrl <- gsub(replPar[i], as.character(replVal[i]), tmpUrl, fixed = TRUE)
    }
    retValue <- rbind.fill(retValue, data.table(siteTableId = siteRow["siteTableId"], analystId = siteRow["analystId"], 
                                                urlData = siteRow["urlData"], nameCol = siteRow["nameCol"], siteUrl = tmpUrl))
  }
  return(retValue)
}

# scrapeUrl calls readUrl to retrieve data and then cleans it up
scrapeUrl <- function(x) {
  if(!dbIsValid(siteDb)){
    siteDb <- connectDb("projectionsites")
  }
  
  analystId <- as.numeric(x["analystId"])
  siteId <- analysts[analysts$analystId == analystId, "siteID"]
  dataSrc <- sites[sites$siteID == siteId , "siteName"]
  tblId <- as.numeric(x["siteTableId"])
  urlCols <- tableColumns[tableColumns$siteTableID == tblId,]
  urlCols <- urlCols[with(urlCols, order(columnOrder)),]
  colTypes <- urlCols$columnType
  colNames <- urlCols$columnName
  posId <- siteTables[siteTables$siteTableId == tblId, "positionId"]
  
  rows2Remove <- tableRowRemove[tableRowRemove$siteTableId == tblId, "rowRemove"]

  fbgUserName = ""
  fbgPassword = ""  

  dataTable <- data.table(readUrl(x["siteUrl"], dataSrc, colTypes, nameCol = as.numeric(x["nameCol"]), removeRow = rows2Remove, fbgUserName, fbgPassword)) 
  
  if(length(dataTable) > 0){
    colCount <- min(length(colNames), length(dataTable))
    setnames(dataTable, 1:colCount, colNames[1:colCount])
  }
  else {
    return(data.table())
  }
  
  dataTable[, player := getPlayerName(getPlayerName(getPlayerName(player)))]
  
  idCol <- switch(dataSrc,
                  "Yahoo" = "yahooId",
                  "Footballguys" = "fbgId",
                  "CBS" = "cbsId",
                  "FOX" = "foxId",
                  "FFToday" = "fftId",
                  "ESPN" = "None",
                  "FantasyPros" = "None",
                  "NumberFire" = "None",
                  "NFL" = "nflId"
  )
  
  # Merging with player data from NFL.com to find the playerId.
  nflPos <- c("QB", "RB", "WR", "TE", "K", "DEF")
  
  if(dataSrc != "NFL"){
    if(idCol %in% names(nflPlayers) & posId != 6 & "playerId" %in% names(dataTable) ){
      setnames(dataTable, "playerId", idCol)
      dataTable <- merge(dataTable, nflPlayers[, c("playerId", idCol), with = FALSE], by=idCol)
    }else{
      dataTable$playerId <- NULL
      dataTable <- merge(dataTable, nflPlayers[position == nflPos[as.numeric(posId)], c("playerId", "player"), with = FALSE], by= "player")
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
  dataTable[, analystId := x["projAnalystId"]]
  dataTable[,name := nameMerge(dataTable$player)]
  
  #Remove duplicate cases
  duplicateCases <- dataTable[duplicated(name)]$name
  dataTable[which(name %in% duplicateCases),]
  
  return(dataTable[, intersect(names(dataTable), finalVarNames), with = FALSE])  
}

# getPlayerName - cleans the player column for projection data.
getPlayerName <- function(playerCol){
  nameCorrect <- read.csv(paste(getwd(), "/Data/NameCorrections.csv", sep = ""), stringsAsFactors = FALSE)
  teamCorrect <- read.csv(paste(getwd(), "/Data/NFLTeams.csv", sep =""), stringsAsFactors = FALSE)
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
    playerCol[playerCol == nameCorrect[n,]$NameFrom] <- nameCorrect[n,]$NameTo
  }
  
  for(n in 1:nrow(teamCorrect)){
    playerCol[playerCol == teamCorrect[n,]$TeamArea] <- teamCorrect[n,]$TeamName
    playerCol[playerCol == paste(teamCorrect[n,]$TeamArea, teamCorrect[n,]$TeamName)] <- teamCorrect[n,]$TeamName
  }
  rm(nameCorrect, teamCorrect)
  return(playerCol)
}
