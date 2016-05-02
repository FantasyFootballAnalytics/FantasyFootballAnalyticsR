###########################
# File: scrapefunctions.R
# Description: Helper functions for the data scrapes.
# Date: 2/23/2015
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# This file contains functions that are used for the data scrapes.
# To do:
###########################
require(RCurl)
require(stringr)
require(XML)
require(data.table)
require(plyr)

#source("R Scripts/Functions/sharkSegment.R")

# League ID for the "dummy" Yahoo league that is setup to use for data scrapes
yahooLeague <- 170716

# API Key to access the Fantasy Football Nerd API. Register here http://www.fantasyfootballnerd.com/fantasy-football-api
ffnAPIKey <- "test"

# Load configuration data.
nameCorrect <- data.table::data.table(read.csv("Data/NameCorrections.csv", stringsAsFactors = FALSE))
teamCorrect <- data.table::data.table(read.csv("Data/NFLTeams.csv", stringsAsFactors = FALSE))
allUrls <- data.table::data.table(read.csv("Config/Data/allUrls.csv", stringsAsFactors = FALSE))
dataColumns <- data.table::data.table(read.csv("Config/Data/dataColumns.csv", stringsAsFactors = FALSE))
playerPositions <- data.table::data.table(read.csv("Config/Data/playerPositions.csv", stringsAsFactors = FALSE))
posMap <- data.table::data.table(read.csv("Config/Data/positionMap.csv", stringsAsFactors = FALSE))
siteAnalysts <- data.table::data.table(read.csv("Config/Data/analysts.csv", stringsAsFactors = FALSE))
dataSites <- data.table::data.table(read.csv("Config/Data/dataSites.csv", stringsAsFactors = FALSE))
removeRows <- data.table::data.table(read.csv("Config/Data/rowRemove.csv", stringsAsFactors = FALSE))

# Check if player Data file exists, and if not create it.
if(!file.exists("Config/Data/playerData.csv")){
  source("Config/playerData.R")
  if(month(Sys.time()) <= 2){
    yr <- year(Sys.time()) - 1
    }
  else {
    yr <- year(Sys.time())
  }
  
  players <-  data.table::data.table(getPlayerData(yr))
} else {
  players <- data.table::data.table(read.csv("Config/Data/playerData.csv", stringsAsFactors = FALSE))
}


# getPlayerName - cleans the player column for projection data.
getPlayerName <- function(playerCol){
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
  
  playerCol <- gsub("RBTE$|RBWR$|TERB$|WRRB$|WRTE$|TEWR$|QBRB$|RBQB$|QBWR$|WRQB$|TEQB$|QBTE$|QB$|RB$|WR$|TE$|K$|DEF$|DST$|FA$|DL$|LB$|DB$| FA|DST D", "", playerCol)
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
  playerCol <- gsub("^\\s+|\\s$", "", playerCol)
  
  return(playerCol)
}

# Retrieve ADP Values from Fantasy Football Calculator
getFFCValues <- function(format = "standard", teams = 12){
  ffcFile <- url(paste("https://fantasyfootballcalculator.com/adp_csv.php?format=", format, "&teams=", teams, sep = ""), open = "rt")
  
  ffcTbl <- data.table::data.table(read.csv(ffcFile, skip = 4, stringsAsFactors = FALSE))
  close(ffcFile)
  ffcTbl[, Name := getPlayerName(Name)]
  ffcTbl[Position == "PK", Position := "K"]
  ffcTbl[Position == "DEF", Position := "DST"]
  ffcTbl[, ADP := NULL]
  data.table::setnames(ffcTbl, c("Overall", "Name", "Position"), c("adp", "player", "position"))
  ffcTbl[, leagueType := ifelse(format == "standard", "std", format)]
  return(ffcTbl[!is.na(adp), c("player", "position", "adp", "leagueType"), with = FALSE])
  
}

# Retrieve ADP and Auction Values from ESPN
getESPNValues <- function(){
    espnPos <- c("QB", "RB", "WR", "TE", "K", "D/ST", "DT", "DE", "CB", "S", "LB")
    espnData <- lapply(espnPos, function(p){
        espnUrl <-paste("http://games.espn.go.com/ffl/livedraftresults?position=", p, sep ="")
        espnTbl <- data.table::data.table(XML::readHTMLTable(espnUrl, which = 2, skip.rows=c(1,2), stringsAsFactors = FALSE))
        setnames(espnTbl, c(1:8), c("rank", "player", "position", "adp", "snake7day", "aav", "value7day", "pctOwn"))
        espnTbl[, player := getPlayerName(encodeString(player))]
        espnTbl <- espnTbl[!is.na(player)]
        espnTbl[position == "D/ST", position := "DST"]
        espnTbl[position %in% c("DT", "DE") , position := "DL"]
        espnTbl[position %in% c("CB", "S") , position := "DB"]
        espnTbl[, leagueType := "std"]
        return(espnTbl)
    })
    espnData <- data.table::rbindlist(espnData) 
  return(espnData[,c("player", "position", "adp","aav", "leagueType"), with = FALSE])
}

# Retrieve ADP values from CBS
getCBSValues <-function(){
    cbsVal <- data.table::data.table(XML::readHTMLTable("http://www.cbssports.com/fantasy/football/draft/averages?&print_rows=9999", which = 1, stringsAsFactors = FALSE, skip.rows = 1))
    pgeLinks <- XML::getHTMLLinks("http://www.cbssports.com/fantasy/football/draft/averages?&print_rows=9999")
    pId <- as.numeric(unique(gsub("[^0-9]", "", pgeLinks[grep("/fantasy/football/players/[0-9]{3,6}", pgeLinks)])))
    setnames(cbsVal, c(1:6), c("rank", "player", "trend", "adp", "HiLo", "pctOwn"))
    cbsVal <- cbsVal[!is.na(player)]
    cbsVal[, player := getPlayerName(player)]
    cbsVal[, cbsId := pId]
    plyrs <- players[, c("playerId", "cbsId"), with = FALSE]
    cbsVal <- merge(cbsVal, plyrs, by = "cbsId")
    cbsVal[, leagueType := "std"]
    cbsVal <- cbsVal[, c("playerId", "adp", "leagueType"), with = FALSE]
    return(cbsVal)
}

# Retrieve ADP and auction values from NFL
getNFLValues <- function(season){
    pgs <- seq(from = 1, to = 826, by = 25)
    nflValues <- data.table::data.table(playerId = as.numeric(), player = as.character(), adp = as.numeric(), avgRd = as.numeric(), aav = as.numeric())
    for (pg in pgs){
        nflUrl <- paste("http://fantasy.nfl.com/draftcenter/breakdown?offset=", pg, "&position=all&season=", season, "&sort=draftAveragePosition", sep = "")
        nfldata <- data.table::data.table(XML::readHTMLTable(nflUrl, stringsAsFactors = FALSE, which = 1))
        names(nfldata) <- c("player", "adp", "avgRd", "aav")
        pgeLinks <- XML::getHTMLLinks(nflUrl)
        pId <- as.numeric(unique(gsub("[^0-9]", "", pgeLinks[grep("playerId=[0-9]{3,7}$", pgeLinks)])))
        nfldata[, playerId := pId]
        nflValues <- data.table::rbindlist(list(nflValues, nfldata), fill = TRUE)
    }
    
    nflValues[, player := getPlayerName(getPlayerName(getPlayerName(player)))]
    nflValues <- nflValues[, leagueType := "std"]
    return(nflValues[, names(nflValues), with = FALSE])
}

# Retrive Values from MFL
getMFLValues <- function(year, type = "adp", league = NULL, week = NULL, ppr = FALSE){
  type <- ifelse(type == "nflSchedule", type, tolower(type))
  mflUrl <- paste("http://football.myfantasyleague.com/", year, "/export?TYPE=", type, 
                  "&L=", league, "&W=", week, "&JSON=0", sep = "")
  if(type == c("adp")){
      mflUrl <- paste(mflUrl, "&IS_PPR=", as.numeric(ppr), "&IS_MOCK=0&DAYS=14", sep = "")

  }
  if(type == c("players")){
      mflUrl <- paste(mflUrl, "&DETAILS=1", sep = "")
  }
  mflTbl <- XML::xmlToList(xmlParse(mflUrl))
  
  if(type != "nflSchedule"){
    mflTbl <- mflTbl[-which(names(mflTbl) == ".attrs")]
  }
  
  if(type == "nflSchedule"){
    newTbl <- data.table::data.table(game = as.numeric(), gameDate = as.character(), timeRemaining = as.numeric())
    for(g in 1:(length(mflTbl)-1)){
      data<- mflTbl[[g]][[".attrs"]]
      
      gameData <- data.frame(t(data), stringsAsFactors = FALSE)
      
      kickoff <- as.numeric(gameData$kickoff)
      
      gameDate <- as.Date("1/1/1970 00:00:00", format = "%m/%d/%Y %H:%M:%S")
      gameDate <- gameDate + kickoff/3600/24 -0.5
      
      teams <- lapply(mflTbl[[g]][which(names(mflTbl[[g]])=="team")], function(l)data.frame(t(l), stringsAsFactors = FALSE))
      teams <- data.table::rbindlist(teams)
      teams[, c("game","gameDate", "timeRemaining")  := list(g,  gameDate, gameData$gameSecondsRemaining)]
      newTbl <- plyr::rbind.fill(data.frame(newTbl), data.frame(teams))
    }
    mflTbl <- data.table(newTbl)
    mflTbl[id == "KCC", id := "KC"]
    mflTbl[id == "NEP", id := "NE"]
    mflTbl[id == "SDC", id := "SD"]
    mflTbl[id == "SFO", id := "SF"]
    mflTbl[id == "TBB", id := "TB"]
    mflTbl[id == "NOS", id := "NO"]
    mflTbl[id == "GBP", id := "GB"]
  }
 
  if(type %in% c("players", "injuries")){
    mflTbl <- lapply(mflTbl, function(l)data.frame(t(l), stringsAsFactors = FALSE))
  }
  
  if(type %in% c("aav", "adp")){
    mflTbl <- lapply(mflTbl, function(l)data.frame(t(as.numeric(l)), stringsAsFactors = FALSE))
  }

  if(type != "nflSchedule"){
  mflTbl <- data.table::rbindlist(mflTbl, fill = TRUE)  
  }
  if(exists("id", mflTbl) & type != "nflSchedule"){
    mflTbl$id <- as.numeric(mflTbl$id)
  }
  
  if(type == "players"){
    nameMatrix <- matrix(unlist(strsplit(as.character(mflTbl$name), ", ", fixed = TRUE)), ncol = 2, byrow = TRUE)
    mflTbl$name <- getPlayerName(paste(nameMatrix[,2], nameMatrix[,1]))
    data.table::setnames(mflTbl, c("id", "name"), c("playerId", "player"))
  }
  
  if(type == "adp"){
    data.table::setnames(mflTbl, c("playerId", "selectedInDraft", "adp", "minPick", "maxPick"))
    mflTbl <- mflTbl[, c("playerId", "adp"), with = FALSE]
    mflTbl[, leagueType := ifelse(ppr, "ppr", "std")]
    mflTbl <- mflTbl[order(adp)]
  }
  if(type == "aav"){
    data.table::setnames(mflTbl, c("playerId", "selectedInAuct", "aav"))
    mflTbl <- mflTbl[, c("playerId", "aav"), with = FALSE]
    mflTbl[, leagueType := "std"]
    mflTbl <- mflTbl[order(aav)]
  }
  if(type == "injuries"){
    data.table::setnames(mflTbl, c("playerId", "status", "details"))
  }
  
  return(mflTbl)
}

# Retrieve ADP and Auction Values from Yahoo
getYahooValues <- function(type = "SD"){
  #yahooUrl <- paste("http://football.fantasysports.yahoo.com/f1/draftanalysis?tab=", type, "&pos=", pos, "&sort=DA_AP&count=", sep = "")
  
  if(type == "SD"){
    yahooPos <- list(QB = c(0), RB = c(0), WR =c(0,50), TE = c(0), K = c(0), DEF = c(0))
  }
  if(type == "AD"){
    yahooPos <- list(QB = c(0, 50, 100), RB = seq(0, 200, 50), WR =seq(0, 350, 50), TE = seq(0, 200, 50), K = c(0, 50), DEF = c(0))
  }
  
  yahooDataUrls <- lapply(names(yahooPos), function(pos){
    
    yahooUrl <- paste("http://football.fantasysports.yahoo.com/f1/draftanalysis?tab=", type, "&pos=", pos, "&sort=DA_AP&count=", sep = "")
    yahooPgs <- lapply(yahooPos[[pos]], function(pg)data.table(url = paste(yahooUrl, pg, sep = "")))
    pos <- ifelse(pos == "DEF", "DST", pos)
    if(length(yahooPgs) > 1){
      yahooPgs <- rbindlist(yahooPgs)
    }
    else{
      yahooPgs <- yahooPgs[[1]]
    }
    
    yahooPgs[, position := pos]
    return(yahooPgs)
  })
  
  yahooDataUrls <- data.table::rbindlist(yahooDataUrls)
  yahooData <- apply(yahooDataUrls, 1, function(u){
    data <- data.table::data.table(XML::readHTMLTable(u["url"], stringsAsFactors = FALSE)$draftanalysistable)
    if(nrow(data) > 0){
      data[, position := u["position"]]
      return(data)
    }
    })
  yahooData <- data.table::rbindlist(yahooData)
  if(type == "SD"){
    data.table::setnames(yahooData, c("player", "adp", "avgRound", "pctDraft", "position"))
  }
  if(type == "AD"){
    data.table::setnames(yahooData, c("player", "aav", "avgCost", "pctDraft", "position"))
    yahooData[, aav := as.numeric(gsub("$", "", aav, fixed = TRUE))]
    yahooData[, avgCost :=  as.numeric(gsub("$", "", avgCost, fixed = TRUE))]
  }
  
  yahooData[, player := gsub("Sun 10:00 am|Sun 5:30 pm|Mon 4:10 pm|Sun 1:25 pm|Thu 5:30 pm|Mon 7:20 pm|Sun 1:05 pm", "", player)]
  yahooData[, player := getPlayerName(getPlayerName(getPlayerName(player)))] 
  yahooData[, leagueType := "std"]
  return(yahooData[,c("player", "position", ifelse(type == "SD", "adp", "aav"), "leagueType"), with = FALSE])
}

# Function to retrieve html data from footballguys.com
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
    handle = handle("http://subscribers.footballguys.com"),
    path = "amember/login.php",
    body = list(amember_login = userName,
                amember_pass = password,
                amember_redirect_url = inpUrl)
  )
  return(content(dataPge))
}

# helper function to convert a text value to numeric
tryAsNumeric = function(node) {
    val = XML::xmlValue(node)
    ans = as.numeric(gsub("Â", "", val))
    if(is.na(ans))
        val
    else
        ans
}

# Function to retrieve data from a website table either as html, csv or xml data
# Returns data.table with data
retrieveData <- function(inpUrl, columnTypes, columnNames, removeRow = integer(), 
                         whichTable, dataType = "html", playerLinkString = "", userName, password){
  
  orgUrl <- inpUrl
  is.fbg <- length(grep("footballguys", tolower(inpUrl))) > 0 
  is.fft <- length(grep("fftoday", tolower(inpUrl))) > 0 
  is.shark <- length(grep("fantasysharks", tolower(inpUrl))) > 0
  is.walter <- length(grep("walterfootball", tolower(inpUrl))) > 0 
  is.yahoo <- length(grep("yahoo", tolower(inpUrl))) > 0 
  is.fox <- length(grep("fox", tolower(inpUrl))) > 0 
  is.numberfire <- length(grep("numberfire", tolower(inpUrl))) > 0
  is.rotowire <- length(grep("rotowire", tolower(inpUrl))) > 0
  is.rtsports <- length(grep("rtsports", tolower(inpUrl))) > 0
  is.nerd <- length(grep("fantasyfootballnerd", tolower(inpUrl))) > 0
  stRow <- 1
  if(is.numberfire){
      stRow <- 2
  }
  
  nameColumn <- which(columnNames == "player")
  
  if(is.fbg){
    inpUrl <- fbgUrl(inpUrl, userName, password)
  }
  if(length(removeRow) == 0){
    removeRow <- NULL
  }
  if(dataType == "file" & !is.walter){
      whichTable <- 1
  }
  if(is.walter){
    whichTable <- switch(whichTable,
                         "1" = "QBs",
                         "2" = "RBs",
                         "3" = "WRs",
                         "4" = "TEs",
                         "5" = "Ks",
                         "6" = "DEFs"
                         )
  }
  
  
  if(is.rtsports){
    rtColTypes <- columnTypes
    columnTypes <- rep("character", length(rtColTypes))
  }

  dataTable <- switch(dataType, 
                      "html" = XML::readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = removeRow, 
                                             colClasses = columnTypes, which = as.numeric(whichTable)),
                      "csv" = data.table::data.table(read.csv(inpUrl, stringsAsFactors = FALSE)),
                      "xml" = t(XML::xpathSApply(XML::xmlParse(inpUrl), "//Player", fun = xmlToList)), # This only works for fantasyfootballnerd
                      "xls" = XLConnect::readWorksheetFromFile(file = dataFile, sheet = whichTable , header = TRUE),
                      "file" = XLConnect::readWorksheetFromFile(file = inpUrl, sheet = whichTable, header = TRUE, startRow = stRow)
  )
  
  if(is.rtsports){
      dataTable <- data.table::data.table(sapply(dataTable, function(x)gsub("[^A-Za-z0-9,. ]", "", x) ))
  }
  
  
  if(length(dataTable) == 0 | is.null(dataTable) ){
    warning(cat("Empty data table retrieved from\n", orgUrl), call. = FALSE)
    return(data.table())
  }
  
  if(nrow(dataTable) == 0 ){
    warning(cat("Empty data table retrieved from\n", orgUrl), call. = FALSE)
    return(dataTable)
  }
  ## On CBS.com the last rows of the table includes links to additional pages, so we remove those:
  if(length(grep("Pages:", dataTable[,nameColumn], fixed = TRUE))>0){
    dataTable <- dataTable[-grep("Pages:", dataTable[,nameColumn], fixed = TRUE),]
  }
  
  
  ## Setting names on the columns
  dataTable <- data.table(dataTable)
  if(length(columnNames) != length(dataTable)){
      
      if(is.fox){
          fox.try = 0
          while(length(dataTable) > 1 & (length(columnNames) != length(dataTable)) & fox.try < 10 ){
              print("Retrying fox")
          dataTable <- switch(dataType, 
                              "html" = XML::readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = removeRow, 
                                                     colClasses = columnTypes, which = as.numeric(whichTable)),
                              "csv" = data.table::data.table(read.csv(inpUrl, stringsAsFactors = FALSE)),
                              "xml" = t(XML::xpathSApply(XML::xmlParse(inpUrl), "//Player", fun = xmlToList)), # This only works for fantasyfootballnerd
                              "xls" = XLConnect::readWorksheetFromFile(file = dataFile, sheet = whichTable , header = TRUE),
                              "file" = XLConnect::readWorksheetFromFile(file = inpUrl, sheet = whichTable, header = TRUE, startRow = stRow)
            
          )
          fox.try <- fox.try + 1
          }
          if(length(dataTable) > 1 & (length(columnNames) != length(dataTable)) & fox.try >= 10){
              dataTable   <- data.table::data.table(t(columnNames))
              data.table::setnames(dataTable, columnNames)
              dataTable <- dataTable[0]
          }
          dataTable <- data.table::data.table(dataTable)
      }
      else {
    cat(paste("Mismatch in columns from \n", orgUrl, sep = ""))
    print(paste("Table columns:", paste(names(dataTable), collapse = ", ")))
    print(paste("Expected columns:", paste(columnNames, collapse =", ")))}
  }
  
  colCount <- min(length(columnNames), length(dataTable))
  data.table::setnames(dataTable, 1:colCount, columnNames[1:colCount])
  
  if(is.shark | is.rotowire){
      if(exists("position", dataTable)){
          dataTable[position == "Def", position := "DST"]
          dataTable[position == "D", position := "DST"]
          dataTable[position == "PK", position := "K"]
          dataTable[position %in% c("CB", "S"), position := "DB"]
          dataTable[position %in% c("DE", "DT"), position := "DL"]
      }
      if(exists("firstName", dataTable)){
          dataTable[, player := paste(firstName, lastName)]
      } else {
      dataTable[, player:= firstLast(player)]
          }
  }  
  if(is.nerd & exists("position", dataTable)){
      dataTable[, position := as.character(position)]
  }
  if(is.walter){
    dataTable[, player := paste(firstName, lastName)]
  }
  
  if(is.yahoo){
    dataTable[, player := gsub("Sun 10:00 am|Sun 5:30 pm|Mon 4:10 pm|Sun 1:25 pm|Thu 5:30 pm|Mon 7:20 pm|Sun 1:05 pm", "", player)]
  }

  if(is.numberfire & dataType == "file"){
      wbfile <- XLConnect::loadWorkbook(inpUrl)
      sheet <- XLConnect::getSheets(wbfile)
      if(sheet == "IDP"){
          dataTable[, position := stringr::str_extract(dataTable$player, "DB|LB|DL")]
      }
  }  
  
  # Cleaning up player names
  dataTable[, player := getPlayerName(getPlayerName(getPlayerName(player)))]
 
  
  ## Finding playerId for the sources that has that
  if(dataType == "html"){  
    pgeLinks <- XML::getHTMLLinks(inpUrl)
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
      dataTable <- data.table::data.table(playerId, dataTable)
    }
  }
  return(data.table::data.table(dataTable))
}




# scrapeUrl calls readUrl to retrieve data and then cleans it up
scrapeUrl <- function(urlData, siteCredentials = list()) {
  
  
  table <- urlData["siteTableId"]
  analyst <-urlData["analystId"]
  period <- urlData["urlPeriod"]
  posId <- as.numeric(urlData["positionId"])
  
  posName <- playerPositions$positionCode[playerPositions$positionId == posId]
  tblColumns <- dataColumns[siteTableId == table & columnPeriod == period]

  if(nrow(tblColumns) == 0){
    return(data.table::data.table())
  }
  site <- siteAnalysts$siteID[siteAnalysts$analystId == analyst]
  
  siteInfo <- dataSites[siteId == site]
  subscription <- as.logical(siteInfo$subscription)
  siteName <- tolower(siteInfo$siteName)
  rows2remove <- removeRows$rowRemove[removeRows$siteTableId == table]
 
  if(length(rows2remove) == 0){
    rows2remove <- integer()
  }
  
  un <- as.character()
  pw <- as.character()
  
  if(subscription){
    un <- siteCredentials[[siteName]][["user"]]
    pw <- siteCredentials[[siteName]][["pwd"]]
  }
 
  dataTable <- retrieveData(inpUrl = as.character(urlData[["siteUrl"]]),
                            columnTypes = tblColumns$columnType,
                            columnNames = make.unique(tblColumns$columnName),
                            removeRow = rows2remove,
                            whichTable = as.numeric(as.character(urlData["whichTable"])),
                            dataType = as.character(urlData["urlData"]),
                            playerLinkString = as.character(urlData["playerLinkString"]),
                            userName = un, 
                            password = pw) 
  
  if(nrow(dataTable) == 0){
    return(data.table::data.table())
  }
  idCol <- siteInfo$playerIdCol
  
  if(exists("position", dataTable)){
   dataTable <- dataTable[position == posName,]
  }
  
  
  # Merging with player data from NFL.com to find the playerId.
  detailPos <- posMap[posMap$positionCode == posName, "detailPosition", with = FALSE]$detailPosition
  
  if(siteInfo$siteName != "NFL"){
    if(idCol %in% names(players) & posId != 6 & "playerId" %in% names(dataTable) ){
      data.table::setnames(dataTable, "playerId", idCol)
      dataTable <- merge(dataTable, players[, c("playerId", idCol), with = FALSE], by=idCol)
      dataTable[, c(idCol) := NULL]
    }else{
        if(exists("playerId", dataTable)){
            dataTable$playerId <- NULL
        }
        players[, pname := toupper(player)]
        dataTable[, pname := toupper(player)]
        dataTable <- merge(dataTable, players[position %in% detailPos, c("playerId", "pname"), with = FALSE], by= "pname")
        players[, pname := NULL]
        dataTable[, pname := NULL]
    }
  }
  if(posId == 6){
    print(dataTable)
  }
  
  dataTable[, player:=NULL]
  
  dataTable <- merge(dataTable, players[, c("playerId", "player"), with = FALSE], by = "playerId")
  
  # Separate pass completions from attempts
  if(exists("passCompAtt", dataTable)){
    dataTable[, passComp := stringr::str_sub(string=passCompAtt, end=str_locate(string=passCompAtt, '/')[,1]-1)]
    dataTable[, passAtt :=  stringr::str_sub(string=passCompAtt, start=str_locate(string=passCompAtt, '/')[,1]+1)]
    dataTable[, passCompAtt := NULL]
  }
  
  # Calculate pass incompletions and pass completion percentage
  if(exists("passComp", dataTable) & exists("passAtt", dataTable)){
      dataTable[, c("passComp", "passAtt") := list(as.numeric(passComp), as.numeric(passAtt))]
      dataTable[!is.na(passAtt) & !is.na(passComp), passInc := passAtt - passComp]
      dataTable[!is.na(passAtt) & !is.na(passComp) & passAtt > 0, passCompPct := passComp / passAtt * 100]
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
      dataTable[, fg := ifelse(is.na(fg0019), 0, fg0019) + ifelse(is.na(fg2029), 0, fg2029) + ifelse(is.na(fg3039), 0, fg3039) +
                    ifelse(is.na(fg4049), 0, fg4049) + ifelse(is.na(fg50), 0, fg50)]
  }
   
  # Calculate field goal attempts if missing
  if(exists("fgMiss", dataTable) & exists("fg", dataTable) & !exists("fgAtt", dataTable)){
      dataTable[, fgAtt := fgMiss + fg]
  }
  
  if(!exists("fgMiss", dataTable) & exists("fg", dataTable) & exists("fgAtt", dataTable)){
      dataTable[, fgMiss := as.numeric(fgAtt) - as.numeric(fg)]
  }
  
  
  dataTable[, analystId := analyst]
  removeCols <- tblColumns$columnName[as.numeric(tblColumns$removeColumn) == 1]
  
  # Removing columns not needed.
  if(length(removeCols) >0 ){
    dataTable <- dataTable[, !removeCols, with = FALSE]
  }
  
  return(dataTable)  
}





firstLast <- function(lastFirst){
  nameMatrix <- matrix(unlist(strsplit(lastFirst, ", ")), ncol =2, byrow = TRUE)
  fullName <- paste(nameMatrix[,2], nameMatrix[,1])
  return(fullName)
}
