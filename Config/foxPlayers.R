require(data.table)
require(plyr)
season = 2014
week = 0


sites <- connectDb("projectionsites")

siteUrls <- dbReadTable(sites, "urlInfo")
analysts <- dbReadTable(sites, "analysts")
dbDisconnect(sites)

fox<- analysts[analysts$analystName %in% c("FOX Sports"), "analystId"]

siteUrls <- siteUrls[siteUrls$analystId %in% fox & siteUrls$urlPeriod == 'season', ]

urlTable <- apply(siteUrls,1,urlList)
urlTable <- rbindlist(urlTable)

nameTable <- apply(urlTable, 1, function(x){
  inpUrl <- x["siteUrl"]
  if(length(grep("position=32768", inpUrl, fixed = TRUE))>0){
    return()
  }
  dataTable <- readHTMLTable(inpUrl, stringsAsFactors = FALSE)$playerTable[1]
  names(dataTable) <- c("player")
  dataTable$player <- gsub("Van ", "Van", dataTable$player, fixed = TRUE)
  dataTable$player <- gsub("Don ", "Don", dataTable$player, fixed = TRUE)
  dataTable$player <- gsub(" {2,99}", " ", gsub("Jr.|Sr.|III|\\r|\\n|\\t|\\(|\\)| -|, ", "" , dataTable$player))
  datamatrix <- matrix(unlist(strsplit(dataTable$player, " ", fixed = TRUE)), ncol = 4, byrow = TRUE)
  dataTable$player <- getPlayerName(paste(datamatrix[,1], datamatrix[,2]))
  dataTable$team <- toupper(datamatrix[,3])
  pgeLinks <- getHTMLLinks(inpUrl)
  playerId <- unique(gsub("[^0-9]", "", pgeLinks[grep("/fantasy/football/commissioner/Players/Profile.aspx", pgeLinks)]))
  
  dataTable$playerId <- playerId
  return(dataTable)
})

nameTable <- rbindlist(nameTable)

playerDb <- connectDb("ffplayerinfo")

players <- dbReadTable(playerDb, "players")
players <- players[is.na(players$foxId),]

nameTable <- nameTable[nameTable$player %in% players$player,]

if(nrow(nameTable) >0){
  print(paste("Updating", nrow(nameTable), "players"))
  valueList <- paste("('", nameTable$player, "', '", nameTable$team, "', ", nameTable$playerId, ")", sep ="")
  res <- dbSendQuery(playerDb, "create table foxplayers (player varchar(255), team varchar(10), playerId int(11)) ENGINE=InnoDB DEFAULT CHARSET=utf8;")
  dbClearResult(res)
  res <- dbSendQuery(playerDb, paste("insert into foxplayers values ", paste(valueList, collapse = ", "), ";", sep =""))
  dbClearResult(res)
  res <- dbSendQuery(playerDb, "call updatefoxplayers();")
  dbClearResult(res)
  dbRemoveTable(playerDb, "foxplayers")
}
dbDisconnect(playerDb)