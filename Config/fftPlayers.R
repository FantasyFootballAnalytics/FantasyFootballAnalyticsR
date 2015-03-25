require(data.table)
season = 2014
week = 0


sites <- connectDb("projectionsites")

siteUrls <- dbReadTable(sites, "urlInfo")
analysts <- dbReadTable(sites, "analysts")
dbDisconnect(sites)

fft <- analysts[analysts$analystName %in% c("FFToday", "FFToday - IDP"), "analystId"]

siteUrls <- siteUrls[siteUrls$analystId %in% fft & siteUrls$urlPeriod == 'season', ]

urlTable <- apply(siteUrls,1,urlList)
urlTable <- rbindlist(urlTable)

nameTable <- apply(urlTable, 1, function(x){
  inpUrl <- x["siteUrl"]
  if(length(grep("PosID=99", inpUrl, fixed = TRUE))>0){
    return()
  }
  dataTable <- readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = 1, which = 11)[, c(2,3)]
  names(dataTable) <- c("player", "team")
  pgeLinks <- getHTMLLinks(inpUrl)
  playerId <- unique(gsub("[^0-9]","", gsub("LeagueID=[0-9]{1,6}", "", pgeLinks[grep("/stats/players/", pgeLinks)])))
  
  dataTable$playerId <- playerId
  return(dataTable)

})

nameTable <- rbindlist(nameTable)

nameTable[, player := getPlayerName(player)]

playerDb <- connectDb("ffplayerinfo")

dbplayers <- dbReadTable(playerDb, "players")
dbplayers <- dbplayers[is.na(dbplayers$fftId),]

nameTable <- nameTable[player %in% dbplayers$player,]

if(nrow(nameTable) > 0 ){
  print(paste("Updating", nrow(nameTable), "players"))
  valueList <- paste("('", nameTable$player, "', '", nameTable$team, "', ", nameTable$playerId, ")", sep="")
  if(dbExistsTable(playerDb, "fftplayers")){
    dbRemoveTable(playerDb, "fftplayers")
  }
  res <- dbSendQuery(playerDb, "CREATE TABLE fftplayers (player varchar(255), team varchar(10), playerId int(11)) ENGINE=InnoDB DEFAULT CHARSET=utf8;")
  dbClearResult(res)
  res <- dbSendQuery(playerDb, paste("insert into fftplayers values", paste(paste(valueList, collapse =", "), ";", sep="")))
  dbClearResult(res)
  res <- dbSendQuery(playerDb, "call updatefftplayers();")
  dbClearResult(res)
  dbRemoveTable(playerDb, "fftplayers")
}
dbDisconnect(playerDb)
