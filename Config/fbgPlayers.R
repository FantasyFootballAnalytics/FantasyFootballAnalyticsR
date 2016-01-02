source("../scripts/connectDb.R")
getFbgPlayers <- function(posData){
  inpUrl <- paste("http://subscribers.footballguys.com/players/ppindex-", posData[["name"]], ".php", sep = "")
  playerTbl <- readHTMLTable(inpUrl, header = FALSE, stringsAsFactors = FALSE)$`NULL`[,1]
  playerTeam <- matrix(unlist(strsplit(playerTbl," - ",  fixed = TRUE)), nrow =length(playerTbl), byrow = TRUE)[,2]
  playerTeam[playerTeam == "JAX"] <- "JAC"
  playerTbl <-matrix(unlist(strsplit(playerTbl," - ",  fixed = TRUE)), nrow =length(playerTbl), byrow = TRUE)[,1]
  nameMatrix <- matrix(unlist(strsplit(playerTbl, ", ", fixed = TRUE)), nrow = length(playerTbl), byrow = TRUE)
  playerNames <- getPlayerName(paste(nameMatrix[,2], nameMatrix[,1]))
  pgeLinks = getHTMLLinks(inpUrl)
  playerId = gsub("http://subscribers.footballguys.com/apps/news.php?id=","",pgeLinks[grep("/apps/news.php?", pgeLinks, fixed = TRUE)], fixed = TRUE)
  playerDf <- data.frame(Player = as.character(playerNames), playerId = as.character(playerId), Team = as.character(playerTeam))
  return(playerDf)
}

fbgPositions <- list( qb = list(name = "qb"),
                      rb = list(name = "rb"),
                      wr = list(name = "wr"),
                      te = list(name = "te"),
                      pk = list(name = "pk"),
					  dt = list(name = "dt"),
					  de = list(name = "de"),
					  ilb = list(name = "ilb"),
					  olb = list(name = "olb"),
					  cb = list(name = "cb"),
					  s = list(name = "s"))

fbgPlayers <- lapply(fbgPositions, getFbgPlayers)

fbgAllPlayers <- data.frame(Player = as.character(), playerId = as.character(), Team = as.character(), stringsAsFactors = FALSE)

for(plList in fbgPlayers){
  fbgAllPlayers <- rbind.fill(fbgAllPlayers, plList)
}

playerDb <- connectDb("ffplayerinfo")

players <- dbReadTable(playerDb, "players")

players <- players[is.na(players$fbgId), ]

fbgAllPlayers <- fbgAllPlayers[fbgAllPlayers$Player %in% players$player, ]
if(nrow(fbgAllPlayers) > 0){
  print(paste("Updating", nrow(fbgAllPlayers), "players"))
  valueList = paste("('", fbgAllPlayers$Player, "' ,'", fbgAllPlayers$playerId, "', '", fbgAllPlayers$Team, "')", sep = "")
  res <- dbSendQuery(playerDb, "CREATE TABLE fbgplayers(player varchar(255), playerId varchar(15), team varchar(15))  ENGINE=InnoDB DEFAULT CHARSET=utf8;")
  dbClearResult(res)
  
  res <- dbSendQuery(playerDb, paste("INSERT into fbgplayers values ", paste(valueList, collapse=","), ";", sep = ""))
  dbClearResult(res)
  
  res <- dbSendQuery(playerDb, "call updatefbgplayers();")
  dbClearResult(res)

  dbRemoveTable(playerDb, "fbgplayers")
}
dbDisconnect(playerDb)