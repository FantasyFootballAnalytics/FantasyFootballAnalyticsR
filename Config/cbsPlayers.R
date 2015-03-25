require(XML)
require(RMySQL)
require(plyr)
getCbsPlayers <- function(posData){
posName = posData[["name"]]
inpUrl <- paste("http://fantasynews.cbssports.com/fantasyfootball/playerindex/POS_", posName, sep = "")
cbsPlayers <- readHTMLTable(inpUrl, stringsAsFactors = FALSE)[[7]]
playerMatrix <- matrix(unlist(strsplit(cbsPlayers[,1],  paste(" ", posName, ", ", sep=""), fixed = TRUE)), nrow = nrow(cbsPlayers), byrow = TRUE)

playerNames <- gsub(" (FA)", "", playerMatrix[,1], fixed = TRUE)
playerTeam <- playerMatrix[,2]

nameSplit <- matrix(unlist(strsplit(playerNames, ", ", fixed = TRUE)), nrow = length(playerNames), byrow = TRUE)

playerNames <- paste(nameSplit[,2], nameSplit[,1])

playerNames <- getPlayerName(playerNames)

pgeLinks <- getHTMLLinks(inpUrl)

playerId <- unique(gsub("[^0-9]", "", 
            pgeLinks[grep("/fantasyfootball/players/playerpage/[0-9]{3,6}", pgeLinks)]
))

cbsPlayers <- data.frame(Player = playerNames, playerId = as.numeric(playerId), Team = playerTeam, stringsAsFactors = FALSE)
return(cbsPlayers)
}

cbsPosition = list(qb = list(name = "QB"),
                   rb = list(name = "RB"),
                   wr = list(name = "WR"),
                   te = list(name = "TE"),
                   pk = list(name = "K")
                   )

cbsPlayers <- lapply(cbsPosition, getCbsPlayers)

cbsAllPlayers <- data.frame(Player = as.character(), playerId = as.numeric(), Team = as.character())

for(plList in cbsPlayers){
  cbsAllPlayers <- rbind.fill(cbsAllPlayers, plList)
}

playerDb <- connectDb("ffplayerinfo")

players <- dbReadTable(playerDb, "players")
players <- players[is.na(players$cbsId),]

teams <- dbReadTable(playerDb, "nflteams")
teams$fullName <- paste(teams$teamArea, teams$teamName)
teams <- teams[, c("teamCode", "fullName")]

cbsAllPlayers <- merge(x=cbsAllPlayers[cbsAllPlayers$Player %in% players$player,], y= teams, by.x = "Team", by.y = "fullName", all.x = TRUE)
cbsAllPlayers$Team <- NULL
if(nrow(cbsAllPlayers) > 0){
  print(paste("Updating", nrow(cbsAllPlayers), "players!"))
  valueList <- paste("(", cbsAllPlayers$playerId, ", '", cbsAllPlayers$Player, "', '", cbsAllPlayers$teamCode, "')", sep="")
  
  res <- dbSendQuery(playerDb, "create table cbsplayers(playerId int(11), Player varchar(255), teamCode varchar(10))  ENGINE=InnoDB DEFAULT CHARSET=utf8;")
  dbClearResult(res)
  res <- dbSendQuery(playerDb, paste("insert into cbsplayers values", paste(valueList, collapse =", "), ";", sep =""))
  dbClearResult(res)
  res <- dbSendQuery(playerDb, "call updateCbsId();")
  dbClearResult(res)
  
  dbRemoveTable(playerDb, "cbsplayers")
}

dbDisconnect(playerDb)
