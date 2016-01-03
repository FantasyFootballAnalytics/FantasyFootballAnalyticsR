## File: updateNflPlayers.R
## Author: Dennis Andersen [andersen.dennis@live.com]
## Date: 3/17/2015
## ----------------------------------------------------------------------------------
## Note:
## This script reads player information off the NFL.com website and writes it to a
## MySQL database. It will add new players and update player information based on
## what is retrieved from NFL.com. 
#####################################################################################
require(RMySQL)
#source(paste(getwd(), "/R Scripts/Functions/nflPlayers.R", sep =""))

## Retrieve all NFL Players -- this takes a while as it retrieves QB, RB, WR, TE, K, 
## DEF and IDP positions.
nflAllPlayers <- readNflPlayers()

## Connect to the playerinfo schema on MySQL Server and retrieve player table
playerDb <- connectDb("ffplayerinfo")
players <- dbReadTable(playerDb, "players")

## Find new players
newPlayers <- merge(x=players, y=nflAllPlayers, by = "playerId", all.y = TRUE)
newPlayers <- newPlayers[is.na(newPlayers$player), c("playerId", "PlayerName", "Pos", "Team", "Status")]
names(newPlayers) <- c("playerId", "player", "position", "team", "status")


# Appending new players to the players table
valueList <- paste(paste(paste("(", newPlayers$playerId, ", '", newPlayers$player, "', '", newPlayers$position, "', '", 
                   newPlayers$team, "', '", newPlayers$status, "')", sep=""), collapse =","), ";", sep = "")

if(!dbIsValid(playerDb)){
  print("reconnecting db")
  playerDb <- connectDb("ffplayerInfo")
}
if(nrow(newPlayers)>0){
  print(paste("Adding", nrow(newPlayers), "new players"))
  res <- dbSendQuery(playerDb, paste("INSERT INTO players(playerId, player, position, team, `status`) values", valueList))
  dbClearResult(res)
}

# Checking for updates too the playres in the player table before new players were added
updatePlayers <- merge(x=players, y=nflAllPlayers, by = "playerId")
updatePlayers <- updatePlayers[,c("playerId", "PlayerName", "Pos", "Team", "Status") ]
names(updatePlayers) <- c("playerId", "player", "position", "team", "status")

# Checking for updated position, team and status
updatePlayers <- updatePlayers[updatePlayers$position != updatePlayers$Pos | updatePlayers$team != updatePlayers$Team |updatePlayers$status != updatePlayers$Status, ]
updatePlayers <- updatePlayers[, c("playerId", "player", "position", "team", "status")]

if(nrow(updatePlayers) >0 ){
  print(paste("Updating", nrow(updatePlayers), "players"))
  valueList <- paste("(",updatePlayers$playerId, " '", updatePlayers$player, "', '", updatePlayers$position, "', '", updatePlayers$team, "', '", updatePlayers$status, "')", sep="")
  
  # Writing the new information to the database by first saving the table, then calling a 
  # stored procedure to update the playertable, before removing the udpateplayers table
  res <- dbSendQuery(playerDb, "CREATE TABLE updateplayers (playerId int(11), player varchar(255), position varchar(10), team varchar(10), `status` varchar(10)) ENGINE=InnoDB DEFAULT CHARSET=utf8;")
  dbClearResult(res)
  res <- dbSendQuery(playerDb, paste("insert into updateplayers values", paste(paste(valueList, collapse =", "), ";", sep="")))
  dbClearResult(res)
  
  res <- dbSendQuery(playerDb, "call updateplayertbl();")
  dbClearResult(res)
  dbRemoveTable(playerDb, "updateplayers")
}
dbDisconnect(playerDb)
