####################################################################################
# File    : playerData.R
# Date    : 1/2/2016
# Author  : Dennis Andersen [andersen.dennis@live.com]
# ----------------------------------------------------------------------------------
# Note    :
# This file consists of functions to create the player data file. The playere data 
# consists of the MFL player database combined with the NFL player database to 
# fill out any missing player IDs
####################################################################################
library(XML)
library(data.table)

# Read data scrape functions and function to retrieve NFL players
source("R Scripts/Functions/scrapefunctions.R")
source("R Scripts/Functions/nflPlayers.R")

# Function to read player data from MFL using the MFL API
mflData <- function(y){
  mflPlayers <- xmlToList(paste("http://football.myfantasyleague.com/", y, "/export?TYPE=players&L=&W=&JSON=0&DETAILS=1", sep =""))
  mflPlayers$.attrs <- NULL
  
  mflPlayers <- rbindlist(lapply(mflPlayers, function(pl)data.table(t(pl))), fill = TRUE)
  
  
  # Reducing the NFL IDs to be just numbers
  mflPlayers[, nfl_id := gsub("[^0-9]", "", mflPlayers$nfl_id)]
  
  # Updating team names
  mflPlayers[team == "FA*", team := "FA"]
  mflPlayers[team == "SFO", team := "SF"]
  mflPlayers[team == "NOS", team := "NO"]
  mflPlayers[team == "TBB", team := "TB"]
  mflPlayers[team == "GBP", team := "GB"]
  mflPlayers[team == "SDC", team := "SD"]
  mflPlayers[team == "KCC", team := "KC"]
  mflPlayers[team == "NEP", team := "NE"]
  
  # Updating position names
  mflPlayers[position %in% c("DE", "DT"), position := "DL"]
  mflPlayers[position %in% c("S", "CB"), position := "DB"]
  mflPlayers[position == "PK", position := "K"]
  mflPlayers[position == "Def", position := "DST"]
  
  # Only keep players that we are actually projecting (excluding coaches and team positions)
  mflPlayers <- mflPlayers[position %in% c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB" )]
  setnames(mflPlayers, c("nfl_id", "stats_id", "cbs_id", "name"), c("playerId", "yahooId", "cbsId", "player"))
  return(mflPlayers)
}

# Function to get the player data and create the data files
getPlayerData <- function(season){
  # Get the MFL Player data
  mflPlayers <- mflData(season)
  
  #  Update names to be uniform
  mflPlayers[, player := getPlayerName(getPlayerName(getPlayerName(firstLast(player))))]
  
  
  # Update team names
  nflTeams <- data.table(read.csv(paste(getwd(), "/Data/NFLTeams.csv", sep =""), stringsAsFactors = FALSE))
  nflTeams[TeamCode == "GNB", TeamCode := "GB"]
  nflTeams[TeamCode == "KAN", TeamCode := "KC"]
  nflTeams[TeamCode == "TAM", TeamCode := "TB"]
  nflTeams[TeamCode == "SDG", TeamCode := "SD"]
  nflTeams[TeamCode == "NOR", TeamCode := "NO"]
  nflTeams[TeamCode == "NWE", TeamCode := "NE"]
  nflTeams[TeamCode == "SFO", TeamCode := "SF"]
  
  # Set playerId for DST
  for(n in 1:nrow(nflTeams)){
    mflPlayers[position == "DST" & team == nflTeams$TeamCode[n], playerId := nflTeams$TeamId]
  }
  
  # Retrieve all NFL Players -- this takes a while as it retrieves QB, RB, WR, TE, K, 
  # DEF and IDP positions.
  nflAllPlayers <- data.table(readNflPlayers())
  
  # Update position names
  nflAllPlayers[Pos %in% c("DE", "DT", "NT"), Pos := "DL"]
  nflAllPlayers[Pos %in% c("OLB", "ILB", "MLB"), Pos := "LB"]
  nflAllPlayers[Pos %in% c("CB", "SS", "FS", "SAF"), Pos := "DB"]
  
  
  
  # Update column names
  setnames(nflAllPlayers, c("Pos", "Num", "PlayerName", "Status", "Team", "playerId"), c("position", "jersey", "player", "status", "team", "playerId"))
  
  # Merge data and update playerId if it is missing
  mflPlayers <- merge(mflPlayers, nflAllPlayers, by = c("player", "position"), all.x = TRUE)
  mflPlayers[is.na(playerId.x) & !is.na(playerId.y), playerId.x := playerId.y]
  
  # Exclude players without playerId
  mflPlayers <- mflPlayers[!is.na(playerId.x)]
  
  # Select columns to export
  playerData <- mflPlayers[, c("playerId.x", "yahooId", "cbsId", "player", "position", "team.x", "draft_year", "birthdate"), with = FALSE]
  setnames(playerData, c("playerId.x", "team.x"), c("playerId", "team"))
  
  # Save and return data
  save(playerData, file = "Config/Data/playerData.RData")
  write.csv(playerData, file = "Config/Data/playerData.csv", row.names = FALSE)
  return(playerData)
}