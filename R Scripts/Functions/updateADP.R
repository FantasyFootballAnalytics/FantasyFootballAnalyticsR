###########################
# File: updateADP.R
# Description: Function get ADP and AAV Values
# Date: 2/6/2016
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# Resulting table will need to be merged with the playerData table to get player names, positions and teams.
# To do:
###########################
source("R Scripts/Functions/scrapefunctions.R")

updateValues <- function(weekNo, season, type = "standard", value = "adp", sources = c("MFL", "ESPN", "FFC", "CBS", "NFL", "Yahoo")){
  
  lgType <- ifelse(type == "standard", "std", "ppr")
  players[, player := toupper(player)]
  
  # Initializing the resulting table
  valTbl <- data.table(player = as.character(), position = as.character(), "value" = as.numeric(), 
                       leagueType = as.character(), "source" = as.character()) 
  setnames(valTbl, "value", value)
  
  # Getting values from MFL
  if(any(sources == "MFL")){
    mflPlayers <- getMFLValues(season, type = "players")
    mflVal <-  getMFLValues(season, type = value, ppr = (type != "standard"))
    mflVal <- merge(mflPlayers, mflVal, by = "playerId")
    mflVal[position == "Def", position := "DST"]
    mflVal[position == "PK", position := "K"]
    mflVal[position %in% c("S", "CB"), position := "DB"]
    mflVal[position %in% c("DE", "DT"), position := "DL"]
    mflVal <- mflVal[, c("player", "position", value, "leagueType"), with = FALSE]
    mflVal[, "source" := "MFL"]
    valTbl <- rbindlist(list(valTbl, mflVal))
  }

  if(type == "standard"){
    # Getting values from ESPN
    if(any(sources == "ESPN")){
      espnVal <- getESPNValues()[, c("player", "position", value, "leagueType"), with = FALSE]
      espnVal[, "source" := "ESPN"]
      valTbl <- rbindlist(list(valTbl, espnVal))
    }

    # Getting values from NFL
    if(any(sources == "NFL")){
      nflVal <- getNFLValues(season)[, c("playerId", value, "leagueType"), with = FALSE]
      nflVal[, source := "NFL"]
      
    }
  }

  if(value == "adp"){
    # Getting values from Fantasy Football Calculator
    if(any(sources == "FFC")){
      ffcVal <- getFFCValues(format = type)[, c("player", "position", value, "leagueType"), with = FALSE]
      ffcVal[, "source" := "FFC"]
      valTbl <- rbindlist(list(valTbl, ffcVal))
    }
    
    if(type == "standard"){
      # Getting adp values from Yahoo
      if(any(sources == "Yahoo")){
        yahooVal <- getYahooValues(type = "SD")[, c("player", "position", value, "leagueType"), with = FALSE]
        yahooVal[, source := "Yahoo"]
        valTbl <- rbindlist(list(valTbl, yahooVal))
      }
      
      # Getting adp values from CBS
      if(any(sources == "CBS")){
        cbsVal <- getCBSValues()
        cbsVal[, source := "CBS"]
      }
    }
    
  }
  else{
    # Getting auction values from Yahoo
    yahooVal <- getYahooValues(type = "AD")[, c("player", "position", value, "leagueType"), with = FALSE]
    yahooVal[, source := "Yahoo"]
    valTbl <- rbindlist(list(valTbl, yahooVal))
  }
  
  # Getting playerId from the players table
  if(nrow(valTbl) > 0){
    valTbl[, player := toupper(player)]
    valTbl <- merge(valTbl, players[, c("player", "position", "playerId"), with = FALSE], by =  c("player", "position"), all.x = TRUE)
    
  }
  
  # Adding the values retrieved from NFL
  if(exists("nflVal"))
    valTbl <- rbindlist(list(valTbl, nflVal), fill = TRUE, use.names = TRUE)
  
  # Adding the values retrieved from CBS
  if(exists("cbsVal"))
    valTbl <- rbindlist(list(valTbl, cbsVal), fill = TRUE, use.names = TRUE)
    
  # Removing player and position columns
  valTbl[, c("player", "position") := list(NULL, NULL)]
  
  # Removing players without playerId
  valTbl <- valTbl[!is.na(playerId)]
  
  # If there are more than 1 source then calculate Average across sources
  if(length(sources) > 1 ){
    setnames(valTbl, value, "dataVal")
    valTbl[, avgVal := mean(as.numeric(dataVal), na.rm = TRUE), by = playerId]
    avgTbl <- unique(valTbl[,c("playerId", "avgVal"), with = FALSE])
    valTbl[, avgVal := NULL]
    setnames(valTbl, "dataVal", value)
    setnames(avgTbl, "avgVal", value)
    avgTbl[, "source" := "AVG"]
    avgTbl <- avgTbl[, c(value, "source", "playerId"), with = FALSE]
    avgTbl[, leagueType := lgType]
    valTbl <- rbindlist(list(valTbl, avgTbl), fill = TRUE)
  }
  
  if(exists("dataVal", valTbl))
    setnames(valTbl, "dataVal", value)
  
  return(valTbl[, c("playerId", value, "leagueType", "source"), with = FALSE])
}
