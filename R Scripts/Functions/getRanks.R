###########################
# File: getRanks.R
# Description: Function get ECR Ranks from Fantasy Pros
# Date: 2/6/2016
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# Resulting table will need to be merged with the playerData table to get player names, positions and teams.
# To do:
###########################
source("R Scripts/Functions/scrapefunctions.R")

getRanks <- function(posName = "consensus", leagueType = "std", weekNo = 0){
  
  # Defining return columns for the table
  if(weekNo == 0)
    returnColumns <-  c("playerId", "ecrRank", "bestRank", "worstRank", "avgRank", "sdRank", "adp", "vsAdp", "rankType")
  else
    returnColumns <- c("playerId", "ecrRank", "bestRank", "worstRank", "avgRank", "sdRank", "rankType")
  
  defaultTable <- rep(NA, length(returnColumns))
  names(defaultTable) <- returnColumns
  
  # There aren't ppr rankings for QB, K, DST and IDP, so we use the standard ones
  if(!(posName %in% c("RB", "WR", "TE", "consensus")) & leagueType != "std"){
    leagueType = "std"
  }
  
  # There aren't weekly rankings for IDP so we return empty table
  if(weekNo != 0 & posName %in% c("DB", "DL", "LB")){
    warning("Weekly ECR ranks not available for IDP", call. = FALSE)
    dt <- data.table(t(defaultTable))
    return(dt[!is.na(playerId), returnColumns, with = FALSE])
  }
  
  # Overall ranks are not available on weekly data, so we return empty table
  if(weekNo != 0 & posName == "consensus"){
    warning("Weekly overall rankings not available", call. = FALSE)
    dt <- data.table(t(defaultTable))
    return(dt[!is.na(playerId), returnColumns, with = FALSE])
  }
  
  # Generating the URL to scrape the data from
  url_base <- "http://www.fantasypros.com/nfl/rankings"
  if(weekNo != 0){
    url_path <- paste("/", ifelse(leagueType != "std", paste(tolower(leagueType), "-", sep = ""),""), tolower(posName), ".php", sep = "") 
  } else {
    url_path <- paste("/", ifelse(leagueType != "std", paste(tolower(leagueType), "-", sep = ""),""), 
                      ifelse(posName == "consensus" & leagueType != "std", "", paste(tolower(posName), "-", sep ="")), "cheatsheets.php", sep = "") 
  }
  
  inpUrl <- paste(url_base, url_path, sep = "")
  
  # Setting up column names and types
  if(posName %in% c("DB", "DL", "LB", "consensus")){
    cNames <- c("ecrRank", "player", "position" , "bye", "bestRank", "worstRank", "avgRank", "sdRank")
    cTypes <- c("numeric", "character", "character","numeric" ,"numeric", "numeric", "numeric", "numeric")
  }
  else{
      if(weekNo == 0){
          cNames <- c("ecrRank", "player", "bye" , "bestRank", "worstRank", "avgRank", "sdRank")
          cTypes <- c("numeric", "character", "numeric", "numeric", "numeric", "numeric", "numeric")
      }
      else {
          cNames <- c("ecrRank", "player", "opp", "bestRank", "worstRank", "avgRank", "sdRank")
          cTypes <- c("numeric", "character", "character","numeric", "numeric", "numeric", "numeric")
      }
  }
  
  if (weekNo == 0 & !(posName %in% c("DB", "DL", "LB"))){
    cNames <- c(cNames, "adp", "vsAdp")
    cTypes <- c(cTypes, "numeric", "numeric")
  }
  
  # Retrieveing the data
  rnks <- tryCatch(retrieveData(inpUrl, 
                                columnTypes = cTypes,  
                                columnNames = cNames, 
                                whichTable = 2), error = function(e){warning(paste("didn't get ranks from", inpUrl))
                                                                     return()})
  
  # Cleaning player names
  rnks$player <- getPlayerName(rnks$player)
  
  positions <- data.table(positionId = c(1:9), position = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"))
  
  # Removing numbers from the position variable, i.e. RB5, WR14 -> RB, WR
  if(exists("position", rnks)){
    rnks[, position := gsub("[0-9]", "", position)]
  }
  
  # Merging with player data from NFL.com to find the playerId.
  if(posName != "consensus"){
    rnks <- merge(rnks, players[position == posName, c("playerId", "player"), with = FALSE], by= "player")
    posId <- positions[position == posName, c("positionId"), with = FALSE]$positionId
    rnks[, positionId := posId]
    rnks[, rankType := "position"]
  }
  else{
    rnks <- merge(rnks, players[, c("playerId", "player", "position"), with = FALSE], by= c("player", "position"))
    rnks <- merge(rnks, positions, by = "position")
    rnks[, rankType := "overall"]
  }
  
  # Order result by rank
  rnks <- rnks[order(ecrRank),]
  
  return(rnks[, intersect(returnColumns, names(rnks)), with = FALSE])
}