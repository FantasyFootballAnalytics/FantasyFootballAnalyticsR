###########################
# File: dualPositionData.R
# Description: Function to combine data for players with dual positions
# Date: 2/6/2016
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# 
# To do:
###########################
require(data.table)

dualPositionData <- function(projectionData = list(), offense = c("QB", "RB", "WR", "TE"), idp =  c("DL", "DB", "LB")){
  
  # Validating that we have a list as input.
  stopifnot(class(projectionData) == "list")
  
  # Defining defensive and offensive positions and intesecting with the names in the
  # list of data tables. Kickers and Team Defenses are excluded.
  offpos <- intersect(names(projectionData), offense)
  defpos <- intersect(names(projectionData), idp)
  
  # Creating pairwise combinations of the positions
  if(length(defpos) > 1 & length(offpos) > 1){
    posComb <- cbind(combn(offpos, 2), combn(defpos, 2))
    positions <- c(offpos, defpos)
  }
  
  if(length(defpos) == 0 & length(offpos) > 1){
    posComb <- combn(offpos, 2)
    positions <- offpos
  }
  
  if(length(defpos) > 1 & length(offpos) == 0){
    posComb <- combn(defpos, 2)
    positions <- defpos
  }
  
  positionData <- list()
  
  # posComb will only be created if there are any combinations of positions based on
  # what positions are available in the source data
  if(exists("posComb")){
    # Stepping through each position combination
    dualData <- apply(posComb,2, function(comb){
      data1 <- projectionData[[comb[1]]]
      data2 <- projectionData[[comb[2]]]
      
      # Finding common players
      commonPlayers <- intersect(data1$playerId, data2$playerId)
      
      # Initializing data tables for new data
      newData1 <- data.table()
      newData2 <- data.table()
      
      if (length(commonPlayers) > 0){
        
        # Finding columns that are in both data sets
        commonData <- intersect(names(data1), names(data2))
        
        # Stepping through common players and 
        for(pl in commonPlayers){
          
          # Determining with data sources are in one but not the other data set
          addSources1 <- setdiff(data1$analystId[data1$playerId == pl], data2$analystId[data2$playerId == pl])
          addSources2 <- setdiff(data2$analystId[data2$playerId == pl], data1$analystId[data1$playerId == pl])
          
          # Adding additional data to original data.
          if(length(addSources1) >0 ){
            addData1 <- projectionData[[comb[1]]][playerId == pl & analystId %in% addSources1, commonData, with = FALSE]
            newData2<- rbindlist(list(newData2, addData1), use.names = TRUE, fill = TRUE)
            rm(addData1)
          }
          if(length(addSources2) >0 ){
            addData2 <- projectionData[[comb[2]]][playerId == pl & analystId %in% addSources2, commonData, with = FALSE]
            newData1<- rbindlist(list(newData1, addData2), use.names = TRUE, fill = TRUE)
            rm(addData2)
          }
        }
        result = list(newData1, newData2)
        names(result) <- c(comb[1], comb[2])
        rm(newData1, newData2)
        return(result)
      }
    })
    
    # Returns a list with a data.table for each position 
    positionData <- lapply(names(positions), function(pos)rbindlist(lapply(dualData, function(dt)dt[[pos]]), fill = TRUE))
  }
  return(positionData)
}