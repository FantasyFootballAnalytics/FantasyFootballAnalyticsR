###########################
# File: redistributeValues.R
# Description: Function to redistribute values from one statistic to a list of others.
# Date: 2/13/2016
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# 
# To do:
###########################
redistributeValues <- function(valueTable = data.table(), valueVar = "value", dataVar = "dataCol", positionVar = "position",
                               playerVar = "playerId", analystVar = "analystId", weightVar = "weight", calcType = "weighted",
                               fromVar = "fg", toVars = c("fg0019", "fg2029", "fg3039", "fg4049", "fg50"),
                               excludeAnalyst = "wafo"){
  
  if(!all(c(valueVar, dataVar, playerVar, analystVar) %in% names(valueTable)))
    stop("Not all specified columns are present in the table", call. = FALSE)
  
  if(calcType == "weighted" & !(weightVar %in% names(valueTable))){
    warning("calcType is weighted but no weights specified. Using even weights", call. = FALSE)
    weightVar <- "weight"
    valueTable[, (weightVar) := 1]
  }
  
  if(length(excludeAnalyst) > 1)
    stop("Only specify one analyst to exclude", call. = FALSE)
  
  if(length(fromVar) != 1)
    stop("Please specify one variable to distribute from", call. = FALSE)
  
  allAnalysts <- valueTable[[analystVar]]
  allValues <- valueTable[[valueVar]]
  allVars <- valueTable[[dataVar]]
  allWeights <- valueTable[[weightVar]]
  
  if(any(allAnalysts == excludeAnalyst)){
    fromSelection <- which(allAnalysts == excludeAnalyst & allVars == fromVar)
    toSelection <- which(allAnalysts !=  excludeAnalyst & allVars %in% toVars)
  } else {
    fromSelection <- which(allVars == fromVar)
    toSelection <- which(allVars %in% toVars)
  }
  
  fromTable <- valueTable[fromSelection,]
  fromTable[, (dataVar) := NULL]
  
  avgTable <- valueTable[toSelection, 
                         .(totalValue = avgValue(calcMethod = calcType, dataValue = allValues[toSelection], 
                                                 dataWeights = allWeights[toSelection], na.rm = TRUE)), 
                         by = c(playerVar, dataVar, positionVar)]
  
  allAvgVars <- avgTable[[dataVar]]
  
  avgTable[which(allAvgVars %in% toVars), totalVar := sum(totalValue, na.rm = TRUE), by = c(playerVar, positionVar)]
  avgTable[which(allAvgVars %in% toVars) & totalVar != 0 , varShare := totalValue / totalVar]
  
  toTable <- merge(avgTable, fromTable, suffixes = c("", "_from"),  by = c(playerVar, positionVar), allow.cartesian = TRUE)
  toTable <- toTable[!is.na(totalValue)]
  
  allToVars <- toTable[[dataVar]]
  fromValues <- toTable[[valueVar]]
  
  toTable[which(allToVars %in% toVars), (valueVar) := fromValues * ifelse(is.na(varShare), 0, varShare)]
  
  if(weightVar %in% names(toTable)){
    toTable <- toTable[, c(playerVar, analystVar, positionVar, dataVar, valueVar, weightVar), with = FALSE]
  } else {
    toTable <- toTable[, c(playerVar, analystVar, positionVar, dataVar, valueVar), with = FALSE]
  }
  
  return(toTable)
}