###########################
# File: getMeltedData.R
# Description: Function to transpose data files
# Date: 2/13/2016
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# 
# To do:
###########################
getMeltedData <- function(dbData, idVars = c("playerId", "analystId"), valueCol = "dataCol", removeCol = "player"){
  # Remove specified columns from the data table
  if(any(names(dbData) %in% removeCol)){
    remove <- intersect(names(dbData), removeCol)
    dbData <- dbData[, dbData[, remove] := NULL]
  }
  
  # Variable columns are the remaining columns not identified by the idVars
  varCols <- setdiff(names(dbData), idVars)
  
  # Make sure all variable columns are numric
  dbData <- dbData[, lapply(.SD, as.numeric), .SDcols = varCols, by = idVars]
  
  proj <- melt.data.table(dbData, id.vars = idVars, variable.name = valueCol , measure.vars = varCols)
  return(proj)
}