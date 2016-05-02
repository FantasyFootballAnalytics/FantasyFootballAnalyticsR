###########################
# File: adjustFieldGoalStats.R
# Description: Function to adjust field goals statistics
# Date: 2/13/2016
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# 
# To do:
###########################
adjustFieldGoalStats <- function(kickerData = data.table::data.table(), idVars = c("playerId", "analystId"), 
                                 fgTotal = "fg",
                                 fgVars = c("fg0019", "fg2029", "fg3039", "fg4049", "fg50", "fg0039"),
                                 missTotal = "fgMiss",
                                 missVar = c("fgMiss0019", "fgMiss2029", "fgMiss3039", "fgMiss4049", "fgMiss50")){
  
  # Checks that all variables are in the kickData data set. If not an NA column is added.
  for(var in c(fgTotal, fgVars, missTotal, missVar)){
    if(!exists(var, kickerData) & length(kickerData)>0){
      kickerData[ , fgVar:= as.numeric(NA)]
      setnames(kickerData, "fgVar", var)
    }
  }
  
  # Make sure all data columns are numeric
  kickerData <- kickerData[, lapply(.SD, as.numeric), .SDcols = c(fgTotal, fgVars, missTotal, missVar), by = idVars]
  
  
  # Transposing the data set around idVars (default player and analyst)
  meltKicker <- data.table::melt(kickerData, id.vars = idVars, measure.vars = c(fgTotal, fgVars, missTotal, missVar), variable.name = "dataVar")
  
  # Calculating the total number of Field goals from field goals per distance, and using that value where either the total
  # number of field goals are missing or the total number of field goals are less than sum of field goals by distance
  meltKicker <- merge(meltKicker,meltKicker[dataVar %in% fgVars, .(totalFg = sum(value, na.rm = TRUE)), by = idVars], by = idVars)
  
  meltKicker[dataVar == fgTotal & (is.na(value) | value < totalFg), value := totalFg]
  
  meltKicker <- merge(meltKicker, meltKicker[dataVar %in% missVar, .(totalMiss = sum(value, na.rm = TRUE)), by = idVars], by = idVars)
  meltKicker[dataVar == missTotal, value := totalMiss]
  
  meltKicker[, c("totalFg", "totalMiss") := NULL]
  
  castFormula <- as.formula(paste(paste(idVars, collapse = "+"), " ~ ", "dataVar"))
  
  kickerData <- data.table::dcast(meltKicker, castFormula, fun = sum)

  return(kickerData[,  names(kickerData)[!sapply(kickerData, function(x)all(is.na(x)))], with = FALSE])
}