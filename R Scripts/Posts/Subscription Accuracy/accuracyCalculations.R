
# Compare overall accuracy
overallAccuracy <- function(dataTable){
  # Calculate average projected points for each group
  subsGroup <- dataTable[, .(projectPoints = mean(points_proj, na.rm = TRUE)), by = c("subscription", "points_act", "playerId")]
  
  subAccuracy <- subsGroup[, .(rsq = summary(lm(points_act ~ projectPoints, data = subsGroup[subscription == unlist(.BY)]))$r.squared), by = "subscription"]
  subMase <- subsGroup[, .(mase = calculateMASE(projectPoints, points_act)), by = "subscription"]
  alldata <- dataTable[, .(projectPoints = mean(points_proj, na.rm = TRUE)), by = c("points_act", "playerId")]
  allAccuracy <-  data.table::data.table("Source Type" = "All",
                                         "R-Squared" = summary(lm(points_act ~ projectPoints, data = alldata))$r.squared,
                                         "MASE" =  calculateMASE(alldata$projectPoints, alldata$points_act))
  
  
  accTable <- merge(subAccuracy, subMase, by = "subscription")
  accTable[, "Source Type" := ifelse(subscription == 1, "Subscription", "Free")]
  accTable[, subscription := NULL]
  data.table::setnames(accTable, c("rsq", "mase"), c("R-Squared", "MASE"))
  accTable <- data.table::rbindlist(list(accTable, allAccuracy), fill = TRUE)
  return(accTable[, c("Source Type", "R-Squared", "MASE"), with = FALSE])
}

# Calculate accuracy with all possible combinations of free sources matching
# the number of subscription sources
freeCombinations <- function(dataTable){
  numSubs <-  length(unique(dataTable$analystId[dataTable$subscription == 1]))
  free <-  unique(dataTable$analystId[dataTable$subscription == 0])
  freeComb <- combn(free, numSubs)
  
  freeComp <- data.table::rbindlist(apply(freeComb, 2, function(comb){
    # Calculate average projected points for each group
    subsGroup <- dataTable[analystId %in% comb, .(projectPoints = mean(points_proj, na.rm = TRUE)), by = c("points_act", "playerId")]
    
    subAccuracy <- summary(lm(points_act ~ projectPoints, data = subsGroup))$r.squared
    subMase <- calculateMASE(subsGroup$projectPoints, subsGroup$points_act)
    
    
    accTable <- data.table::data.table("R-Squared"= subAccuracy, "MASE" = subMase)
    return(accTable)
  }))
  
  resultTable <- data.table::data.table(" "= "Combinations of free sources", "R-Squared" = mean(freeComp$`R-Squared`, na.rm = TRUE), 
                                  "MASE" =  mean(freeComp$MASE, na.rm = TRUE))
  
  return(resultTable[, c(" ", "R-Squared", "MASE"), with = FALSE])
}

# Compare accuracy by position
positionAccuracy <- function(dataTable){
  # Calculate average projected points for each group
  subsGroup <- dataTable[, .(projectPoints = mean(points_proj, na.rm = TRUE)), by = c("subscription", "points_act", "positionId", "playerId")]
  alldata <- dataTable[, .(projectPoints = mean(points_proj, na.rm = TRUE)), by = c("points_act", "positionId", "playerId")]
  posIds <- c(QB = 1, RB = 2, WR = 3, TE = 4)
  
  subAccuracy <- subsGroup[, .(rsq = summary(lm(points_act ~ projectPoints, data = subsGroup[subscription == unlist(.BY)[1] & positionId == unlist(.BY)[2]]))$r.squared), by = c("subscription", "positionId")]
  subMase <- subsGroup[, .(mase = calculateMASE(projectPoints, points_act)), by = c("subscription", "positionId")]
  
  allAccuracy <- alldata[, .(rsq = summary(lm(points_act ~ projectPoints, data = alldata[positionId == unlist(.BY)]))$r.squared), by = c("positionId")]
  allMase <- alldata[, .(mase = calculateMASE(projectPoints, points_act)), by = c( "positionId")]
  
  allTable <- merge(allAccuracy, allMase, by = "positionId")
  allTable[, c("subscription", "Source Type") := list(2, "All")]
  accTable <- merge(subAccuracy, subMase, by = c("subscription", "positionId"))
  accTable[, "Source Type" := ifelse(subscription == 1, "Subscription", "Free")]
    accTable <- data.table::rbindlist(list(accTable, allTable), fill = TRUE)
  
  data.table::setnames(accTable, c("rsq", "mase"), c("R-Squared", "MASE"))
  accTable <- accTable[order(positionId, subscription)]
  
  accTable[, Position := names(posIds)[which(posIds == positionId)],  by = c("subscription", "positionId")]
  accTable[is.na("Source Type"), Position := NA]
  return(accTable[, c("Source Type", "Position", "R-Squared", "MASE"), with = FALSE])
}

# Calculate accuracy with all possible combinations of free sources matching
# the number of subscription sources by position
freeCombinationsPos <- function(dataTable){
  posIds <- c(QB = 1, RB = 2, WR = 3, TE = 4)
  numSubs <-  length(unique(dataTable$analystId[dataTable$subscription == 1]))
  free <-  unique(dataTable$analystId[dataTable$subscription == 0])
  freeComb <- combn(free, numSubs)
  
  freeComp <- data.table::rbindlist(apply(freeComb, 2, function(comb){
    # Calculate average projected points for each group
    subsGroup <- dataTable[analystId %in% comb, .(projectPoints = mean(points_proj, na.rm = TRUE)), by = c("positionId", "points_act", "playerId")]
    
    subAccuracy <- subsGroup[, .(rsq = summary(lm(points_act ~ projectPoints, data = subsGroup[positionId == unlist(.BY)]))$r.squared), by = "positionId"]
    subMase <- subsGroup[, .(mase = calculateMASE(subsGroup$projectPoints[subsGroup$positionId == unlist(.BY)], 
                                                  subsGroup$points_act[subsGroup$positionId == unlist(.BY)])), by = "positionId"]
    
    accTable <- merge(subAccuracy, subMase, by = "positionId")
    
    data.table::setnames(accTable, c("rsq", "mase"), c("R-Squared", "MASE"))
    accTable[, Position := names(posIds)[which(posIds == positionId)],  by = c("positionId")]
    

    return(accTable[, c("positionId", "Position",  "R-Squared", "MASE"), with = FALSE])
  }))
  
  resultTable <- freeComp[, c("R-Squared", "MASE") := list(mean(`R-Squared`, na.rm = TRUE), mean(MASE, na.rm = TRUE)), by = c("positionId", "Position")]
  
  
  return(unique(resultTable[, c("Position", "R-Squared", "MASE"), with = FALSE]))
}
