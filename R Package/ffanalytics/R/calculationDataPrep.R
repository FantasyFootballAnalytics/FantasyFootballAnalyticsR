#' Melt data into long form
#'
#' Takes the data result with a projected stat in each column and melts the
#' data into one row per player per stat column.
#' @param dataResult A \link{dataResult} object from the data scrape
#' @export getMeltedData
getMeltedData <- function(dataResult){
  statData <- dataResult@resultData
  idVars = intersect(names(statData), c("playerId", "player", "position", "analyst", "analystId"))
  # Variable columns are the remaining columns not identified by the idVars
  varCols <- setdiff(names(statData), idVars)
  # Make sure all variable columns are numric

  statData <- statData[, lapply(.SD, as.numeric), .SDcols = varCols, by = idVars]

  proj <- data.table::melt.data.table(statData, id.vars = idVars,
                                      variable.name = "dataCol" ,
                                      measure.vars = varCols)
  return(proj)
}

#' Calculate average based on selected method
#'
#' Will calculate average of provided data and weights based on the selected
#' method
#' @param calcMethod One of \code{c("average", "weighted", "robust")}
#' @param dataValue A numeric vector of values to average base on chosen calculation
#' method.
#' @param dataWeights A numeric vector of weight values associated with the
#' dataValue parameter.
#' @param na.rm A logical value determining if NA values should be removed.
#' @export avgValue
avgValue <- function(calcMethod = "weighted", dataValue = as.numeric(),
                     dataWeights = as.numeric(), na.rm = FALSE){
  allowedMethods <- c("average", "weighted", "robust")
  calcMethod <- match.arg(calcMethod, allowedMethods)
  # Function to calculate the location estimate for the wilcox test
  # If there are more than 2 observations then the function will return the
  # median of the paired averages. If there are 2 or less observation a simple
  # mean will be returned.
  wilcox.loc <- function(vec, na.rm = FALSE){
    n <- length(vec)
    # If number of observations is less than 2 then we just return mean as location estimate
    if(n <= 2){
      return(mean(vec, na.rm = na.rm))
    }

    # Calculating the paired avagerages
    pairAvg <- sort(c(vec, combn(vec, 2, function(x)mean(x, na.rm = na.rm))))
    return(median(pairAvg, na.rm = na.rm))
  }

  # Checking to see if any weights have been passed. If not then we create a weights vector
  # consisting of 1s
  if(length(dataWeights) == 0)
    dataWeights <- rep(1, length(dataValue))

  # Determining which average to use.
  avgFunction <- switch(calcMethod,
                        "average" = mean,
                        "weighted" = weighted.mean,
                        "robust" = wilcox.loc)

  # If weighted average is requested then pass weights. Don't pass weights if not.
  if(calcMethod == "weighted"){
    returnValue <- avgFunction(as.numeric(dataValue), dataWeights, na.rm = TRUE)
  } else {
    returnValue <- avgFunction(as.numeric(dataValue), na.rm = TRUE)
  }

  return(returnValue)
}

#' Redistribute values
#'
#' Allows for the redistribution of values from \bold{one} variable to a set of
#' others based on the averages. For example, the function can be used to
#' redistribute total field goals to field goals per distances based of what
#' the average values are for each of the field goals per distance.
#' @param valueTable A \link[data.table]{data.table}. Assumes outout from the
#' \link{getMeltedData} function.
#' @param calcType A string specifying which calculation method to use for the
#' average values
#' @param fromVar A string specifying the name of the variable to distribute from
#' @param toVars A character vector with the names of variables to distribute to
#' @param excludeAnalyst An integer indicating an analyst to exclude. This will
#' exclude the analyst from the averages
#' @export redistributeValues
redistributeValues <- function(valueTable = data.table(), calcType = "weighted",
                               fromVar = "fg",
                               toVars = c("fg0019", "fg2029", "fg3039", "fg4049", "fg50"),
                               excludeAnalyst = 20){

  emptyTable <- data.table::data.table(playerId = as.numeric(NA),
                                       player = as.character(NA),
                                       analyst = as.numeric(NA),
                                       position = as.character(NA),
                                       dataCol = as.character(NA),
                                       value = as.numeric(NA),
                                       weight = as.numeric(NA))

  if(!("weight" %in% names(valueTable))){
    if(calcType == "weighted")
      warning("calcType is weighted but no weights specified. Using default weights", call. = FALSE)
    analystWeights <- analysts[, c("analystId", "weight"), with = FALSE]
    setnames(analystWeights, "analystId", "analyst")
    valueTable <- merge(valueTable, analystWeights, by = "analyst")
  }


  if(length(excludeAnalyst) > 1)
    stop("Only specify one analyst to exclude", call. = FALSE)

  if(length(fromVar) != 1)
    stop("Please specify one variable to distribute from", call. = FALSE)

  allAnalysts <- valueTable[["analyst"]]
  allVars <- valueTable[["dataCol"]]

  if(any(allAnalysts == excludeAnalyst)){
    fromSelection <- which(allAnalysts == excludeAnalyst & allVars == fromVar)
    toSelection <- which(allAnalysts !=  excludeAnalyst & allVars %in% toVars)
  } else {
    fromSelection <- which(allVars == fromVar)
    toSelection <- which(allVars %in% toVars)
  }

  if(length(toSelection) == 0)
    return(emptyTable[0])
  fromTable <- valueTable[fromSelection,]
  fromTable[, dataCol := NULL]

  avgTable <- valueTable[toSelection,
                         .(totalValue = avgValue(calcMethod = calcType, dataValue = .SD[[1]],
                                                 dataWeights = .SD[[2]], na.rm = TRUE)),
                         by = c("playerId", "dataCol", "position"),
                         .SDcols = c("value", "weight")]

  allAvgVars <- avgTable[["dataCol"]]

  avgTable[which(allAvgVars %in% toVars), totalVar := sum(totalValue, na.rm = TRUE),
           by = c("playerId", "position")]

  avgTable[which(allAvgVars %in% toVars) & totalVar != 0 , varShare := totalValue / totalVar]

  toTable <- merge(avgTable, fromTable, suffixes = c("", "_from"),
                   by = c("playerId", "position"), allow.cartesian = TRUE)
  toTable <- toTable[!is.na(totalValue)]

  allToVars <- toTable[["dataCol"]]
  fromValues <- toTable[["value"]]

  toTable[which(allToVars %in% toVars), value := fromValues * ifelse(is.na(varShare), 0, varShare)]

  toCols <- intersect(names(toTable),c("playerId", "player", "analyst",
                                       "position", "dataCol", "value", "weight"))

  toTable <- toTable[, toCols, with = FALSE]

  return(toTable)
}

#' Find replacement data for missing values
#'
#' For analysts that don't report on certain values the averages across other
#' analysts are calculated so they can be imputed.
#' @param statData A \link[data.table]{data.table}. Assumes outout from the
#' \link{getMeltedData} function.
#' @param calcType A string specifying which calculation method to use for the
#' average values
#' @export replaceMissingData
replaceMissingData <- function(statData = data.table(), calcType = "weighted"){

  if(!("weight" %in% names(statData))){
    if(calcType == "weighted")
      warning("calcType is weighted but no weights specified. Using default weights", call. = FALSE)
    analystWeights <- analysts[, c("analystId", "weight"), with = FALSE]
    data.table::setnames(analystWeights, "analystId", "analyst")
    statData <- merge(statData, analystWeights, by = "analyst")
  }

  missVars <- c("analyst", "position", "dataCol")
  avgVars <- c("playerId", "position", "dataCol")

  # Finding all the data that is missing for analysts
  missData <- statData[, .(missTest = all(is.na(.SD))), by = missVars ,
                       .SDcols = "value"][missTest == TRUE, missVars, with = FALSE]

  # Calculating the average for each of the variables for each player
  avgData <- statData[, .(replValue = avgValue(calcMethod = calcType,
                                               dataValue = .SD[[1]],
                                               dataWeights = .SD[[2]],
                                               na.rm = TRUE)),
                      by = avgVars, .SDcols = c("value", "weight")]
  avgData <- avgData[is.finite(replValue)]
  mergeVar <- intersect(missVars, avgVars)

  return(merge(missData, avgData, by = mergeVar, allow.cartesian = TRUE))
}


#' @export dualPositionData
dualPositionData <- function(scrapeData){

  ## Combining data for dual position players
  offpos <- intersect(names(scrapeData), c("QB", "RB", "WR", "TE"))
  defpos <- intersect(names(scrapeData), c("DL", "DB", "LB"))

  if(length(defpos) > 1 & length(offpos) > 1){
    posComb <- cbind(combn(offpos, 2), combn(defpos, 2))
  }

  if(length(defpos) == 0 & length(offpos) > 1){
    posComb <- combn(offpos, 2)
  }

  if(length(defpos) > 1 & length(offpos) == 0){
    posComb <- combn(defpos, 2)
  }

  if(exists("posComb")){
    copyData <- apply(posComb,2, function(comb){
      data1 <- scrapeData[comb[1]]@resultData
      data2 <- scrapeData[comb[2]]@resultData
      commonPlayers <- intersect(data1$playerId, data2$playerId)
      newData1 <- data.table()
      newData2 <- data.table()
      if (length(commonPlayers) >0){
        commonData <- intersect(names(data1), names(data2))
        for(pl in commonPlayers){
          addSources1 <- setdiff(data1$analystId[data1$playerId == pl], data2$analystId[data2$playerId == pl])
          addSources2 <- setdiff(data2$analystId[data2$playerId == pl], data1$analystId[data1$playerId == pl])

          if(length(addSources1) >0 ){
            addData1 <- posProj[[comb[1]]][playerId == pl & analystId %in% addSources1, commonData, with = FALSE]
            newData2<- rbindlist(list(newData2, addData1), use.names = TRUE, fill = TRUE)
          }
          if(length(addSources2) >0 ){
            addData2 <- posProj[[comb[2]]][playerId == pl & analystId %in% addSources2, commonData, with = FALSE]
            newData1<- rbindlist(list(newData1, addData2), use.names = TRUE, fill = TRUE)
          }

        }
        result = list(newData1, newData2)
        names(result) <- c(comb[1],comb[2])
        return(result)
      }
    }
    )

    appendData <- lapply(names(scrapeData), function(pos)rbindlist(lapply(copyData, function(dt)dt[[pos]]), fill = TRUE))

    return(appendData)
  }

}
