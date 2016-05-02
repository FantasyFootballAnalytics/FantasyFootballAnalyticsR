#' Calculate points
#'
#' For the scraped data, projected points, confidence interval, standard deviation
#' and position ranks are calculated
#' @param projectionData A \link[data.table]{data.table} with projected stats
#' @param scoringRules A list with tables for league scoring rules. See
#' \link{scoringRules} for reference on format
#' @param avgType Which average to use. Should be one of \code{average, robust, weighted}
#' @export projectPoints
projectPoints <- function(projectionData, scoringRules, avgType = "average"){


  sourcePoints <- calculatePoints(projectionData, scoringRules)
  aWeights <- unique(projectionData[, c("analyst", "weight"), with = FALSE])
  sourcePoints <- merge(sourcePoints, aWeights, by = "analyst")
  avgProjections <- projectionData[, .(value = avgValue(calcMethod = avgType,
                                                        dataValue = value,
                                                        dataWeights = weight, na.rm = TRUE)),
                                   by = c("playerId", "position", "dataCol")]

  projectedPoints <- calculatePoints(avgProjections, scoringRules)

  confInterval <- data.table::copy(sourcePoints)
  confInterval[, c("lower", "upper") := as.list(confidenceInterval(calcMethod = avgType,
                                                                   dataValue = points,
                                                                   dataWeights = weight, na.rm = TRUE)),
               by = c("playerId", "position")]

  confInterval <- unique(confInterval[, c("playerId", "position", "lower", "upper"), with = FALSE])
  ptsStdDev <- sourcePoints[, .(sdPts = calcStdDev(calcMethod = avgType,
                                                   dataValue = points,
                                                   dataWeights = weight, na.rm = FALSE)),
                            by = c("playerId")]
  confInterval <- merge(confInterval, ptsStdDev, by = "playerId")
  projectedPoints <- merge(projectedPoints, confInterval, by = c("playerId", "position"))
  projectedPoints[, positionRank := rank(-points, ties.method = "min"), by = "position"]
  projectedPoints[, dropoff := dropoffValue(points), by = "position"]
  projectedPoints[, tier := tierFunction(playerId, points, sourcePoints)$tier, by = "position"]
  return(projectedPoints[order(-points)])
}

#' Calculate Value Over Replacement
#'
#' Based on provided ranks and points calculate the value over replacement.
#' Function uses \link{vorBaseline} and \link{vorAdjustment} in the calculation.
#' Please adjust these to match your league before running calculation.
#' @param ranks Player ranks
#' @param points Player Points
#' @param position Player Position. Used to extract value from \link{vorBaseline}
#' and \link{vorAdjustment}
#' @export calculateVor
calculateVor <- function(ranks, points, position){
  vorValue <- vorBaseline[[position]]
  vorType <- vorType[[position]]
  if(vorType == "Rank"){
    vorBase <- mean(points[which(ranks >= vorValue - 1 & ranks <= vorValue + 1)],
                    na.rm = TRUE)
  } else {
    vorBase <- vorValue
  }
  return(points - vorBase)
}
