#' Caluclate fantasy points
#'
#' Based on list of scoring rules calculate fantasy points using projection data
#' passed in a table
#' @param projectionData A \link[data.table]{data.table} with projected stats.
#' Should from the \link{getMeltedData} function
#' @param scoringRules A list of scoring rules with one element per position
#' @return A \link[data.table]{data.table} with up to 4 cloumns
#' \describe{
#'  \item{position}{The position the player is playing}
#'  \item{playerId}{The ID of the player. Get player names by merging results from
#'  \link{getPlayerData}}
#'  \item{analyst}{The ID of the analyst which projections are used as the basis
#'  for the points}
#'  \item{points}{The calculated number of fantasy points}
#' }
#' @export calculatePoints
calculatePoints <- function(projectionData = data.table(), scoringRules = list())
  {

  # Function to calculate DST points from the ptsAllowed brackets
  dstPts <- function(ptsAllow, brackets){

    # Making sure that the bracket are sorted by threshold value
    brackets <- brackets[order(threshold)]

    # if all the points allowed are over 100 then we assume it is seasonal data
    is.season <- all(ptsAllow > 100)
    if(is.season){
      ptsAllow <- ptsAllow / 16
    }

    # Creating a 0 vector to capture the points
    pts <- rep(0, length(ptsAllow))

    # Traversing through the bracket thresholds
    for(r in nrow(brackets):1){
      pts[ptsAllow <= brackets$threshold[r]] <- brackets$points[r]
    }

    if(is.season){
      pts <- pts * 16
    }
    return(as.numeric(pts))
  }

  # Pulling out the threshold brackets for the DST points allowed
  scoringBracket <- scoringRules[["ptsBracket"]]

  scoringNames <- names(scoringRules)[which(names(scoringRules) != "ptsBracket")]

  scoringRules <- lapply(scoringNames, function(sr){
    scoringRules[[sr]][, position :=  sr]
    return(scoringRules[[sr]])
  })


  # Combining the scoring rules for the positions
  scoringTbl <- data.table::rbindlist(scoringRules)

  # If points allowed is not in the table we will add a row
  if(!any(scoringTbl[["dataCol"]] == "dstPtsAllow")){
    addTbl <- data.table::data.table("DST", "dstPtsAllow", 0)
    data.table::setnames(addTbl, c("position", "dataCol", "multiplier"))
    scoringTbl <- data.table::rbindlist(list(scoringTbl, addTbl), fill = TRUE)
  }

  # merging projection data with the scoring rules
  scoreData <- merge(projectionData, scoringTbl, by = c("position", "dataCol"))

  # Calculating the points for the "multiplier"s
  scoreVars <- intersect(names(scoreData), c("playerId", "analyst", "position"))

  scoreData <- scoreData[,.(points = sum(.SD[[1]] * .SD[[2]], na.rm = TRUE)),
                         by = scoreVars, .SDcols = c("value", "multiplier")]


  # Calculating data for DST points allowed
  dstData <- projectionData[which(projectionData[["dataCol"]] == "dstPtsAllow")]
  dstData[, points := dstPts(value, scoringBracket)]
  dstData <- dstData[, c(scoreVars, "points"), with = FALSE]

  scoreData <- data.table::rbindlist(list(scoreData, dstData), fill = TRUE)
  scoreData <- scoreData[, .(points = sum(points, na.rm = TRUE)), by = scoreVars]
  return(scoreData)
}
