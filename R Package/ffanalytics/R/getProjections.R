#' Calculate Projected Points
#'
#' Calculate projected fantasy points, confidence intervals, risk, tiers, etc.
#' @param scrapeData The scraped projections data from \link{runScrape}.
#' @param avgMethod Which average method should be used for aggregating the
#' projections from different sources.
#' @param leagueScoring List of scoring rules for the league see \link{scoringRules}
#' for an example
#' @param teams Number of teams in the league (integer)
#' @param format League format
#' @param mflMocks Include mock drafts from MFL. Set to 1 if only mock drafts
#' should be used, 0 if only real drafts should be used. If not speficied all
#' types of drafts will be used.
#' @param mflLeagues What type of leagues to include for MFL. Set to 0 to use
#' redraft leagues only; 1 to only use keeper leagues, 2 for rookie drafts, and
#' 3 for MFL Public Leagues. If not speficied all types of drafts will be used.
#' @param ADPsource Character vector with one or more of \code{c("CBS", "ESPN", "FFC", "MFL", "NFL")}
#' @export getProjections
getProjections <- function(scrapeData = NULL,
                           avgMethod = "average",
                           leagueScoring = scoringRules,
                           teams = 12, format = "standard", mflMocks = NULL,
                           mflLeagues = NULL,
                           adpSources =  c("CBS", "ESPN", "FFC", "MFL", "NFL"))
  {
  # Run data scrape if scrapeData is not passed.
  if(is.null(scrapeData))
    scrapeData <- runScrape()
  scrapePositions <- intersect(position.name, names(scrapeData))
  scrapePeriod <- scrapeData$period
  week <- scrapePeriod["weekNo"]
  season <- scrapePeriod["season"]
  scrapeAnalysts <- scrapeData$analysts
  cat("\n")
  # Add on weights for analysts
  analystWeights <- analysts[analystId %in% scrapeAnalysts, c("analystId", "weight"), with = FALSE]
  data.table::setnames(analystWeights, "analystId", "analyst")

  allProjections <- data.table::rbindlist(lapply(scrapeData[scrapePositions],
                                                 getMeltedData), fill = TRUE)
  allProjections <- merge(allProjections, analystWeights, by = "analyst")

  # Redistributing WalterFootballData
  if(any(allProjections$analyst == 20)){
    cat("Redistributing WalterFootball Data                                 \r")
    wafoKicker <- redistributeValues(valueTable = allProjections[position == "K"],
                                     calcType = avgMethod,
                                     fromVar = "fg0039",
                                     toVars = c("fg0019", "fg2029", "fg3039"),
                                     excludeAnalyst = 20)

    allProjections <- allProjections[!(analyst== 20 & dataCol %in% c("fg0019", "fg2029", "fg3039"))]
    allProjections <- data.table::rbindlist(list(allProjections, wafoKicker[, names(allProjections), with = FALSE]))
    allProjections <- allProjections[dataCol != "fg0039"]

    wafoTDs <- redistributeValues(valueTable = allProjections[position %in% c("RB", "WR", "TE")],
                                  calcType = avgMethod,
                                  fromVar = "regTds", toVars = c("rushTds", "recTds"),
                                  excludeAnalyst = 20)

    allProjections <- allProjections[!(analyst == 20 & dataCol %in% c("rushTds", "recTds"))]
    allProjections <- rbindlist(list(allProjections, wafoTDs[, names(allProjections), with = FALSE]))

    allProjections[position == "QB" & dataCol == "regTds", dataCol := "rushTds"]

    allProjections <- allProjections[dataCol != "regTds"]
  }

  # Redistributing Field goals to field goals by distance if these are missing
  if(any(allProjections$position == "K")){
    cat("Redistributing Field Goal Data                                     \r")
    kickerFG <- redistributeValues(valueTable = allProjections[position == "K"],
                                   calcType = avgMethod,
                                   fromVar = "fg",
                                   toVars = c("fg0019", "fg2029", "fg3039", "fg4049", "fg50"),
                                   excludeAnalyst = NULL)

    allProjections <- merge(allProjections, kickerFG[, names(allProjections), with = FALSE],
                            by = c("playerId", "analyst", "position", "dataCol"),
                            suffixes = c("", "_new"), all.x = TRUE)

    allProjections[is.na(value) & !is.na(value_new), value := value_new]

    allProjections[, (names(allProjections)[grep("_new", names(allProjections))]) := NULL]
  }

  # Replacing data that is missing for analysts
  cat("Replacing Missing Data                                               \r")
  missData <- replaceMissingData(statData = allProjections, calcType = avgMethod)

  allProjections <- merge(allProjections, missData,
                          by = c("playerId", "analyst", "position", "dataCol"),
                          all.x = TRUE)
  allProjections[!is.finite(value) & is.finite(replValue), value := replValue]
  allProjections[, replValue := NULL]

  # Calculate Points, Confidence intervals, standard deviation, and position ranks
  cat("Calculating Points                                                   \r")
  projectedPoints <- projectPoints(allProjections, scoringRules, avgType = avgMethod)

  # If seasonal data then we will calculate Value Over Replacement. We also
  # get the ADP data for the season
  if(week == 0){
    cat("Calculate VOR                                                      \r")
    projectedPoints[, vor := calculateVor(positionRank, points, unlist(.BY)),
                    by = "position"]
    projectedPoints[, overallRank := rank(-vor, ties.method = "min")]

    cat("Adding ADP data                                                    \r")
    adpValues <- getADPdata(adpSources, season, teams, format, mflMocks, mflLeagues)
    adpValues[, playerId := as.numeric(playerId)]
    projectedPoints <- merge(projectedPoints, adpValues[, c("playerId", "adp"),
                                                        with = FALSE],
                             by = "playerId", all.x = TRUE)
    projectedPoints[, adpDiff := overallRank - adp]
  }

  # Adding player information
  cat("Adding player information                                            \r")
  players <- playerData[, c("playerId", "player", "team", "draftYear", "birthdate"),
                        with = FALSE]
  players[, playerId := as.numeric(playerId)]
  projectedPoints <- merge(players, projectedPoints, by = "playerId")
  projectedPoints[, exp := season - as.numeric(draftYear)]

  projectedPoints[, c("playerId", "draftYear") := NULL]

  playerColumns <- c("player", "team", "position", "birthdate")
  dataColumns <- names(projectedPoints)[which(!(names(projectedPoints) %in% playerColumns))]
  projectedPoints <- projectedPoints[, c(playerColumns, dataColumns), with = FALSE]

  # Retrieving expert ranking information
  if(format == "ppr"){
    rankFormat = "ppr"
  } else {
    rankFormat = "std"
  }
  cat("Retrieving ECR ranks for position                                    \r")
  rankTable <- data.table::rbindlist(lapply(scrapePositions, getRanks,
                                            leagueType = rankFormat,
                                            weekNo = week))


  rankTable <- rankTable[, c("player", "position", "team", "ecrRank", "sdRank"),
                         with = FALSE]

  data.table::setnames(rankTable, "ecrRank", "ecrPosition")

  cat("Retrieving overall ECR ranks                                         \r")
  overallRanks <- getRanks("consensus", leagueType = rankFormat, weekNo = week)
  overallRanks <- overallRanks[, c("player", "position", "team", "ecrRank"),
                   with = FALSE]

  rankTable <- merge(rankTable, overallRanks, by = c("player", "position", "team"))

  # Adding ranking info to table
  projectedPoints <- merge(projectedPoints, rankTable,
                           by = c("player", "position", "team"), all.x = TRUE)

  # Caluclate risk
  cat("Calculating risk                                                     \r")
  projectedPoints[, risk := calculateRisk(sdPts, sdRank), by = "position"]

  # Set tiers
  cat("Setting tiers                                                        \r")
  if(week == 0){
    projectedPoints[, tier :=  setTier(points, unlist(.BY)), by = "position"]
  } else {
    projectedPoints[, tier :=  clusterTier(points, unlist(.BY)), by = "position"]
  }

  if(exists("vor", projectedPoints))
    projectedPoints <- projectedPoints[order(-vor)]
  write.csv(projectedPoints, file = "projectedPoints.csv", row.names = FALSE,
            na = "")

  dataGadget(projectedPoints)
  return(projectedPoints)
}



