#' Calculate Projected Points
#'
#' Calculate projected fantasy points, confidence intervals, risk, tiers, etc.
#' @param scrapeData The scraped projections data from \link{runScrape}.
#' @param avgMethod A string specifying which average method to use for aggregating the
#' projections from different sources: mean ("average"), robust average ("robust"), or weighted average ("weighted"). Defaults to mean. Edit the analysts' weights for the weighted average in the \link{analysts} table.
#' @param leagueScoring List of scoring rules for the league see \link{scoringRules}
#' for an example.
#' @param vorBaseline The numbers (position rank values or point values) at each position to use for the baseline when
#' calculating VOR.
#' @param vorType Whether the baseline numbers are ranks or points. Defaults to position ranks.
#' @param teams Number of teams in the league (integer).
#' @param format League format ("standard" for standard leagues or "ppr" for Point-Per-Reception leagues).
#' @param mflMocks Whether to include mock drafts from MyFantasyLeague.com (MFL). Set to 1 to use only mock drafts,
#' 0 to use only real drafts. If not specified, all draft types will be used.
#' @param mflLeagues What type of leagues to include for MyFantasyLeague.com (MFL). Set to 0 to use
#' only redraft leagues; 1 to use only keeper leagues, 2 for rookie drafts, and
#' 3 for MFL Public Leagues. If not specified, all draft types will be used.
#' @param ADPsource Character vector with one or more of \code{c("CBS", "ESPN", "FFC", "MFL", "NFL")}.
#' @examples
#' getProjections(scrapeData,                    ## Based on data in scrapeData
#'                avgMethod = "weighted",        ## calculate the projections using a weighted average
#'                leagueScoring = scoringRules,  ## using defined scoringRules,
#'                vorBaseline, vorType,          ## VOR Baselines and types
#'                teams = 12, format = "ppr",    ## for a 12 team ppr league
#'                mflMocks = 0, mflLeagues = 0,  ## using only real MFL redraft league
#'                adpSources =  c("FFC", "MFL")) ## and ADP data from MFL and FFC
#' @export getProjections
getProjections <- function(scrapeData = NULL,
                           avgMethod = "average",
                           leagueScoring = scoringRules,
                           vorBaseline, vorType,
                           teams = 12, format = "standard", mflMocks = NULL,
                           mflLeagues = NULL,
                           adpSources =  c("CBS", "ESPN", "FFC", "MFL", "NFL"),
                           getADP = TRUE, getECR = TRUE, writeFile = TRUE)
  {
  # Run data scrape if scrapeData is not passed.
  if(is.null(scrapeData))
    scrapeData <- runScrape()

  getADP <- !is.null(adpSources) & getADP

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
  if(exists("analystId", allProjections))
    data.table::setnames(allProjections, "analystId", "analyst")
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

    kickerFGMiss <- redistributeValues(valueTable = allProjections[position == "K"],
                                   calcType = avgMethod,
                                   fromVar = "fgMiss",
                                   toVars = c("fgMiss0019", "fgMiss2029", "fgMiss3039", "fgMiss4049", "fgMiss50"),
                                   excludeAnalyst = NULL)

    allProjections <- merge(allProjections, kickerFGMiss[, names(allProjections), with = FALSE],
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
  projectedPoints <- projectPoints(allProjections, leagueScoring, avgType = avgMethod)
  avgProjections <- projectedPoints$avgStats
  projectedPoints <- projectedPoints$pointsTable
  # If seasonal data then we will calculate Value Over Replacement. We also
  # get the ADP data for the season
  if(week == 0){
    cat("Calculate VOR                                                      \r")
    projectedPoints[, vor := calculateVor(positionRank, points, unlist(.BY)),
                    by = "position"]

    projectedPoints[, overallRank := rank(-vor, ties.method = "min")]

    if(getADP){
      cat("Adding ADP data                                                    \r")
      draftValues <- getDraftData(adpSources, season, teams, format, mflMocks, mflLeagues)
      draftValues[, playerId := as.numeric(playerId)]
      projectedPoints <- merge(projectedPoints, draftValues[, c("playerId", "adp", "aav"),
                                                            with = FALSE],
                               by = "playerId", all.x = TRUE)
      projectedPoints[, adpDiff := overallRank - adp]
    }
  }

  # Adding player information
  cat("Adding player information                                            \r")
  players <- playerData[, c("playerId", "player", "team", "draftYear", "birthdate"),
                        with = FALSE]
  players[, playerId := as.numeric(playerId)]
  projectedPoints <- merge(players, projectedPoints, by = "playerId")
  projectedPoints[, exp := season - as.numeric(draftYear)]

  projectedPoints[, c("draftYear") := NULL]

  playerColumns <- c("playerId", "player", "team", "position", "birthdate")
  dataColumns <- names(projectedPoints)[which(!(names(projectedPoints) %in% playerColumns))]
  projectedPoints <- projectedPoints[, c(playerColumns, dataColumns), with = FALSE]

  if(getECR){
    # Retrieving expert ranking information
    if(format == "ppr"){
      rankFormat = "ppr"
    } else {
      rankFormat = "std"
    }

    cat("Retrieving ECR ranks for position                                    \r")
    rankTable <- data.table::rbindlist(lapply(scrapePositions, getRanks,
                                              leagueType = rankFormat,
                                              weekNo = week), fill = TRUE)


    rankTable <- rankTable[, c("player", "position", "team", "avgRank", "sdRank"),
                           with = FALSE]


    data.table::setnames(rankTable, "avgRank", "ecrPosition")


    cat("Retrieving overall ECR ranks                                         \r")
    overallRanks <- getRanks("consensus", leagueType = rankFormat, weekNo = week)
    overallRanks <- overallRanks[, c("player", "position", "team", "avgRank"),
                                 with = FALSE]

    data.table::setnames(overallRanks, "avgRank", "ecrRank")

    rankTable <- merge(rankTable, overallRanks, by = c("player", "position", "team"))

    # Adding ranking info to table
    projectedPoints <- merge(projectedPoints, rankTable,
                             by = c("player", "position", "team"), all.x = TRUE)

    # Caluclate risk
    cat("Calculating risk                                                     \r")
    projectedPoints[, risk := calculateRisk(sdPts, sdRank), by = "position"]
  }

  if(exists("vor", projectedPoints))
    projectedPoints <- projectedPoints[order(-vor)]

  if(writeFile){
    write.csv(projectedPoints, file = "projectedPoints.csv", row.names = FALSE,
              na = "")
    projectedPoints <- dataGadget(projectedPoints)
  }

  avgTbl <- lapply(split(avgProjections, avgProjections$position),
                   data.table::dcast,
                   as.formula("playerId + position ~ dataCol"))

  avgData <- lapply(names(avgTbl),
                    function(p)ffanalytics::dataResult(resultData = avgTbl[[p]],
                                                       position = p))
  names(avgData) <- names(avgTbl)
  avgData$period <- scrapeData$period
  return(list(scrape = scrapeData,
              avgStats = avgData,
              projections = projectedPoints))
}
