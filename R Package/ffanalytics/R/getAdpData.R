#' Get ADP data
#'
#' Retrieve ADP data from multiple sources and combine into average.
#' @param ADPsource Character vector with one or more of
#' \code{"CBS", "ESPN", "FFC", "MFL", "NFL"}
#' @param season The season the ADP data is from
#' @param teams Number of teams in the league
#' @param format The format of the league, i.e. standard, ppr
#' @param mflMocks Include mock drafts from MFL. Set to 1 if only mock drafts
#' should be used, 0 if only real drafts should be used. If not speficied all
#' types of drafts will be used.
#' @param mflLeagues What type of leagues to include for MFL. Set to 0 to use
#' redraft leagues only; 1 to only use keeper leagues, 2 for rookie drafts, and
#' 3 for MFL Public Leagues. If not speficied all types of drafts will be used.
#' @export getADPdata
getADPdata <- function(ADPsources = c("CBS", "ESPN", "FFC", "MFL", "NFL"),
                       season =  as.POSIXlt(Sys.Date())$year + 1900,
                       teams = 12, format = "standard", mflMocks = NULL,
                       mflLeagues = NULL){

  adpData <- list()

  if("ESPN" %in% ADPsources){
    espnADP <- getESPNValues()[, c("player", "position", "team", "adp"), with = FALSE]
    espnADP <- merge(playerData[, c("playerId", "player", "position", "team"), with = FALSE],
                     espnADP, by = c("player", "position", "team"))
    adpData$ESPN <- espnADP
  }

  if("CBS" %in% ADPsources){
    cbsADP <- getCBSValues()[, c("cbsId", "adp"), with = FALSE]
    cbsADP[, cbsId := as.character(cbsId)]
    cbsADP <- merge(playerData[, c("playerId", "cbsId", "player", "position", "team"), with = FALSE],
                    cbsADP, by = "cbsId")
    cbsADP[, cbsId := NULL]
    adpData$CBS <- cbsADP
  }

  if("FFC" %in% ADPsources){
    ffcADP <- getFFCValues(format, teams)[, c("player", "position", "team", "adp"), with = FALSE]
    ffcADP <- merge(playerData[, c("playerId", "player", "position", "team"), with = FALSE],
                    ffcADP, by = c("player", "position", "team"))
    adpData$FFC <- ffcADP
  }


  if("NFL" %in% ADPsources){
    nflADP <- getNFLValues()[, c("esbid","player", "position", "adp"), with = FALSE]
    dstADP <- merge(playerData[position == "DST", c("playerId", "player", "position", "team"), with = FALSE],
                    nflADP[position == "DST"], by = c("player", "position") )
    nflADP[,  c("player", "position") := NULL]
    nflADP <- merge(playerData[, c("esbid", "playerId", "player", "position", "team"), with = FALSE],
                    nflADP, by = "esbid")
    nflADP <- data.table::rbindlist(list(nflADP, dstADP), fill = TRUE)

    nflADP[, esbid := NULL]
    adpData$NFL <- nflADP
  }

  if("MFL" %in% ADPsources){
    if(format == "standard")
      mflppr = 0
    if(format == "ppr")
      mflppr = 1
    if(is.null(mflMocks))
      mflMocks = -1
    if(is.null(mflLeagues))
      mflLeagues = -1
    mflADP <- getMFLValues(season, type = "adp", teams, ppr = mflppr,
                           mock = mflMocks, keeper = mflLeagues)[, c("mflId", "adp"),  with = FALSE]
    mflADP <- merge(playerData[, c("playerId", "mflId", "player", "position", "team"), with = FALSE],
                    mflADP, by = "mflId")
    mflADP[, mflId := NULL]
    adpData$MFL <- mflADP
  }

  calcAvg <- (length(adpData) > 1)
  adpData <- data.table::rbindlist(adpData, fill = TRUE)
  adpData[, adp := as.numeric(adp)]
  if(calcAvg){
    adpData <- adpData[, .(adp = mean(adp, na.rm = TRUE)),  by = c("playerId", "player", "position", "team")]
  }
  return(adpData[order(adp)])
}


#' Get AAV data
#'
#' Retrieve AAV data from multiple sources and combine into average.
#' @param ADPsource Character vector with one or more of
#' \code{"CBS", "ESPN", "FFC", "MFL", "NFL"}
#' @param season The season the ADP data is from
#' @param teams Number of teams in the league
#' @param format The format of the league, i.e. standard, ppr
#' @param mflMocks Include mock drafts from MFL. Set to 1 if only mock drafts
#' should be used, 0 if only real drafts should be used. If not speficied all
#' types of drafts will be used.
#' @param mflLeagues What type of leagues to include for MFL. Set to 0 to use
#' redraft leagues only; 1 to only use keeper leagues, 2 for rookie drafts, and
#' 3 for MFL Public Leagues. If not speficied all types of drafts will be used.
#' @export getAAVdata
getAAVdata <- function(AAVsources = c( "ESPN", "MFL", "NFL"),
                       season =  as.POSIXlt(Sys.Date())$year + 1900,
                       teams = 12, format = "standard", mflMocks = NULL,
                       mflLeagues = NULL){

  aavData <- list()

  if("ESPN" %in% AAVsources){
    espnAAV <- getESPNValues()[, c("player", "position", "team", "aav"), with = FALSE]
    espnAAV <- merge(playerData[, c("playerId", "player", "position", "team"), with = FALSE],
                     espnAAV, by = c("player", "position", "team"))
    aavData$ESPN <- espnAAV
  }

  if("NFL" %in% AAVsources){
    nflAAV <- getNFLValues()[, c("esbid","player", "position", "aav"), with = FALSE]
    dstAAV <- merge(playerData[position == "DST", c("playerId", "player", "position", "team"), with = FALSE],
                    nflAAV[position == "DST"], by = c("player", "position") )
    nflAAV[,  c("player", "position") := NULL]
    nflAAV <- merge(playerData[, c("esbid", "playerId", "player", "position", "team"), with = FALSE],
                    nflAAV, by = "esbid")
    nflAAV <- data.table::rbindlist(list(nflAAV, dstAAV), fill = TRUE)

    nflAAV[, esbid := NULL]
    aavData$NFL <- nflAAV
  }

  if("MFL" %in% AAVsources){
    if(format == "standard")
      mflppr = 0
    if(format == "ppr")
      mflppr = 1
    if(is.null(mflMocks))
      mflMocks = -1
    if(is.null(mflLeagues))
      mflLeagues = -1
    mflAAV <- getMFLValues(season, type = "aav", teams, ppr = mflppr,
                           mock = mflMocks, keeper = mflLeagues)[, c("mflId", "aav"),  with = FALSE]
    mflAAV <- merge(playerData[, c("playerId", "mflId", "player", "position", "team"), with = FALSE],
                    mflAAV, by = "mflId")
    mflAAV[, mflId := NULL]
    aavData$MFL <- mflAAV
  }

  calcAvg <- (length(aavData) > 1)
  aavData <- data.table::rbindlist(aavData, fill = TRUE)
  aavData[, aav := as.numeric(aav)]
  if(calcAvg){
    aavData <- aavData[, .(aav = mean(aav, na.rm = TRUE)),  by = c("playerId", "player", "position", "team")]
  }
  return(aavData[order(-aav)])
}

#' @export getDraftData
getDraftData <- function(draftSources = c("CBS", "ESPN", "FFC", "MFL", "NFL", "Yahoo"),
                       season =  as.POSIXlt(Sys.Date())$year + 1900,
                       teams = 12, format = "standard", mflMocks = NULL,
                       mflLeagues = NULL){

  draftData <- list()

  if("ESPN" %in% draftSources){
    espnDraft <- getESPNValues()[, c("player", "position", "team", "adp", "aav"), with = FALSE]
    espnDraft <- merge(playerData[, c("playerId", "player", "position", "team"), with = FALSE],
                       espnDraft, by = c("player", "position", "team"))
    draftData$ESPN <- espnDraft
  }

  if("CBS" %in% draftSources){
    cbsDraft <- getCBSValues()[, c("cbsId", "adp"), with = FALSE]
    cbsDraft[, cbsId := as.character(cbsId)]
    cbsDraft <- merge(playerData[, c("playerId", "cbsId", "player", "position", "team"), with = FALSE],
                    cbsDraft, by = "cbsId")
    cbsDraft[, cbsId := NULL]
    draftData$CBS <- cbsDraft
  }

  if("FFC" %in% draftSources){
    ffcADP <- getFFCValues(format, teams)[, c("player", "position", "team", "adp"), with = FALSE]
    ffcADP <- merge(playerData[, c("playerId", "player", "position", "team"), with = FALSE],
                    ffcADP, by = c("player", "position", "team"))
    draftData$FFC <- ffcADP
  }


  if("NFL" %in% draftSources){
    nflDraft <- getNFLValues()[, c("esbid","player", "position", "adp", "aav"), with = FALSE]
    dstDraft <- merge(playerData[position == "DST", c("playerId", "player", "position", "team"), with = FALSE],
                    nflDraft[position == "DST"], by = c("player", "position") )
    nflDraft[,  c("player", "position") := NULL]
    nflDraft <- merge(playerData[, c("esbid", "playerId", "player", "position", "team"), with = FALSE],
                      nflDraft, by = "esbid")
    nflDraft <- data.table::rbindlist(list(nflDraft, dstDraft), fill = TRUE)

    nflDraft[, esbid := NULL]
    draftData$NFL <- nflDraft
  }

  if("MFL" %in% draftSources){
    if(format == "standard")
      mflppr = 0
    if(format == "ppr")
      mflppr = 1
    if(is.null(mflMocks))
      mflMocks = -1
    if(is.null(mflLeagues))
      mflLeagues = -1
    mflADP <- getMFLValues(season, type = "adp", teams, ppr = mflppr,
                           mock = mflMocks, keeper = mflLeagues)[, c("mflId", "adp"),  with = FALSE]
    mflAAV <- getMFLValues(season, type = "aav", teams, ppr = mflppr,
                           mock = mflMocks, keeper = mflLeagues)[, c("mflId", "aav"),  with = FALSE]
    mflDraft <- merge(mflAAV, mflADP, all = TRUE, by = "mflId")
    mflDraft <- merge(playerData[, c("playerId", "mflId", "player", "position", "team"), with = FALSE],
                      mflDraft, by = "mflId")
    mflDraft[, mflId := NULL]
    draftData$MFL <- mflDraft
  }

  if("Yahoo" %in% draftSources){
    yahooDraft <- getYahooDraftData(season, yahooLeague)[, c("yahooId", "adp", "aav"), with = FALSE]
    yahooDraft[, yahooId := as.numeric(yahooId)]
    playerData[, yahooId := as.numeric(yahooId)]
    yahooDraft <- merge(playerData[, c("playerId", "yahooId", "player", "position", "team"), with = FALSE],
                        yahooDraft, by = "yahooId")
    yahooDraft[, yahooId:= NULL]
    draftData$Yahoo <- yahooDraft
  }

  calcAvg <- (length(draftData) > 1)
  draftData <- data.table::rbindlist(draftData, fill = TRUE)
  draftData[, adp := as.numeric(adp)]
  draftData[, aav := as.numeric(aav)]

  if(calcAvg){
    draftData <- draftData[, .(adp = mean(adp, na.rm = TRUE),
                               aav = mean(aav, na.rm = TRUE)),
                           by = c("playerId", "player", "position", "team")]
  }
  return(draftData[order(adp)])
}
