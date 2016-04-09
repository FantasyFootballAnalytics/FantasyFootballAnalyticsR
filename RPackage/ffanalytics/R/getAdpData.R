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
