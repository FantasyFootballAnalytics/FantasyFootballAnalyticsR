#' ADP data from FantasyFootballCalculator.com
#'
#' Retrieve ADP data from fantasyfootballcalculator.com.
#' @param format Format of league. Can be one of \code{"standard", "ppr", "2qb", "dynasty", "rookie"}.
#' @param teams Numer of teams in the league. One of 8, 10, 12, 14
#' @return \link[data.table]{data.table} with 5 columns:
#' \describe{
#'  \item{player}{Name of player}
#'  \item{position}{Player position}
#'  \item{team}{Team the player is playing for}
#'  \item{adp}{Average ADP}
#'  \item{leagueType}{Chosen format for the league}}
#' @export getFFCValues
getFFCValues <- function(format = "standard", teams = 12){
  ffcFile <- url(paste("https://fantasyfootballcalculator.com/adp_csv.php?format=",
                       format, "&teams=", teams, sep = ""), open = "rt")

  ffcTbl <- data.table::data.table(read.csv(ffcFile, skip = 4, stringsAsFactors = FALSE))
  close(ffcFile)
  ffcTbl[, Name := getPlayerName(Name)]
  ffcTbl[Position == "PK", Position := "K"]
  ffcTbl[Position == "DEF", Position := "DST"]
  ffcTbl[, ADP := NULL]
  data.table::setnames(ffcTbl, c("Overall", "Name", "Position", "Team"),
                       c("adp", "player", "position", "team"))

  ffcTbl[, leagueType := ifelse(format == "standard", "std", format)]
  return(ffcTbl[!is.na(adp), c("player", "position", "team", "adp", "leagueType"), with = FALSE])

}

#' ADP and auction value data from ESPN
#'
#' Retrieve ADP and auction value from ESPN
#' @return \link[data.table]{data.table} with 6 columns:
#' \describe{
#'  \item{player}{Name of player}
#'  \item{position}{Player position}
#'  \item{team}{Team the player is playing for}
#'  \item{adp}{Average ADP}
#'  \item{aav}{Average auction value}
#'  \item{leagueType}{Assuming standard league type}}
#' @export getESPNValues
getESPNValues <- function(){
  espnPos <- c("QB", "RB", "WR", "TE", "K", "D/ST", "DT", "DE", "CB", "S", "LB")
  espnData <- lapply(espnPos, function(p){
    espnUrl <-paste("http://games.espn.go.com/ffl/livedraftresults?position=", p, sep ="")
    espnTbl <- data.table::data.table(XML::readHTMLTable(espnUrl, which = 2,
                                                         skip.rows=c(1,2), stringsAsFactors = FALSE))
    emptyTbl <- data.table(rank = NA)
    emptyTbl[, c("rank", "player", "position", "team", "adp", "snake7day", "aav",
                 "value7day", "pctOwn", "leagueType") := NA]
    emptyTbl[, c("player", "position", "team") := as.character(NA)]
    if(nrow(espnTbl) <= 1)
      return(emptyTbl[0])
    data.table::setnames(espnTbl, c(1:8), c("rank", "player", "position", "adp",
                                            "snake7day", "aav", "value7day", "pctOwn"))
    espnTbl[, player := gsub(", Wsh", ", WAS", player, fixed = TRUE)]
    playerTeams <- toupper(espnTbl$player)
    playerTeams <- sapply(nflTeam.abb, function(nt)stringr::str_extract(playerTeams, nt))
    playerTeams <- apply(playerTeams, 1, function(pr)pr[which(!is.na(pr))])
    playerTeams <- lapply(playerTeams, function(pr)ifelse(length(pr) == 0, "FA", pr))
    playerTeams <- unlist(playerTeams)
    espnTbl$team <- playerTeams
    if(!exists("team", espnTbl)){
      espnTbl$team <- ""
    }
    espnTbl[, player := getPlayerName(encodeString(player))]
    espnTbl <- espnTbl[!is.na(player)]
    espnTbl[position == "D/ST", position := "DST"]
    espnTbl[position %in% c("DT", "DE") , position := "DL"]
    espnTbl[position %in% c("CB", "S") , position := "DB"]
    espnTbl[, leagueType := "std"]

    return(espnTbl)
  })
  espnData <- data.table::rbindlist(espnData, fill = TRUE)

  if(any(espnData$position == "DST") & nrow(espnData) > 0){
    espnData[position == "DST", team := nflTeam.abb[nflTeam.name == player], by = "player"]
  }
  return(espnData[,c("player", "position", "team", "adp","aav", "leagueType"), with = FALSE])
}


#' ADP data from CBS
#'
#' Retrieve ADP data from CBS
#' @return \link[data.table]{data.table} with 3 columns:
#' \describe{
#'  \item{cbsId}{Player ID from CBS. Merge with results from \link{getPlayerData}
#'  to get player names}
#'  \item{adp}{Average ADP}
#'  \item{leagueType}{Assuming standard league type}}
#' @export getCBSValues
getCBSValues <-function(){
  cbsVal <- data.table::data.table(XML::readHTMLTable("http://www.cbssports.com/fantasy/football/draft/averages?&print_rows=9999", which = 1, stringsAsFactors = FALSE, skip.rows = 1))
  pgeLinks <- XML::getHTMLLinks("http://www.cbssports.com/fantasy/football/draft/averages?&print_rows=9999")
  pId <- as.numeric(unique(gsub("[^0-9]", "", pgeLinks[grep("/fantasy/football/players/[0-9]{3,6}", pgeLinks)])))
  data.table::setnames(cbsVal, c(1:6), c("rank", "player", "trend", "adp", "HiLo", "pctOwn"))
  cbsVal <- cbsVal[!is.na(player)]
  cbsVal[, player := getPlayerName(player)]
  cbsVal[, cbsId := pId]
  cbsVal[, leagueType := "std"]
  cbsVal <- cbsVal[, c("cbsId", "adp", "leagueType"), with = FALSE]
  return(cbsVal)
}

#' ADP data from FantasyFootballCalculator.com
#'
#' Retrieve ADP data from fantasyfootballcalculator.com.
#' @return \link[data.table]{data.table} with 5 columns:
#' \describe{
#'  \item{esbid}{Player ID for the player. Merge with results from \link{getPlayerData}
#'  to get player names}
#'  \item{player}{Name of player}
#'  \item{position}{Player position}
#'  \item{adp}{Average ADP}
#'  \item{aav}{Average auction value}
#'  }
#' @export getNFLValues
getNFLValues <- function(){
  pCount <- 0
  nflTable <- data.table::data.table(esbid = as.character(),
                                     gsisPlayerId = as.character(),
                                     firstName = as.character(),
                                     lastName = as.character(),
                                     rank = as.numeric(),
                                     aav = as.numeric(),
                                     position = as.character())
  repeat{
    nflUrl <- paste0("http://api.fantasy.nfl.com/v1/players/userdraftranks?offset=",
                     pCount,"&count=100")
    nfldata <- XML::xmlToList(nflUrl)
    if(!any(names(nfldata) == "player"))
      break

    nfldata <- nfldata[!(names(nfldata) == ".attrs")]
    nfldata <- data.table::rbindlist(lapply(nfldata, function(pl)data.table::as.data.table(as.list(pl))))
    nflTable <- data.table::rbindlist(list(nflTable, nfldata))
    pCount <- pCount + 100
  }
  nflTable[, player := getPlayerName(paste(firstName, lastName))]
  data.table::setnames(nflTable, "rank", "adp")
  nflTable[position == "DEF", position := "DST"]
  return(nflTable[, c("esbid", "player", "position", "adp", "aav"), with = FALSE])
}

#' ADP and auction value data from Yahoo
#'
#' Retrieve ADP and auction value data from Yahoo
#' @param type Draft type: AD for auction draft, SD for standard draft.
#' @return \link[data.table]{data.table} with 3 columns:
#' \describe{
#'  \item{cbsId}{Player ID from CBS. Merge with results from \link{getPlayerData}
#'  to get player names}
#'  \item{adp/aav}{Average ADP if type = "SD"; Average auction value if type = "AD"}
#'  \item{leagueType}{Assuming standard league type}}
#' @export getYahooValues
getYahooValues <- function(type = "SD"){
  if(type == "SD"){
    yahooPos <- list(QB = c(0), RB = c(0), WR =c(0,50), TE = c(0), K = c(0), DST = c(0))
  }
  if(type == "AD"){
    yahooPos <- list(QB = c(0, 50, 100), RB = seq(0, 200, 50), WR =seq(0, 350, 50), TE = seq(0, 200, 50), K = c(0, 50), DST = c(0))
  }

  yahooDataUrls <- lapply(names(yahooPos), function(pos){

    yahooUrl <- paste("http://football.fantasysports.yahoo.com/f1/draftanalysis?tab=", type, "&pos=", pos, "&sort=DA_AP&count=", sep = "")
    yahooPgs <- lapply(yahooPos[[pos]], function(pg)data.table::data.table(url = paste(yahooUrl, pg, sep = "")))

    if(length(yahooPgs) > 1){
      yahooPgs <- data.table::rbindlist(yahooPgs)
    }
    else{
      yahooPgs <- yahooPgs[[1]]
    }

    yahooPgs[, position := pos]
    return(yahooPgs)
  })

  yahooDataUrls <- data.table::rbindlist(yahooDataUrls)
  yahooData <- apply(yahooDataUrls, 1, function(u){
    data <- data.table::data.table(XML::readHTMLTable(u["url"], stringsAsFactors = FALSE)$draftanalysistable)
    pgeLinks <- XML::getHTMLLinks(u["url"])

    yahooId <- as.numeric(unique(gsub("[^0-9]", "",
                                       pgeLinks[grep("http://sports.yahoo.com/nfl/players/[0-9]{3,6}", pgeLinks)])))

    if(nrow(data) > 0){
      data[, position := u["position"]]
      data <- data.table::data.table(yahooId, data)
      return(data)
    }
  })
  yahooData <- data.table::rbindlist(yahooData)
  if(type == "SD"){
    data.table::setnames(yahooData, c("yahooId", "player", "adp", "avgRound", "pctDraft", "position"))
  }
  if(type == "AD"){
    data.table::setnames(yahooData, c("yahooId", "player", "aav", "avgCost", "pctDraft", "position"))
    yahooData[, aav := as.numeric(gsub("$", "", aav, fixed = TRUE))]
    yahooData[, avgCost :=  as.numeric(gsub("$", "", avgCost, fixed = TRUE))]
  }

  yahooData[, leagueType := "std"]
  return(yahooData[,c("yahooId", ifelse(type == "SD", "adp", "aav"), "leagueType"), with = FALSE])
}

#' ADP and auction value data from MyFantasyLeague.com
#'
#' Retrieve ADP and auction value data from MyFantasyLeague.com
#' @param season Year the data is retrieved for
#' @param type One of "adp" or "aav" to indicate whether ADP or auction values
#' should be retrieved.
#' @param teams Number of teams. If specified only drafts with that number of
#' teams will be include
#' @param ppr Specify if only ppr or non-ppr drafts should be considered. Set to 1
#' if only ppr drafts should be used, 0 if only standard drafts should be used.
#' If not specified all types of draft will be used.
#' @param mock Specify if only mock or real drafts should be used. Set to 1 if
#' only mock drafts should be used, 0 if only real drafts should be used. If not
#' speficied all types of drafts will be used.
#' @param keeper Specify to select what types of leagues should be used. Set to 0
#' to use redraft leagues only; 1 to only use keeper leagues, 2 for rookie drafts,
#' and 3 for MFL Public Leagues. If not  speficied all types of drafts will be used.
#' @return \link[data.table]{data.table} wih up to 5 columns:
#' \describe{
#'  \item{mflId}{Player ID from MFL Merge with results from \link{getPlayerData}
#'  to get player names}
#'  \item{selectedIn}{Number of drafts player has been selected in}
#'  \item{aav}{Average auction value}
#'  \item{adp}{ADP}
#'  \item{minPick}{Earliest pick}
#'  \item{maxPick}{Latest pick}
#'  }
#' @export getMFLValues
getMFLValues <- function(season = as.POSIXlt(Sys.Date())$year + 1900,
                         type = "adp", teams = -1, ppr = -1, mock = -1, keeper = -1){
  url_list <- list(scheme = "http",
       hostname = "www03.myfantasyleague.com",
       path = paste(season, "export", sep = "/"),
       query = list(TYPE = type, FRANCHISES = teams, IS_PPR = ppr,
                    IS_MOCK = mock, IS_KEEPER = keeper))
  attr(url_list, "class") <- "url"

  if(teams == -1)
    url_list$query$FRANCHISES <- NULL
  if(ppr == -1)
    url_list$query$IS_PPR <- NULL
  if(mock == -1)
    url_list$query$IS_MOCK <- NULL
  if(keeper == -1)
    url_list$query$IS_KEEPER <- NULL

  mfl_url <- httr::build_url(url_list)
  MFL_adp <- XML::xmlToList(mfl_url)
  MFL_adp <- data.table::rbindlist(lapply(MFL_adp[names(MFL_adp) == "player"],
                                          function(plList)data.table::as.data.table(as.list(plList))))
  if(exists("id", MFL_adp))
    data.table::setnames(MFL_adp, "id", "mflId")
  if(exists("draftsSelectedIn", MFL_adp))
    data.table::setnames(MFL_adp, "draftsSelectedIn", "selectedIn")
  if(exists("auctionsSelectedIn", MFL_adp))
    data.table::setnames(MFL_adp, "auctionsSelectedIn", "selectedIn")
  if(exists("averageValue", MFL_adp))
    data.table::setnames(MFL_adp, "averageValue", "aav")
  if(exists("averagePick", MFL_adp))
    data.table::setnames(MFL_adp, "averagePick", "adp")
  if(length(MFL_adp) == 0)
    MFL_adp <- data.table::data.table(mflId = NA, adp = NA)[0]
  return(MFL_adp)
}


#' @export getYahooDraftData
getYahooDraftData <- function(season, league){
  league.key <- getItemKey(season, league, "league")
  start.num <- 0
  draftTable <- data.table::data.table(yahooId = as.numeric(),
                                       adp = as.numeric(), avgRd = as.numeric(),
                                       aav = as.numeric(), pctDraft = as.numeric())
  repeat{
    draft.query <- list(start = start.num)
    draftUrl <- apiUrl(resource = "league", resourceId = league.key,
                       subResource = "players/draft_analysis",
                       queryParams = draft.query)

    draftData <- callAPI(draftUrl)
    if(length(draftData$league[[2]]$players) == 0)
      break

    draft.data <- draftData$league[[2]]$players
    draft.data <- draft.data[!(names(draft.data) == "count")]
    draftInfo <- data.table::rbindlist(
      lapply(draft.data, function(pl){

        playerInfo <- as.numeric(unlist(pl[["player"]][[2]][["draft_analysis"]]))
        playerInfo <- data.table::as.data.table(t(playerInfo))
        data.table::setnames(playerInfo, c("average_pick", "average_round",
                                          "average_cost", "percent_drafted"))

        playerInfo[, yahooId := as.numeric(pl[["player"]][[1]][[2]][["player_id"]])]
        return(playerInfo)
      }), fill = TRUE
    )

    data.table::setnames(draftInfo, c("average_pick", "average_round",
                                      "average_cost", "percent_drafted"),
                         c("adp", "avgRd", "aav", "pctDraft"))

    draftTable <- data.table::rbindlist(list(draftTable, draftInfo), fill = TRUE)
    start.num <- start.num + 25
  }
  draftTable <- draftTable[!is.na(adp) & !is.na(aav)]
  return(draftTable[order(adp)])
}
