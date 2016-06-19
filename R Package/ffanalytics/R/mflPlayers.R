#' Read MFL Player Data
#'
#' Function to read player data from MFL using the MFL API
#' @param season The year that player data is to be retrieved from
#' @param weekNo The weekNo that the player data is to be retrieved from
#' @param pos A character vector with position names to be retrieved
#' @return A \link[data.table]{data.table} with 10 columns
#' \describe{
#'  \item{playerId}{NFL's ID for the player}
#'  \item{player}{Name of player}
#'  \item{yahooId}{Yahoo's ID for the player}
#'  \item{cbsId}{CBS's ID for the player}
#'  \item{mflId}{MFL's ID for the player}
#'  \item{position}{The position the player is playing}
#'  \item{team}{The team the player is playing for}
#'  \item{draftYear}{The year the player was drafted}
#'  \item{birthData}{The players birth date}
#'  \item{rookie}{A logical value indicating whether the player is a rookie}
#' }
#' @import XML
#' @import data.table
#' @export mflPlayers
mflPlayers <- function(season = 2016, weekNo = 0, pos = position.name){
  mflData <- XML::xmlToList(paste("http://football.myfantasyleague.com/", season,
                                  "/export?TYPE=players&L=&W=", weekNo, "&JSON=0&DETAILS=1", sep =""))
  mflData$.attrs <- NULL

  mflData <- data.table::rbindlist(lapply(mflData, function(pl)data.table::data.table(t(pl))), fill = TRUE)

    # Reducing the NFL IDs to be just numbers
  mflData[, nfl_id := gsub("[^0-9]", "", nfl_id)]

  # Updating team names
  mflData[team == "FA*", team := "FA"]
  mflData[team == "SFO", team := "SF"]
  mflData[team == "NOS", team := "NO"]
  mflData[team == "TBB", team := "TB"]
  mflData[team == "GBP", team := "GB"]
  mflData[team == "SDC", team := "SD"]
  mflData[team == "KCC", team := "KC"]
  mflData[team == "NEP", team := "NE"]
  mflData[team == "RAM", team := "LA"]

  # Updating position names
  mflData[position %in% c("DE", "DT"), position := "DL"]
  mflData[position %in% c("S", "CB"), position := "DB"]
  mflData[position == "PK", position := "K"]
  mflData[position == "Def", position := "DST"]

  # Set playerId for DST
  for(n in seq_along(nflTeam.id)){
    mflData[position == "DST" & team == nflTeam.abb[n], nfl_id := as.character(nflTeam.id[n])]
  }

  # Only keep players that we are actually projecting (excluding coaches and team positions)
  mflData <- mflData[position %in% pos]
  data.table::setnames(mflData, c("nfl_id", "stats_id", "cbs_id", "name", "id", "draft_year", "status"),
                       c("playerId", "yahooId", "cbsId", "player", "mflId", "draftYear", "rookie"))
  mflData[, player:= getPlayerName(getPlayerName(getPlayerName(firstLast(player))))]
  mflData[, birthdate := as.Date(as.POSIXct(as.numeric(birthdate), origin = "1970-01-01"))]
  mflData[, rookie := as.logical(ifelse(is.na(rookie), "", rookie) == "R")]
  mflData[, cbsId := as.numeric(cbsId)]
  return(mflData[,c("playerId", "player", "yahooId", "cbsId",  "mflId", "position",
                    "team", "draftYear", "birthdate", "rookie"), with = FALSE])
}
