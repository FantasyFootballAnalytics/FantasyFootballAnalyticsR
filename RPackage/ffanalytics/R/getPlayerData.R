#' Combine MFL and NFL player data
#'
#' Retrieve data from MFL and NFL and combine
#' @param season The year that player data is to be retrieved from
#' @param weekNo The weekNo that the player data is to be retrieved from
#' @param pos A character vector with position names to be retrieved
#' @return A \link[data.table]{data.table} with 13 columns
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
#'  \item{opponent}{Team the player is facing next}
#'  \item{depthChart}{Number on the depth chart for the player 1 = starter}
#'  \item{esbid}{Alternate ID for player. Used for ADP/AAV data from NFL}
#' }
#' @export getPlayerData
getPlayerData <- function(season, weekNo, pos = position.name){
  mfl <- mflPlayers(season, weekNo, pos)
  nfl <- nflPlayerData(season, weekNo, positions = pos)

  # Checking if any of the NFL IDs are missing
  allPlayers <- merge(mfl, nfl, by = c("player", "position"),
                      suffixes = c("", "_nfl"), all.x = TRUE)
  allPlayers[is.na(playerId) & !is.na(playerId_nfl), playerId := playerId_nfl]
  allPlayers <- allPlayers[!is.na(playerId)]
  allPlayers <- allPlayers[, names(mfl), with = FALSE]

  # Adding data from NFL
  allPlayers <- merge(allPlayers,
                      nfl[, c("playerId", "opponent", "depthChart", "esbid"), with = FALSE],
                      by = "playerId", all.x = TRUE)

  return(allPlayers)
}
