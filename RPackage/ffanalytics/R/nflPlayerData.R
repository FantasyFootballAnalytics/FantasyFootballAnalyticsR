#' Player Data from NFL.com
#'
#' Retrieve player data from NFL.com
#' @param season The year data is to be retrieved from
#' @param weekNo The week that data is to be retrieved from
#' @param positions A character vector of positions to be retrieved
#' @return A \link[data.table]{data.table} with 7 columns:
#' \describe{
#'  \item{playerId}{NFL's ID For the player}
#'  \item{player}{Name of player}
#'  \item{position}{NFL's position designation for the player}
#'  \item{team}{NFL Team that the player is playing for}
#'  \item{opponent}{Team the player is facing next}
#'  \item{depthChart}{Number on the depth chart for the player 1 = starter}
#'  \item{esbid}{Alternate ID for player. Used for ADP/AAV data from NFL}
#' }
#' @export nflPlayerData
nflPlayerData <- function(season = 2016, weekNo = 0, positions = position.name){

  return.data <- data.table::data.table()
  for(pos in positions){
    p = 0
    repeat{
      nfl.url <- paste0("http://api.fantasy.nfl.com/v1/players/researchinfo?season=",
                        season, "&week=", weekNo, "&offset=", p, "&count=1000&format=json",
                        "&position=", pos)
      nfl.data <-httr::content(httr::GET(nfl.url))

      if(length(nfl.data$players) == 0){
        break
      } else {
        nfl.data <- nfl.data$players
        nfl.data <- lapply(nfl.data, function(d){
          data <- data.table::as.data.table(t(d))
          return(data)}
        )
        nfl.data <- data.table::rbindlist(nfl.data, fill = TRUE)
        return.data <- data.table::rbindlist(list(return.data, nfl.data), fill = TRUE)

      }
      p = p + 1000
    }
  }
  return.data[, player := getPlayerName(paste(firstName, lastName))]
  data.table::setnames(return.data, c("id", "teamAbbr", "opponentTeamAbbr", "depthChartOrder"),
                       c("playerId", "team", "opponent", "depthChart"))
  return.data[, c("playerId", "player", "position", "team", "opponent", "depthChart", "esbid") :=
                list(as.character(playerId), as.character(player), as.character(position),
                     as.character(team), as.character(opponent), as.character(depthChart), as.character(esbid))]

  return.data[team == "", team := "FA"]
  return(return.data[, c("playerId", "player", "position", "team", "opponent", "depthChart", "esbid"), with = FALSE])
}

