#' @export scrapeJSONdata
scrapeJSONdata <- function(inpUrl){
  urlSite <- websites[sapply(websites,
                             function(ws)(length(grep(ws, tolower(inpUrl),
                                                      fixed = TRUE)) >0))]


  switch (urlSite,
          "nfl.com" =  {
            srcData <- httr::content(httr::GET(inpUrl))
            nflProjection <- srcData$players
            srcData <- data.table::rbindlist(lapply(nflProjection, function(np){
              statTable <- data.table::as.data.table(lapply(np$stats, as.numeric))
              names(statTable) <- nflstats$ffanalytics[nflstats$id %in% names(statTable)]
              dt <- data.table::as.data.table(t(np))
              dt[, stats := NULL]
              data.table::setnames(dt, c("id", "name", "teamAbbr"), c("playerId", "player", "team"))
              dt <- dt[, c("playerId", "player", "team"), with = FALSE]
              dt[, c("playerId", "player", "team") := list(as.numeric(playerId), as.character(player),
                                                           as.character(team))]
              return(cbind(dt, statTable))

            }
            ), fill = TRUE)

          },
          "yahoo" = {
            yahoo.url <- httr::parse_url(inpUrl)
            yahooPos <- yahoo.url$path
            yahooSeason <- yahoo.url$query$season
            yahooWeek <- yahoo.url$query$week
            srcData <- getYahooPlayerStats(yahooSeason, yahooWeek,
                                           yahooLeague, yahooPos)
          }
  )

  return(srcData)
}
