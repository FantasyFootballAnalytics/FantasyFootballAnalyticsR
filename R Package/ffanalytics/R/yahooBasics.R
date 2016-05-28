
yahooGames <- c("2012" = 273, "2013" = 314, "2014" = 331, "2015" = 348,
                "2016" = 359)

getItemKey <- function(season, item.id, item.type){
  game.key <- yahooGames[[as.character(season)]]
  item <- substring(tolower(item.type), 1, 1)
  item.key <- paste0(game.key, ".", item, ".", item.id)
  return(item.key)
}
# Function to generate url to call the Yahoo Fantasy sports API
apiUrl <- function(resource = "game", filter = NULL, resourceId = NULL,
                   subResource = "nfl", queryParams = list() ){
  yahooHost <- "fantasysports.yahooapis.com"
  resourcePath <- resource

  # Filters don't really work the way they are described in the documentation.
  # Use query paramaters instead
  if(!is.null(filter))
    resourcePath <- paste(resourcePath, filter, sep = ";")

  #  Building the resource path
  if(!is.null(resourceId))
    resourcePath <- paste(resourcePath, resourceId, sep = "/")
  if(!is.null(subResource))
    resourcePath <- paste(resourcePath, subResource, sep = "/")

  # Components of the URL as list
  yahooUrl <- list(scheme = "http",
                   hostname = yahooHost,
                   path = paste("fantasy", "v2", resourcePath, sep = "/")
  )

  # Setting the query paramaters
  queryParams$format = "json"
  yahooUrl$query <- queryParams

  # Setting class for the url and returning the url as string
  attr(yahooUrl, "class") <- "url"
  return(httr::build_url(yahooUrl))
}

# Function to execute the API call
callAPI <- function(api.url){
  # Get the token. Will not ask for credentials if cached.
  api.token <- yahoo_token(yahoo_endpoint, yahoo_app, cache=TRUE)

  # Checking if token is expired and if so then refresh the token.
  cacheTime <- file.info(api.token$cache_path)$mtime
  tokenValidFor <- as.numeric(api.token$credentials$oauth_expires_in)
  if(Sys.time() > cacheTime + tokenValidFor){
    refresh_token(api.token)
  }
  # Setting authorization header for the API call and get response and content
  api.auth <-  httr::config(token = api.token)
  api.response <- httr::GET(api.url, api.auth)
  api.content <- httr::content(api.response)

  # Checking for errors
  if(api.response$status_code >= 400){
    errorMsg <- api.content$error$description
    stop(errorMsg, call. = FALSE)
  }

  # If status = 200 then the call was successful and we can return the content
  # if status != 200 then we have an unexpected response and we will stop
  if(api.response$status_code == 200){
    return(api.content$fantasy_content)
  } else {
    stop("Unexpected response in API call", call. = FALSE)
  }
}

# Function to generate a random string
random.string <- function(str.length = 40){
  # create random alphanumeric vector
  rand.vector <- sample(c(sample(letters), sample(LETTERS), sample(0:9)))

  # Select a random number between 5 and 15 as number of elements then split the
  # random alphanumeric into groups with that many elements. Shuffle the groups
  # then put them back as a vector and sample the vector
  num.elements <- sample(5:15, 1)
  random.list <- split(rand.vector, floor(seq_along(rand.vector)/num.elements))
  random.list <- lapply(random.list, sample)
  random.list <- random.list[sample(1:length(random.list))]
  rand.vector <- sample(unlist(random.list))

  # If requested length is greater than the lenght of the vector then we allow
  # reuse of values
  replace.values <- (str.length > length(rand.vector))
  return(paste(sample(rand.vector, str.length, replace = replace.values),
               collapse = ""))
}

# Function to refresh access token at Yahoo
refresh_token <- function(acces.token){
  # list of paramaters we need for the refresh request.
  oauth_refresh <- list(
    oauth_nonce = random.string(40),
    oauth_consumer_key = acces.token$app$key,
    oauth_signature_method = "plaintext",
    oauth_signature = paste0(acces.token$app$secret, "&",
                             acces.token$credentials$oauth_token_secret),
    oauth_version ="1.0",
    oauth_token = acces.token$credentials$oauth_token,
    oauth_timestamp = as.integer(Sys.time()),
    oauth_session_handle = acces.token$credentials$oauth_session_handle
  )

  # Getting the URL for the access request
  refreshURL <- httr::parse_url(acces.token$endpoint$access)

  # Adding the refresh parameters
  refreshURL$query <- oauth_refresh
  # Rebuils URL and send request
  requestURL <- httr::build_url(refreshURL)
  newToken <- httr::content(httr::GET(requestURL))
  # Retrieve values from response and update existing token
  token_elements <- stringr::str_split(newToken, "&")
  elements_vector <- unlist(stringr::str_split(unlist(token_elements), "="))
  tokenValues <- elements_vector[ which(1:length(elements_vector)%%2 == 0)]
  names(tokenValues) <- elements_vector[ which(1:length(elements_vector)%%2 != 0)]
  tokenValues["oauth_token"] <- URLdecode(tokenValues["oauth_token"])
  acces.token$credentials <- as.list(tokenValues)

  # Cache the updated token and return it
  acces.token$cache()
  return(acces.token)
}

# Function to get the stat categories for a season.
getYahooStats <- function(season){
  game.key <- yahooGames[[as.character(season)]]
  statUrl <- apiUrl(resource = "game", resourceId = game.key,
                    subResource = "stat_categories")
  statList <- callAPI(statUrl)$game[[2]]$stat_categories$stats
  stat.categories <- data.table::rbindlist(lapply(statList, function(sl){
    return(data.table::as.data.table(sl[["stat"]][c("stat_id", "name",
                                                    "display_name")]))
  }))
  return(stat.categories)
}

# It seems that the easiest way to retrieve player stats from Yahoo is through
# a league, so one needs to be setup and be configured to have the data you want
# to retrieve.
#' @export getYahooPlayerStats
getYahooPlayerStats <- function(season = NULL, week = 0, league, position = NULL){

  league.key <- getItemKey(season, item.id = league, item.type = "league")

  # The API call returns 25 players at a time so we need to make repeated calls
  # startNum will help us step through the process.
  startNum = 0
  stat.query <- list()
  resultTable <- data.table::data.table()

  # Generate query parameters
  if(!is.null(position))
    stat.query$position <- position
  if(week > 0){
    stat.query$type <- "week"
    stat.query$week <- week
  }

  # Get 25 players at a time
  repeat {
    stat.query$start <- startNum
    # Generate the URL to call
    stat.url <- apiUrl(resource = "league", resourceId = league.key,
                       subResource = "players/stats", queryParams = stat.query)
    # Extract the player data
    player.data <- callAPI(stat.url)$league[[2]]$players

    # It no players are returned then stop the loop
    if(length(player.data) == 0)
      break

    player.data <- player.data[!(names(player.data) == "count")]

    # Get the data from the API Call
    player.table <- data.table::rbindlist(
      lapply(player.data, function(pl){
        # pl.info will contain the information about the player
        pl.info <- data.table::as.data.table(t(unlist(pl[["player"]][[1]])))
        pl.info[, team := toupper(editorial_team_abbr) ]
        data.table::setnames(pl.info, c("player_id", "name.full"),
                             c("yahooId", "player"))

        # pl.stats is the stats for the player
        pl.stats <- pl[["player"]][[2]][["player_stats"]][["stats"]]
        pl.stats <- data.table::rbindlist(lapply(pl.stats, function(pstat){
          data.table::as.data.table(t(unlist(pstat[["stat"]])))
        }))

        # Identifying the stat names
        pl.stats[, stat_id := as.integer(stat_id)]
        stats.table <- merge(yahooStats[, c("stat_id", "ffa_column"),
                                        with = FALSE],
                             pl.stats, by = "stat_id")
        stat.names <- stats.table$ffa_column

        # Flip the stats table so it has one column per stats
        stats.table <- data.table::as.data.table(t(as.numeric(stats.table$value)))
        data.table::setnames(stats.table, stat.names)

        # merge with player information
        return(data.table::data.table(pl.info[, c("yahooId", "player","team"),
                                              with = FALSE], stats.table))

      }), fill = TRUE
    )

    resultTable <- data.table::rbindlist(list(resultTable, player.table),
                                         fill = TRUE)
    startNum <- startNum + 25
  }
  resultTable[team == "JAX", team := "JAC"]
  return(resultTable)
}

#' @export getLeagueSettings
getLeagueSettings <- function(season, league){
  league.key <- getItemKey(season, item.id = league, item.type = "league")

  league.url <- apiUrl(resource = "league", resourceId = league.key,
                       subResource = "settings")
  league.settings <- callAPI(league.url)
  league.info <- data.table::as.data.table(league.settings$league[[1]])

  league.rosters <- data.table::rbindlist(
    lapply(league.settings$league[[2]]$settings[[1]]$roster_positions,
           function(rp){
             return(data.table::as.data.table(rp[["roster_position"]]))
           }), fill = TRUE
  )

  stat.settings <- league.settings$league[[2]]$settings[[1]][c("stat_categories",
                                                               "stat_modifiers")]
  stat.categories <- data.table::rbindlist(
    lapply(stat.settings[["stat_categories"]]$stats, function(sc){
      cat.vars <- sc[["stat"]][!(names(sc[["stat"]]) == "stat_position_types")]
      return(data.table::as.data.table(t(unlist(cat.vars))))
    }), fill = TRUE
  )

  stat.modifiers <- data.table::rbindlist(
    lapply(stat.settings[["stat_modifiers"]]$stats, function(sm){
      cat.mods <- sm[["stat"]]
      return(data.table::as.data.table(t(unlist(cat.mods))))
    }), fill = TRUE
  )
  league.scoring <- merge(stat.categories, stat.modifiers, by = "stat_id",
                          all.x = TRUE)
  return(list(info = league.info[, c("league_id", "name", "url", "season"),
                                 with = FALSE],
              rosters = league.rosters, scoring = league.scoring))
}
