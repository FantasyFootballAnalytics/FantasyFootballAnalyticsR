#' @export extractTeam
extractTeam <- function(teamInfo, siteName){
  teams <- vector(mode = "character", length = length(teamInfo))
  nfl.teams <- c("WSH", "JAX", nflTeam.abb, "FA")
  switch (siteName,
    "fantasypros" = {
      fd.pattern <- paste(paste0(nfl.teams, "$") , collapse = "|")
      teams <- stringr::str_extract(teamInfo, fd.pattern)
    },

    "espn" = {
      espn.pattern <- paste(paste(",", nfl.teams), collapse = "|")
      teams <- stringr::str_extract(toupper(teamInfo), espn.pattern)
      teams <- gsub(", ", "", teams, fixed = TRUE)
    },

    "numberfire" = {
      nfm.pattern <- paste(paste(",", nfl.teams), collapse = "|")
      teams <- stringr::str_extract(teamInfo, nfm.pattern)
      teams <- gsub(", ", "", teams, fixed = TRUE)
    },

    "fox" = {
      tm.info <- stringr::str_extract(teamInfo, "[:alpha:]+ - [:alpha:]+")
      fox.pattern <- paste(nfl.teams, collapse = "|")
      teams <- stringr::str_extract(toupper(tm.info), fox.pattern)
    },

    "footballguys" = {
      fbg.pattern <- paste(nfl.teams, collapse = "|")
      teams <- stringr::str_extract(teamInfo, fbg.pattern)
    }

  )

  return(teams)
}
