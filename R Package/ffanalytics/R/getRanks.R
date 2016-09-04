#' Expert Consensus Rankings
#'
#' Reterieve expert consensus rankings from fantasypros.com
#' @param rank.position Position to retrieve ranks for. Use "consensus" to get
#' overall rankings
#' @param leagueType Indicate whether to get ppr rankings or standard rankings (std)
#' @param weekNo Week number to retrieve ranks for. Use 0 for season ranks.
#' @return \link[data.table]{data.table} with up to 11 columns:
#' \describe{
#'  \item{player}{Name of player}
#'  \item{position}{Player's position}
#'  \item{team}{Team the player is playing for}
#'  \item{ecrRank}{Consensus Rank}
#'  \item{bestRank}{Highest rank from experts}
#'  \item{worstRank}{Lowest rank from experts}
#'  \item{avgRank}{Average rank from experts}
#'  \item{sdRank}{Standard deviation of ranks}
#'  \item{adp}{Average Draft Position}
#'  \item{vsAdp}{Difference between overall rank and ADP}
#'  \item{rankType}{Will have value of "overall" or "position"}
#' }
#' @export getRanks
getRanks <- function(rank.position = "consensus", leagueType = "std", weekNo = 0){

  # Defining return columns for the table
  if(weekNo == 0)
    returnColumns <-  c("player", "position", "team", "ecrRank", "bestRank",
                        "worstRank", "avgRank", "sdRank", "adp", "vsAdp", "rankType")
  else
    returnColumns <- c("player", "position","team", "ecrRank", "bestRank",
                       "worstRank", "avgRank", "sdRank", "rankType")

  defaultTable <- rep(NA, length(returnColumns))
  names(defaultTable) <- returnColumns
  if(leagueType == "half")
    leagueType <- "half-point-ppr"
  # There aren't ppr rankings for QB, K, DST and IDP, so we use the standard ones
  if(!(rank.position %in% c("RB", "WR", "TE", "consensus")) & leagueType != "std"){
    leagueType = "std"
  }

  # There aren't weekly rankings for IDP so we return empty table
  if(weekNo != 0 & rank.position %in% c("DB", "DL", "LB")){
    warning("Weekly ECR ranks not available for IDP", call. = FALSE)
    dt <- data.table::data.table(t(defaultTable))
    return(dt[, returnColumns, with = FALSE][0])
  }

  # Overall ranks are not available on weekly data, so we return empty table
  if(weekNo != 0 & rank.position == "consensus"){
    warning("Weekly overall rankings not available", call. = FALSE)
    dt <- data.table(t(defaultTable))
    return(dt[, returnColumns, with = FALSE][0])
  }

  # Generating the URL to scrape the data from
  url_base <- "https://www.fantasypros.com/nfl/rankings"
  if(weekNo != 0){
    url_path <- paste("/", ifelse(leagueType != "std",
                                  paste(tolower(leagueType),
                                        "-", sep = ""),""), tolower(rank.position),
                      ".php", sep = "")
  } else {
    url_path <- paste("/", ifelse(leagueType != "std", paste(tolower(leagueType),
                                                             "-", sep = ""),""),
                      ifelse(rank.position == "consensus" & leagueType != "std",
                             "", paste(tolower(rank.position), "-", sep ="")),
                      "cheatsheets.php", sep = "")
  }

  inpUrl <- paste(url_base, url_path, sep = "")

  # Setting up column names and types
  if(rank.position %in% c("DB", "DL", "LB", "consensus")){
    cNames <- c("ecrRank", "player", "position" , "bye", "bestRank", "worstRank",
                "avgRank", "sdRank")
    cTypes <- c("numeric", "character", "character","numeric" ,"numeric",
                "numeric", "numeric", "numeric")
  }
  else{
    if(weekNo == 0){
      cNames <- c("ecrRank", "player", "bye" , "bestRank", "worstRank",
                  "avgRank", "sdRank")
      cTypes <- c("numeric", "character", "numeric", "numeric", "numeric",
                  "numeric", "numeric")
    }
    else {
      cNames <- c("ecrRank", "player", "opp", "bestRank", "worstRank",
                  "avgRank", "sdRank")
      cTypes <- c("numeric", "character", "character","numeric", "numeric",
                  "numeric", "numeric")
    }
  }

  if (weekNo == 0 & !(rank.position %in% c("DB", "DL", "LB"))){
    cNames <- c(cNames, "adp", "vsAdp")
    cTypes <- c(cTypes, "numeric", "numeric")
  }


  rnks <-data.table::data.table(XML::readHTMLTable(RCurl::getURL(inpUrl))$data)
  if(length(rnks) == 0){
    rnks <- data.table::data.table(t(rep(NA, length(cNames))))
    rnks <- rnks[0]
  }
  data.table::setnames(rnks, cNames)
  if(nrow(rnks) > 0 ){

    rnks$team <- extractTeam(rnks$player, "fantasypros")
    # Cleaning player names
    rnks$player <- getPlayerName(rnks$player)

    # Removing numbers from the position variable, i.e. RB5, WR14 -> RB, WR
    if(exists("position", rnks)){
      rnks[, position := gsub("[0-9]+", "", position)]
    }

    # Merging with player data from NFL.com to find the playerId.
    if(rank.position != "consensus"){
      rnks[, rankType := "position"]
      rnks[, position :=  rank.position]
    }
    else{
      rnks[, rankType := "overall"]

    }
    if(any(rnks$position == "DST")){
      rnks[position == "DST", team := nflTeam.abb[nflTeam.name == player], by = "player"]
    }
    # Order result by rank
    rnks <- rnks[order(as.numeric(ecrRank)),]
  }
  return(rnks[!is.na(player), intersect(returnColumns, names(rnks)), with = FALSE])
}
