#' @export scrapeJQuery
#' @import RSelenium
scrapeJQuery <- function(inpUrl){
  urlSite <- websites[sapply(websites,
                             function(ws)(length(grep(ws, tolower(inpUrl),
                                                      fixed = TRUE)) >0))]
  scrape.data <- data.table::data.table()
  if(urlSite == "freedraftguide"){

    pos <- as.numeric(httr::parse_url(inpUrl)$query$POS)
    scrape.data <- readRTSdata(pos)
  }

  if(urlSite == "numberfire"){
    scrape.data <- readNMFdata(inpUrl)
  }

  return(scrape.data)
}

# Scrape RTS data. Takes position number
# 0 = QB, 1 = RB, 2 = WR, 3 = TE, 4 = K, 5 = DST
#' @export readRTSdata
readRTSdata <- function(posNum){
  cityPattern <- paste(nflTeam.city, collapse = "|")
  tblNum <- 3 + posNum
  rts.url <- paste0("https://www.freedraftguide.com/football/draft-guide-rankings.php?POS=", posNum)
  rts.xpath <- paste0("/html/body/div[2]/div[", tblNum,"]/table/tbody")
  srv <- RSelenium::startServer()
  remDr <- RSelenium::remoteDriver$new(browserName = "chrome")
  brws <- remDr$open(silent = TRUE)
  nav <- remDr$navigate(rts.url)
  Sys.sleep(3)
  player_table <- remDr$findElement('xpath', rts.xpath)

  players <- strsplit(player_table$getElementText()[[1]], "\n")

  remDr$close()
  players[[1]] <- gsub("T Y ", "TY ", players[[1]])
  players[[1]] <- gsub(" Jr.", "", players[[1]])
  players[[1]] <- gsub(" III", "", players[[1]])
  if(posNum == 5)
    players[[1]] <- gsub(cityPattern, "", players[[1]])

  final <- data.table::data.table()
  for(x in players[[1]]){
    temp <- data.table::as.data.table(t(unlist(strsplit(x, " "))))
    final <- data.table::rbindlist(list(final, temp), fill = TRUE)
  }
  return(final)
}

#' @export readNMFdata
readNMFdata <- function(nmf.url){
  pos <- strsplit(httr::parse_url(nmf.url)$path, "/")[[1]][4]
  srv <- RSelenium::startServer()
  remDr <- RSelenium::remoteDriver$new(browserName = "chrome")
  brws <- remDr$open(silent = TRUE)
  nav <- remDr$navigate(nmf.url)
  player_table <- remDr$findElement('id', 'complete-projection')
  players <- strsplit(player_table$getElementText()[[1]], "\n")

  remDr$close()
  players[[1]] <- gsub("T Y ", "TY ", players[[1]])
  players[[1]] <- gsub(" Jr.", "", players[[1]])
  players[[1]] <- gsub(" III", "", players[[1]])
  players[[1]] <- gsub("Ha Ha", "Ha-Ha", players[[1]])
  if(pos == "d"){
    for(cty in seq_along(nflTeam.city)){
      if(nflTeam.city[cty] == "New York")
        players[[1]] <- trimws(gsub(nflTeam.city[cty], "", players[[1]]))
      else
        players[[1]] <- gsub(nflTeam.city[cty], nflTeam.name[cty], players[[1]])
    }
  }
  final <- data.table::data.table()
  for(x in players[[1]]){
    temp <- data.table::as.data.table(t(unlist(strsplit(x, " "))))
    final <- data.table::rbindlist(list(final, temp), fill = TRUE)
  }

  final[, c("V3", "V4") := list(gsub("\\(|,|\\)", "", V3),
                                gsub("\\(|,|\\)", "", V4))]
  final[, V1 := gsub("Ha-Ha", "Ha Ha", V1)]
  if(pos == "d")
    final[, V3 := NULL]

  return(final[-c(1,2)])
}
