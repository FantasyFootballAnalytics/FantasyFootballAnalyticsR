#' Run a data scrape
#'
#' Executing a data scrape based on inputs of week, season, analysts and positions.
#' If no inputs are specified the user will be prompted.
#'
#' @note Historical scrapes are nearly impossible to do as very few if any sites
#' actually stores their historical projections. An attempt to scrape historical
#' projections will likely produce current projectsions in most cases.
#' @param week The week number that the scrape is going to be executed for
#' @param season The season that the scrape will be executed for
#' @param analysts An integer vector of analystIds specifying which analysts to
#' execute the scrape for. See \link{analysts} data set for values.
#' @param positions A character vector of position names specifying which positions
#' to execute the scrape for
#' @return list of \link{dataResults}. One entry per position scraped.
#' @export runScrape
runScrape <- function(week = NULL, season = NULL,
                      analysts = NULL, positions = NULL){

  # Request input from user to determine period to scrape
  if(is.null(week) & is.null(season)){
    scrapeSeason <- as.numeric(readline("Enter season year to scrape: "))
    scrapeWeek <- as.numeric(readline("Enter week to scrape (use 0 for season): "))
  } else {
    scrapeWeek <- ifelse(is.null(week), 0, week)
    scrapeSeason <- ifelse(is.null(season), as.POSIXlt(Sys.Date())$year + 1900, season)
  }

  scrapePeriod <- dataPeriod()
  if(!is.na(scrapeWeek))
    scrapePeriod["weekNo"] <- scrapeWeek
  if(!is.na(scrapeSeason))
    scrapePeriod["season"] <- scrapeSeason

  scrapeType <- periodType(scrapePeriod)

  # Request input from user to select the analysts to scrape for

  selectAnalysts <- analystOptions(scrapePeriod)
  if(is.null(analysts)){ #} | ifelse(is.null(analysts), "", analysts) != "all"){
    scrapeAnalysts <- selectAnalysts[select.list(names(selectAnalysts),
                                                 title = "Select Analysts to Scrape",
                                                 multiple = TRUE)]

    if(max(nchar(scrapeAnalysts)) == 0)
      scrapeAnalysts <- selectAnalysts
  } else {
    scrapeAnalysts <- analysts
  }

  selectPositions <- analystPositions$position[analystPositions$analystId %in% scrapeAnalysts]
  # Request input from user to select the positions to scrape for
  if(is.null(positions)){
    scrapePosition <- select.list(position.name, multiple = TRUE,
                                  title = "Select positions to scrape")

    if(max(nchar(scrapePosition)) == 0)
      scrapePosition <- position.name
  } else {
    scrapePosition <- positions
  }
  urlTable <- getUrls(scrapeAnalysts, scrapeType, scrapePosition)
  if(nrow(urlTable) == 0){
    stop("Nothing to scrape. Please try again with different selection.", call. = FALSE)
  }

  cat("Retrieving player data \r")
  playerData <<- getPlayerData(season = scrapeSeason, weekNo = scrapeWeek,
                              pos = scrapePosition)

  scrapeResults <- pbapply::pbapply(urlTable, 1, function(urlInfo){
    scrapeSrc <- createObject("sourceTable", as.list(urlInfo))
    retrieveData(scrapeSrc, scrapePeriod)
  })
  returnData <- lapply(intersect(position.name, urlTable$sourcePosition), function(pos){

    resData <- data.table::rbindlist(
      lapply(scrapeResults[which(urlTable$sourcePosition == pos)],
             function(sr)sr@resultData), fill = TRUE)
    return(dataResult(resultData = resData, position = pos))
  })

  names(returnData) <- intersect(position.name, urlTable$sourcePosition)
  returnData$period <- scrapePeriod
  returnData$analysts <- scrapeAnalysts

  return(returnData)
}

#' @export
analystOptions <- function(period){
  if(periodType(period) == "Season"){
    periodAnalysts <- analysts[season == 1]
  }

  if(periodType(period) == "Week"){
    periodAnalysts <- analysts[weekly == 1]
  }
  periodAnalysts <- periodAnalysts[siteId %in% siteUrls$siteId]
  periodAnalysts <- merge(periodAnalysts, sites, by = "siteId")
  periodAnalysts[siteId %in% periodAnalysts$siteId[duplicated(siteId)],
                 listName := paste0(siteName, ": ", analystName)]
  periodAnalysts[is.na(listName), listName := analystName]
  periodAnalysts <- periodAnalysts[order(siteId, analystId)]
  analystList <- periodAnalysts$analystId
  names(analystList) <- periodAnalysts$listName
  return(analystList)
}
