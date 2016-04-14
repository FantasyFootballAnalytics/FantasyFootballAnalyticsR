#' Analyst options for a period
#'
#' Find the analysts that are projecting stats for the provided period
#'
#' @param scrapePeriod A \link{dataPeriod} object
analystOptions <- function(scrapePeriod = dataPeriod()){
  theAnalysts <-   switch (periodType(scrapePeriod),
                           "Week" = analysts[weekly == 1],
                           "Season" = analysts[season == 1]
  )

  theAnalysts <- merge(theAnalysts, sites[, c("siteId", "siteName"), with = FALSE],
                       by = "siteId")
  multiSite <- theAnalysts$siteName[duplicated(theAnalysts$siteName)]
  theAnalysts[siteName %in% multiSite, displayName := paste0(siteName, ": ", analystName)]
  theAnalysts[is.na(displayName), displayName := analystName]

  showAnalyst <- theAnalysts$analystId
  names(showAnalyst) <- theAnalysts$displayName
  return(showAnalyst)
}

#' URLs to scrape
#'
#' Based on selected analysts, a given period and selected analysts generate
#' URLs that will be scraped. URLs are generated as merges of the \link{analysts},
#' \link{sites}, \link{siteUrls}, and \link{siteTables} datasets.
#' @param selectAnalysts An integer vector of selected analystIds.
#' See \link{analysts} for possible values
#' @param period A string indicating wether the URLs to produce are for weekly
#' or seasonal data scrape
#' @param positions A character vector of positions to scrape.
#' @include sourceTable.R
#' @export getUrls
getUrls <- function(selectAnalysts = analysts$analystId,
                    period = periodType(dataPeriod()),
                    positions = position.name){
  period <- tolower(period)
  analystInfo <- merge(analysts, analystPositions, by = c("analystId", "season", "weekly"))
  siteInfo <- merge(sites, siteUrls, by = "siteId")

  urlInfo <- merge(siteInfo, merge(analystInfo,
                                   siteTables, by = c("siteId", "position", "season", "weekly")),
                   by = "siteId", allow.cartesian = TRUE)

  data.table::setnames(urlInfo, "position", "sourcePosition")
  urlInfo <- urlInfo[analystId %in% selectAnalysts &
            urlPeriod == period & sourcePosition %in% positions]
  return(urlInfo[, slotNames("sourceTable"), with = FALSE])

}
