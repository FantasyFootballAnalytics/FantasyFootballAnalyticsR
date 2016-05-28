
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
  type <- ifelse(period != "season", "weekly", period)
  analystInfo <- merge(analysts, analystPositions, by = c("analystId", type))
  siteInfo <- merge(sites, siteUrls, by = "siteId")

  urlInfo <- merge(siteInfo, merge(analystInfo,
                                   siteTables, by = c("siteId", "position", type)),
                   by = "siteId", allow.cartesian = TRUE)
  data.table::setnames(urlInfo, "position", "sourcePosition")
  urlInfo <- urlInfo[analystId %in% selectAnalysts &
            urlPeriod == period & sourcePosition %in% positions]
  urlInfo[, positionId := position.Id[sourcePosition]]
  urlInfo <- urlInfo[order(siteId, analystId, positionId)]
  return(urlInfo[, slotNames("sourceTable"), with = FALSE])

}
