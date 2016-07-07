#' Scraping and calculating data to use for fantasy football projections.
#'
#' The ffanalytics package provides three categories of important functions:
#' scrape, calculation and analysis.
#'
#' @section Scrape functions:
#' The scrape flow works like this:
#' \enumerate{
#'  \item User initiates the script and specifies the data period that needs to
#'  be scraped
#'  \item The scripts displays available analysts to scrape and the user selects
#'  which to use
#'  \item The script then displays available positions and asks the user to
#'  select positions to scrape.
#'  \item  Data scrape is executed and returns a list with a data table for each
#'  position}
#' User can next specify which aggregate method to apply and execute the
#' calculation scripts on this list to get a data table with projected points,
#' confidence intervals, rankings, risk etc.
#'
#' Tiers are calculated using effect size thresholds based on Cohen's d.
#' D value thresholds for determining tiers for each position can be set by:
#' \code{tierDValues <- c(QB = 0.25, RB = 0.4, WR = 0.4, TE = 0.35, K = 0.15, DST = 0.1, DL = 0.3, DB = 0.13, LB = 0.3)}
#'
#' @docType package
#' @name ffanalytics
#' @import RCurl tcltk

NULL
#>
.onLoad <- function(libname, pkgname){
  vorBaseline <<- ffa.vorBaseline
  ffnAPI <<- "test"
  vorType <<- ffa.vorType
  scoreThreshold <<- ffa.scoreThreshold
  tierGroups <<- ffa.tierGroups
  tierDValues <<- c(QB = 0.25, RB = 0.4, WR = 0.4, TE = 0.35, K = 0.15, DST = 0.1, DL = 0.3, DB = 0.13, LB = 0.3)
  projDir <<- Sys.getenv("HOME")
}
