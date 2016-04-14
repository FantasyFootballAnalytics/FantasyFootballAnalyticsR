#' Scraping and calculating data to use for fantasy football projections.
#'
#' The ffanalytics package provides three categories of important functions:
#' scrape, calculation and analysis.
#'
#' @section Scrape functions:
#' The scrape flow works like this:
#' \enumerate{
#'  \item User initiates the script and specifies the data period that needs to be scraped
#'  \item The scripts displays available analysts to scrape and the user selects which to use
#'  \item The script then displays available positions and asks the user to select positions to scrape.
#'  \item  Data scrape is executed and returns a list with a data table for each position}
#' User can next specify which aggregate method to apply and execute the calculation scripts on this list to get a data table with projected points, confidence intervals, rankings, risk etc.
#'
#' @docType package
#' @name ffanalytics

NULL
#>
.onLoad <- function(libname, pkgname){

}
