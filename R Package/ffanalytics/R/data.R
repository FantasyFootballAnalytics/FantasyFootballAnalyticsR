#' Site data
#'
#' Data table with information on sites that are being used for datascrapes.
#' @format A data.table with 17 rows and 5 columns
#' \describe{
#'  \item{siteId}{Unique integer identifier for the site}
#'  \item{siteName}{The name of the site}
#'  \item{subscription}{Indicator whether the site requires subscription to get
#'  to the data}
#'  \item{playerId}{Name of column in the player data table that holds the id
#'  for the players}
#' }
"sites"


#' Analyst data
#'
#' Data table with information on analysts that are bing used for data scrapes.
#' @format A \link[data.table]{data.table} with 27 rows and 7 columns
#' \describe{
#'  \item{analystId}{Unique integer identifier for the analyst}
#'  \item{analystName}{Name of analyst}
#'  \item{siteId}{An integer identifying the site that the analyst is projecting
#'  for. Refers to \link{sites} table.}
#'  \item{season}{Indicator of whether the analyst projects seasonal data or not}
#'  \item{weekly}{Indicator of whether the analyst projects weekly data or not}
#'  \item{sourceId}{The identifier that the site uses for the analyst if any}
#'  \item{weight}{The weight used for the analyst in the weighted calculations}
#' }
"analysts"

#' Analyst positions
#'
#' Data table identifying which positions the different analysts are projecting
#' for
#'
#' @format A \link[data.table]{data.table} with 151 rows and 4 columns
#' \describe{
#'  \item{analystId}{An integer identifying the analyst the row is referring to.
#'  Refers to the link{analyst} table.}
#'  \item{position}{Name of the position}
#'  \item{season}{Indicates whether the analysts projects seasonal data for the
#'  position.}
#'  \item{weekly}{Indicates whether the analysts projects weekly data for the
#'  position.}
#' }
"analystPositions"


#' URL information
#'
#' Data table with information on the URLs that data will be scraped from.
#'
#' @format A \link[data.table]{data.table} with 31 rows and 7 columns.
#' \describe{
#'  \item{siteId}{An integer identifying the site the URL is referring to. See
#'  \link{sites} for values.}
#'  \item{siteUrl}{The URL that the data will be scraped from. Use placeholders
#'  for parameters that needs to be substituted, like position, analyst, page,
#'  season and week.}
#'  \item{urlPeriod}{Specifies whether the URL is used for week or season data.}
#'  \item{urlType}{Specifies the data type that is returned from the URL. Could be
#'  HTML, XML, csv or file}
#'  \item{urlTable}{The table number that the data scrape should read from the
#'  URL. Mostly used for HTML data, but can also be used to designate sheets in
#'  a spreadsheet file.}
#'  \item{playerLink}{A string to identify the links to player profile pages on
#'  an HTML page.}
#' }
"siteUrls"

#' Table information
#'
#' Data table with information on tables that data will be scraped from.
#'
#' @format A \link[data.table]{data.table} with 111 rows and 9 columns.
#' \describe{
#'  \item{tableId}{An integer that uniquely identifies the table}
#'  \item{position}{Name of the position that the table holds data for}
#'  \item{positionAlias}{The position identifier used in the URL for the table}
#'  \item{siteId}{Integer identfying the site that the table is found on. See
#'  \link{sites} for values}
#'  \item{startPage}{If the table spans more pages then the start number for the
#'  pages. If all the data is on one page the value is 1.}
#'  \item{endPage}{If the table spans more pages then the end number for the page
#'  sequence. If all the data is on one page the value is 1}
#'  \item{stepPage}{If the table spans more page then the step number of the page
#'  sequence. If all the data is on one page the value is 1}
#'  \item{season}{Indicates whether the table can be used for seasonal data scrapes.}
#'  \item{weekly}{Indicates whether the table can be used for weekly data scrapes.}
#' }
"siteTables"

#' Column information
#'
#' Data table with information on the columns in tables identified in \link{siteTables}.
#'
#' @format A \link[data.table]{data.table} with 2924 rows and 6 columns.
#' \describe{
#'  \item{tableId}{An integer identifying the table that the column belongs to.
#'  See \link{siteTables} for values}
#'  \item{columnName}{The name of the column}
#'  \item{columnType}{The data type for the column}
#'  \item{columnOrder}{The order in which the column appears in the table}
#'  \item{columnPeriod}{Indicates if the column appears in tables used for seasonal
#'  or weekly data scrapes}
#'  \item{removeColumn}{Indicates if the column can be removed before the table
#'  is returned.}
#' }
"tableColumns"

#' Table rows to remove
#'
#' Data table with information on rows that will need to be removed from \link{siteTables}
#' for the data scrape to be succesfull
#'
#' @format A \link[data.table]{data.table} with 28 rows and 2 columns.
#' \describe{
#'  \item{tableId}{An integer identifying the table that the row can be removed from.
#'  See \link{siteTables} for values}
#'  \item{rowRemove}{An integer identifying the row that can be removed}
#' }
"tableRowRemove"
