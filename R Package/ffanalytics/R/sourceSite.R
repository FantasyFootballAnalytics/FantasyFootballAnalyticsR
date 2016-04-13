#' Class to represent source sites
#'
#' Source sites are one of the foundations to the data scrapes. They are
#' representing the web sites that provides data for the projections. The
#' sources can be different for weekly and seasonal data and the type of
#' data can be different. If the data is scraped from HTML then the playerId
#' can be derived as well.
#'
#' @note When specifying the URLs paramters can be used as place holders.
#' @examples
#' sourceSite(siteName = "CBS",
#'            siteUrl = "http://www.cbssports.com/fantasy/football/stats/weeklyprojections/{$Pos}/season/{$SrcID}/standard?&print_rows=9999",
#'            urlType = "html",
#'            urlTable = "1",
#'            playerLink = "/fantasyfootball/players/playerpage/[0-9]{3,6}",
#'            playerId = "cbsId")
#' @slot siteId The ID for the site as identfied in configuration data
#' @slot siteName Name of source site
#' @slot siteUrl String that represents the URL that the site uses for the seasonal data, if any.
#' @slot urlType String that identifies the type of data in  seasonURL (HTML, XML, CSV or file)
#' @slot urlTable A character string identifying which table to grab from seasonURL
#' @slot playerLink A string representing part of the URL to a player profile
#' @slot playerId What to call the id number for player if present
#' @export sourceSite
#' @exportClass sourceSite
sourceSite <- setClass("sourceSite",
                       representation(
                         siteId = "numeric",
                         siteName = "character",
                         siteUrl = "character",
                         urlType = "character",
                         urlTable = "character",
                         playerLink = "character",
                         playerId = "character"
                       ))



# Show/Print Method:
setMethod(
  f = "show",
  signature = "sourceSite",
  definition = function(object){
    cat(paste("Site:", object@siteName, "\tID:", object@siteId))
    cat(paste("\nURL:\t", object@siteUrl, "\nData type:", object@urlType, "\tTable:", object@urlTable))
  }
)

# GET method:
setMethod(
  f = "[",
  signature = "sourceSite",
  definition = function(x, i, j, drop){
    if(i == "siteId"){return(x@siteId)}
    if(i == "siteName"){return(x@siteName)}
    if(i == "siteUrl"){return(x@siteUrl)}
    if(i == "urlType"){return(x@urlType)}
    if(i == "urlTable"){return(x@urlTable)}
    if(i == "playerLink"){return(x@playerLink)}
    if(i == "playerId"){return(x@playerId)}
  }
)

# SET method:
setReplaceMethod(
  f = "[",
  signature = "sourceSite",
  definition = function(x, i, j, value){
    if(i == "siteId"){x@siteId <- as.numeric(value)}
    if(i == "siteName"){x@siteName <- value}
    if(i == "siteUrl"){x@siteUrl <- value}
    if(i == "urlType"){x@urlType <- value}
    if(i == "urlTable"){x@urlTable <- value}
    if(i == "playerLink"){x@playerLink <- value}
    if(i == "playerId"){x@playerId <- as.character(value)}
    validObject(x)
    return(x)
  }
)

setMethod(
  f= "initialize",
  signature = "sourceSite",
  definition = function(.Object, siteId, siteName, siteUrl, urlType, urlTable, playerLink, playerId){
    .Object@siteId <- siteId
    .Object@siteName <- siteName
    .Object@siteUrl <- siteUrl
    .Object@urlType <- urlType
    .Object@urlTable <- urlTable
    .Object@playerLink <- playerLink
    .Object@playerId <- playerId

    return(.Object)
  }
)





