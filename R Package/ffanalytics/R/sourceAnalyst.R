#' Class to represent the source Analysts
#'
#' @slot analystName The name of the analysts
#' @slot sourceId The id for the source. This is only used if there are multiple
#' analysts for a site
#' @slot analystId A character string specifying the id to be used for the analyst.
#' If left blank, it will be set as a 4 letter abbreviation of the site + analyst
#' names using \link{abbreviate}.
#' @examples
#' cbs.avg <- sourceAnalyst(analystName = "Average",
#'                          sourceId = "avg",
#'                          analystId = "cbav")
#' @include sourceSite.R
#' @export sourceAnalyst
#' @exportClass sourceAnalyst
sourceAnalyst <- setClass("sourceAnalyst", contains = "sourceSite",
                          representation(
                            analystName    = "character",
                            sourceId       = "character",
                            analystId      = "numeric",
                            weight         = "numeric"
                          ),
                          prototype(
                            analystName    = as.character(),
                            sourceId       = as.character(),
                            analystId      = as.numeric(),
                            weight         = as.numeric()
                          )

)

# Show/Print Method:
setMethod(
  f = "show",
  signature = "sourceAnalyst",
  definition = function(object){
    cat(paste("Analyst:", object@analystName, "\tProjects For:", object@siteName))
  }
)

# GET method:
setMethod(
  f = "[",
  signature = "sourceAnalyst",
  definition = function(x, i, j, drop){
    if(i == "analystName"){return(x@analystName)}
    if(i == "sourceId"){return(x@sourceId)}
    if(i == "analystId"){return(x@analystId)}
    if(i == "weight"){return(x@weight)}
  }
)

# SET method:
setReplaceMethod(
  f = "[",
  signature = "sourceAnalyst",
  definition = function(x, i, j, value){
    suppressWarnings(if(!is.na(as.numeric(value)))
      value <- as.numeric(value))
    if(i == "analystName"){x@analystName <- value}
    if(i == "sourceId"){x@sourceId <- value}
    if(i == "analystId"){x@analystId <- as.numeric(value)}
    if(i == "weight"){x@weight <- value}
    validObject(x)
    return(x)
  }
)

setMethod(
  f= "initialize",
  signature = "sourceAnalyst",
  definition = function(.Object, analystName, sourceId, analystId, weight,
                        siteId, siteName, siteUrl, urlType, urlTable, playerLink, playerId){
    if(length(analystId) == 0){
      analystId <- abbreviate(paste(siteName, analystName))[[1]]
    }
    .Object@analystName <- analystName
    .Object@sourceId <- sourceId
    .Object@analystId <- analystId
    .Object@weight <- weight
    .Object@siteId <- siteId
    .Object@siteName <- siteName
    .Object@siteUrl <- siteUrl
    .Object@urlType <- urlType
    .Object@urlTable <- as.character(urlTable)
    .Object@playerLink <- playerLink
    .Object@playerId <- playerId
    return(.Object)
  }
)

