#' Class to represent a data table from a source site
#'
#' Source table class extends the \link{sourceAnalyst} class
#' @slot sourcePosition The position designation that the table represents
#' @slot positionAlias The designation that the source site uses for the position
#' @slot startPage If the table covers multiple pages, the start numbering for the
#' pages. Otherwise 1
#' @slot endPage If the table covers multiple pages, the end numbering for the
#' pages. Otherwise 1
#' @slot stepPage If the table covers multiple pages, the step in the sequence of
#' page numbering. Otherwise 1
#' @slot tableId ID from configuration data that identifies the table uniquely
#'
#' @examples
#' cbs.qb <- sourceTable(sourcePosition = "QB",
#'                       positionAlias = "QB",
#'                       startPage = 1,
#'                       endPage = 1,
#'                       stepPage = 1,
#'                       tableId = 1
#'                      )
#' @import stringr
#' @import XML
#' @export sourceTable
#' @exportClass sourceTable
#' @include sourceAnalyst.R
sourceTable <- setClass("sourceTable", contains = "sourceAnalyst",
                        representation(
                          sourcePosition = "character",
                          positionAlias  = "character",
                          startPage     = "numeric",
                          endPage     = "numeric",
                          stepPage     = "numeric",
                          tableId      = "character"
                        )
)

# Show/Print Method:
setMethod(
  f = "show",
  signature = "sourceTable",
  definition = function(object){
    cat(paste("Site:", object@siteName, "\tPosition:",
              object@sourcePosition, paste("(", object@positionAlias, ")", sep = "")))
          cat(paste("\nPages:", paste(seq(from = object@startPage,
                                      to = object@endPage,
                                      by = object@stepPage),
                                  collapse = ",")))
  }
)

# GET method:
setMethod(
  f = "[",
  signature = "sourceTable",
  definition = function(x, i, j, drop){
    if(i == "siteId"){return(x@siteId)}
    if(i == "siteName"){return(x@siteName)}
    if(i == "siteUrl"){return(x@siteUrl)}
    if(i == "urlType"){return(x@urlType)}
    if(i == "urlTable"){return(x@urlTable)}
    if(i == "playerLink"){return(x@playerLink)}
    if(i == "playerId"){return(x@playerId)}
    if(i == "analystName"){return(x@analystName)}
    if(i == "sourceId"){return(x@sourceId)}
    if(i == "analystId"){return(x@analystId)}
    if(i == "weight"){return(x@weight)}
    if(i == "sourcePosition"){return(x@sourcePosition)}
    if(i == "positionAlias"){return(x@positionAlias)}
    if(i == "startPage"){return(x@startPage)}
    if(i == "endPage"){return(x@endPage)}
    if(i == "stepPage"){return(x@stepPage)}
    if(i == "tableId"){return(x@tableId)}
  }
)

# SET method:
setReplaceMethod(
  f = "[",
  signature = "sourceTable",
  definition = function(x, i, j, value){
    if(i == "siteId"){x@siteId <- as.numeric(value)}
    if(i == "siteName"){x@siteName <- value}
    if(i == "siteUrl"){x@siteUrl <- value}
    if(i == "urlType"){x@urlType <- value}
    if(i == "urlTable"){x@urlTable <- value}
    if(i == "playerLink"){x@playerLink <- value}
    if(i == "playerId"){x@playerId <- as.character(value)}
    if(i == "analystName"){x@analystName <- value}
    if(i == "sourceId"){x@sourceId <- value}
    if(i == "analystId"){x@analystId <- as.numeric(value)}
    if(i == "weight"){x@weight <- value}
    if(i == "sourcePosition"){x@sourcePosition <- value}
    if(i == "positionAlias"){x@positionAlias <- value}
    if(i == "startPage"){x@startPage <- as.numeric(value)}
    if(i == "endPage"){x@endPage <- as.numeric(value)}
    if(i == "stepPage"){x@stepPage <- as.numeric(value)}
    if(i == "tableId"){x@tableId <- as.numeric(value)}
    validObject(x)
    return(x)
  }
)

setMethod(
  f= "initialize",
  signature = "sourceTable",
  definition = function(.Object,siteId, siteName, siteUrl, urlType, urlTable,
                        playerLink, playerId, analystName, sourceId, analystId, weight,
                        sourcePosition, positionAlias, startPage, endPage, stepPage,
                        tableId){
    if(length(analystId) == 0){
      analystId <- abbreviate(paste(siteName, analystName))[[1]]
    }
    .Object@siteId <- as.numeric(siteId)
    .Object@siteName <- siteName
    .Object@siteUrl <- siteUrl
    .Object@urlType <- urlType
    .Object@urlTable <- as.character(urlTable)
    .Object@playerLink <- playerLink
    .Object@playerId <- playerId
    .Object@analystName <- analystName
    .Object@sourceId <- sourceId
    .Object@analystId <- as.numeric(analystId)
    .Object@sourcePosition <- sourcePosition
    .Object@positionAlias <- positionAlias
    .Object@startPage <- as.numeric(startPage)
    .Object@endPage <- as.numeric(endPage)
    .Object@stepPage <- as.numeric(stepPage)
    .Object@tableId <- as.character(tableId)
    .Object@weight <- as.numeric(weight)
    return(.Object)
  }
)


