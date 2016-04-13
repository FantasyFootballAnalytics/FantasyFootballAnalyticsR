#' Class to represent the period for a datascrape
#'
#' @description The datascrapes need a week and season designation to work.
#'
#' @slot weekNo An integer representing the week number. 0 for preseason, 1-17
#' for regular season and 18-21 for post season
#' @slot season A year representing the season. Should be 2008 or later but
#' can't be higher than current year
#' @examples
#' dataPeriod(weekNo = 1, season = 2015) # Week 1 of the 2015 season
#' dataPeriod(season = 2015)             # 2015 season
#' dataPeriod(weekNo = 3)                # Week 3 of the current year
#' dataPeriod()                          # Current season
#' @export dataPeriod
#' @exportClass dataPeriod
#' @exportMethod periodType
dataPeriod <- setClass(
  Class = "dataPeriod",
  representation(weekNo = "numeric", season = "numeric"),
  prototype = prototype(weekNo = 0, season = as.POSIXlt(Sys.Date())$year + 1900),
  validity = function(object){
    if(!(object@weekNo %in% 0:21)){
      stop("Week number not valid. Please set week number between 0 and 21",
           call. = FALSE)
    }
    if(object@season > as.POSIXlt(Sys.Date())$year + 1900 | object@season < 2008){
      stop("Season is not valid. Please set season to be between 2008 and current year",
           call. = FALSE)
    }

  }
)

# Show/Print Method:
setMethod(
  f = "show",
  signature = "dataPeriod",
  definition = function(object){
    print(c("weekNo" = object@weekNo, "season" = object@season))
  }
)

# GET method:
setMethod(
  f = "[",
  signature = "dataPeriod",
  definition = function(x, i, j, drop){
    if(i == "weekNo"){return(x@weekNo)}
    if(i == "season"){return(x@season)}
  }
)

# SET method:
setReplaceMethod(
  f = "[",
  signature = "dataPeriod",
  definition = function(x, i, j, value){
    suppressWarnings(if(!is.na(as.numeric(value)))
      value <- as.numeric(value))
    if(i == "weekNo"){x@weekNo <- value}
    if(i == "season"){x@season <- value}
    validObject(x)
    return(x)
  }
)


setGeneric(name = "periodType", def = function(object){standardGeneric("periodType")})

#' Determine if period is a week or season
#' @param x A dataPeriod object
setMethod(f = "periodType",
          signature = "dataPeriod",
          definition = function(object){
            if(object@weekNo == 0){
              return("Season")
            } else {
              return("Week")
            }
          })
