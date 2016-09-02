#' Class to represent scrape results
#'
#' @slot position Position that the scrape data isfor
#' @slot resultData A \link{data.table} holding the data. The columns in the
#' table are determined by the \link{staticColumns} and \link{resultColumns}.
#' @export dataResult
#' @import data.table
dataResult <- setClass(Class = "dataResult",
                       representation(position = "character",
                                      resultData = "data.table"),
                       prototype(position = as.character(),
                                 resultData = data.table::data.table())
)

setMethod("show",
          "dataResult",
          function(object){
            cat("\nData for:", object@position, "\n")
            cat("============\n")
            print(object@resultData)
          }
)
setMethod("initialize",
          "dataResult",
          function(.Object, position = as.character(),
                   resultData = data.table::data.table()){

            resCols <- c(staticColumns, resultColumns[[position]])

            resultData <- resultData[, intersect(names(resultData), resCols),
                                     with = FALSE]
            if(length(resultData) > 0)
              resultData$position <- position

            .Object@resultData <- resultData
            .Object@position <- position
            return(.Object)
          }
)
