#' Calculate confidence interval
#'
#' Confidence intervals are determined as percentiles (default 10 and 90).
#' If the correcpsonding average value is less than the low point then we
#' set the lower point to the minimum value. Conversely if the average value is
#' higher than the upper limit then we set the upper limit to the max value.
#' @param calcMethod Calculation method. Can be one of "weighted", "robust" or
#' "average"
#' @param dataValue Vector of values to calculate confidence interval for
#' @param dataWeights Vector of weights for weighted calculation
#' @param pValues Vector of percentiles for the confidence interval. Defaults to
#' \code{c(0.1, 0.9)} for 10 and 90 percentiles.
#' @export confidenceInterval
#' @import Hmisc
confidenceInterval <-  function(calcMethod = "weighted", dataValue = as.numeric(),
                                dataWeights = as.numeric(), pValues= c(0.1, 0.9),
                                na.rm = FALSE){
  # Define function to use for each method
  qtFunction <- switch (calcMethod,
                        "average" = quantile,
                        "robust" = quantile,
                        "weighted" = Hmisc::wtd.quantile)

  # If there are zero data values return NA
  if(length(dataValue) == 0)
    return(c(NA, NA))
  
  # If there are 1 data value return the data value as limits on the interval
  if(length(dataValue) == 1)
    return(c(dataValue, dataValue))
  
  # If weights are not passed then we use weights of 1 for all dataValues
  if(length(dataWeights) == 0)
    dataWeights <- rep(1, length(dataValue))

  # Calculate percentiles
  if(calcMethod == "weighted"){
    qtValues <- qtFunction(dataValue[dataWeights != 0],
                           weights = dataWeights[dataWeights != 0],
                           probs = pValues, type ='i/n', na.rm = na.rm)
  } else {
    qtValues <- qtFunction(dataValue, probs = pValues, na.rm = na.rm)
  }

  # If the correcpsonding average value is less than the low point then we
  # set the lower point to the minimum value. Conversely if the average value is
  # higher than the upper limit then we set the upper limit to the max value.
  minVal <- min(dataValue, na.rm = na.rm)
  maxVal <- max(dataValue, na.rm = na.rm)
  avgVal <- avgValue(calcMethod, dataValue, dataWeights, na.rm)

  if(avgVal < qtValues[[1]]){
    qtValues[[1]] <- minVal
  }

  if(avgVal > qtValues[[2]]){
    qtValues[[2]] <- minVal
  }

  return(qtValues)
}
