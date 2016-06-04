#' Calculate Standard Deviation
#'
#' Standard Deviation is calculated based on method passed to the function.
#' @param calcMethod Calculation method. Can be one of "weighted", "robust" or
#' "average"
#' @param dataValue Vector of values to calculate standard deviation for
#' @param dataWeights Vector of weights for weighted calculation
calcStdDev <- function(calcMethod = "weighted", dataValue = as.numeric(),
                       dataWeights = as.numeric(), na.rm = FALSE){

  # Function to calculate the weighted standard deviation
  weighted.sd <- function(x, w, na.rm = FALSE){

    sum.w <- sum(w, na.rm = na.rm)
    sum.w2 <- sum(w^2, na.rm = na.rm)
    mean.w <- sum(x * w,na.rm = na.rm) / sum(w, na.rm = na.rm)
    x.sd.w <- sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2))
    return(x.sd.w)
  }

  # If weight is not passed then we use weight 1 for all data values.
  if(length(dataWeights) == 0)
    dataWeights <- rep(1, length(dataValue))

  sdFunction <- switch (calcMethod,
                        "average" = sd,
                        "robust" = mad,
                        "weighted" = weighted.sd)

  if(calcMethod == "weighted"){

    sdValue <- sdFunction(dataValue, dataWeights)
  } else {
    sdValue <- sdFunction(dataValue, na.rm)
  }
  return(sdValue)
}
