###########################
# File: avgValue.R
# Description: Function to calculate the average values based on different calculation methods
# Date: 2/13/2016
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# 
# To do:
###########################
avgValue <- function(calcMethod = "weighted", dataValue = as.numeric(), dataWeights = as.numeric(), na.rm = FALSE){
  allowedMethods <- c("average", "weighted", "robust")
  calcMethod <- match.arg(calcMethod, allowedMethods)
  # Function to calculate the location estimate for the wilcox test
  # If there are more than 2 observations then the function will return the 
  # median of the paired averages. If there are 2 or less observation a simple
  # mean will be returned.
  wilcox.loc <- function(vec, na.rm = FALSE){
    n <- length(vec)
    # If number of observations is less than 2 then we just return mean as location estimate
    if(n <= 2){
      return(mean(vec, na.rm = na.rm))
    }
    
    # Calculating the paired avagerages
    pairAvg <- sort(c(vec, combn(vec, 2, function(x)mean(x, na.rm = na.rm))))
    return(median(pairAvg, na.rm = na.rm))
  }
  
  # Checking to see if any weights have been passed. If not then we create a weights vector
  # consisting of 1s
  if(length(dataWeights) == 0)
    dataWeights <- rep(1, length(dataValue))
  
  # Determining which average to use.
  avgFunction <- switch(calcMethod,
                        "average" = mean,
                        "weighted" = weighted.mean,
                        "robust" = wilcox.loc)
  
  # If weighted average is requested then pass weights. Don't pass weights if not.
  if(calcMethod == "weighted"){
    returnValue <- avgFunction(as.numeric(dataValue), dataWeights, na.rm = TRUE) 
  } else {
    returnValue <- avgFunction(as.numeric(dataValue), na.rm = TRUE) 
  }
  
  return(returnValue)
}