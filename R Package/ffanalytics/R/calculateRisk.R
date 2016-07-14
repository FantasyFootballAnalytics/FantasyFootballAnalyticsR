#' Risk calculation based on to variables
#'
#' Calculation of risk is done by scaling the standard deviation variables
#' passed and averaging them before returning a measure with mean 5 and standard
#' deviation of 2
#' @export calculateRisk
calculateRisk <- function(var1, var2){
  var1 <- as.numeric(var1)
  var2 <- as.numeric(var2)
  Z_var1 <- scale(var1)
  Z_var2 <- scale(var2)

  Z_var1[is.na(Z_var1)] <- Z_var2[is.na(Z_var1)]
  Z_var2[is.na(Z_var2)] <- Z_var1[is.na(Z_var2)]

  riskValue <- 2 * scale(rowMeans(data.frame(Z_var1, Z_var2), na.rm=TRUE)) + 5

  return(riskValue)

}
