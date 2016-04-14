#' Risk calculation based on to variables
#'
#' Calculation of risk is done by scaling the standard deviation variables
#' passed and averaging them before returning a measure with mean 5 and standard
#' deviation of 2
#' @export calculateRisk
calculateRisk <- function(var1, var2){

  Z_var1 <- scale(var1)
  Z_var2 <- scale(var2)

  Z_var1[is.na(Z_var1)] <- Z_var2[is.na(Z_var1)]
  Z_var2[is.na(Z_var2)] <- Z_var1[is.na(Z_var2)]

  riskValue <- sqrt((Z_var1^2 + Z_var2^2)/2)
  riskValue <- ((riskValue * 2/(sd(riskValue, na.rm=TRUE))) + (3-(mean(riskValue, na.rm=TRUE))))

  return(riskValue)

}
