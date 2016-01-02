#Install packages that work with main libraries
install.packages(c("reshape", "MASS", "psych", "Rglpk", "XML"), dependencies=TRUE)

source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))
