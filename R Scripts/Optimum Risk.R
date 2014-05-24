###########################
# File: Optimum Risk.R
# Description: Determines Optimum Risk Level to Take
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/AvgCost-2014.RData", sep=""))

#Optimum Risk
projectedPoints <- vector(mode="numeric", length=length(seq(min(optimizeData$risk), max(optimizeData$risk), 0.1)))
riskLevel <- vector(mode="numeric", length=length(seq(min(optimizeData$risk), max(optimizeData$risk), 0.1)))
j <- 1
pb <- txtProgressBar(min = 0, max = max(optimizeData$risk), style = 3)
for (i in seq(0, max(optimizeData$risk), 0.1)){
  setTxtProgressBar(pb, i)
  projectedPoints[j] <- optimizeTeam(maxRisk=i)$optimum
  riskLevel[j] <- i
  j <- j+1
}

riskData <- as.data.frame(cbind(riskLevel,projectedPoints))
riskData[match(unique(riskData$projectedPoints),riskData$projectedPoints),c("riskLevel","projectedPoints")]

optimizeTeam(maxRisk=3.3) #optimal
optimizeTeam(maxRisk=3.4)
optimizeTeam(maxRisk=3.5)
optimizeTeam(maxRisk=3.7)
optimizeTeam(maxRisk=4.0)
optimizeTeam(maxRisk=4.1) #optimal
optimizeTeam(maxRisk=4.4)
optimizeTeam(maxRisk=5.4)
optimizeTeam(maxRisk=100)

#Plot
ggplot(data=riskData, aes(x=riskLevel, y=projectedPoints)) + geom_point(size=3) + xlab("Max Risk Level") + ylab("Total Projected Points") + ggtitle("Association Between Max Risk Level and Total Projected Points") # + geom_smooth()
ggsave(paste(getwd(),"/Figures/Optimum Risk 2014.jpg", sep=""))
dev.off()
