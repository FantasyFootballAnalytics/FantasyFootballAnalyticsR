###########################
# File: Optimum Risk.R
# Description: Determines Optimum Risk Level to Take
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These calculations are from last year (they have not yet been updated for the upcoming season)
###########################

#Library
library("Rglpk")

#Load data
load(paste(getwd(),"/Data/AvgCost-2012.RData", sep=""))

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

plot(riskLevel,projectedPoints)
riskData <- as.data.frame(cbind(riskLevel,projectedPoints))
riskData[match(unique(riskData$projectedPoints),riskData$projectedPoints),c("riskLevel","projectedPoints")]

optimizeTeam(maxRisk=2.7)
optimizeTeam(maxRisk=2.8)
optimizeTeam(maxRisk=2.9)
optimizeTeam(maxRisk=3.0)
optimizeTeam(maxRisk=3.1)
optimizeTeam(maxRisk=3.4)
optimizeTeam(maxRisk=3.5)
optimizeTeam(maxRisk=3.6)
optimizeTeam(maxRisk=3.8) #optimal
optimizeTeam(maxRisk=4.8)
optimizeTeam(maxRisk=6.0)

#Plot
ggplot(data=riskData, aes(x=riskLevel, y=projectedPoints)) + geom_point(size=3) + xlab("Max Risk Level") + ylab("Total Projected Points") + ggtitle("Association Between Max Risk Level and Total Projected Points")
  #+ geom_smooth()
  #+ stat_smooth(method = "glm", family = binomial) 
 +stat_smooth(method="lm", model = log(projectedPoints) ~ riskLevel)