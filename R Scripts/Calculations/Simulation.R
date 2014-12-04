###########################
# File: Simulation.R
# Description: Determines Optimum Roster to Maximize Projected Points and Minimize Risk Based on Simulation
# Date: 5/15/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Specify Maximum Risk
#maxRisk <- 3.8

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/BidUpTo.RData", sep=""))
#load(paste(getwd(),"/Data/projectedWithActualPoints.RData", sep=""))

#Roster Optimization
optimizeData <- na.omit(projections[,c("name","player","pos","projections","risk","inflatedCost","sdPts")])
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

#Roster Optimization Simulation
iterations <- 100000
solutionList <- matrix(nrow=dim(optimizeData)[1], ncol=iterations)
pb <- txtProgressBar(min = 0, max = iterations, style = 3)
for (i in 1:iterations){
  setTxtProgressBar(pb, i)
  optimizeData$simPts <- mapply(function(x,y) rnorm(n=1, mean=x, sd=y), x=optimizeData$projections, y=optimizeData$sdPts)
  solutionList[,i] <- optimizeTeam(points=optimizeData$simPts, maxRisk=100)$solution
}

solutionSum <- rowSums(solutionList, na.rm=TRUE)
plot(density(na.omit(solutionSum)))
plot(density(na.omit(solutionSum ^ (1/3))))
plot(density(log(solutionSum + 1)))

#best: log(solutionSum + 1)

optimizeData$solutionSum <- solutionSum
optimizeData$percentage <- (optimizeData$solutionSum / iterations) * 100

optimizeData <- optimizeData[order(-optimizeData$solutionSum),c("name","player","pos","projections","risk","inflatedCost","sdPts","solutionSum","percentage")]
optimizeData$simulation <- log(optimizeData$solutionSum + 1)
projections <- merge(projections, optimizeData[,c("name","simulation")], by="name", all.x=TRUE)

#Save file
save(projections, file = paste(getwd(), "/Data/simulation.RData", sep=""))
write.csv(projections, file=paste(getwd(), "/Data/simulation.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(), "/Data/Historical Files/simulation-", season, ".RData", sep=""))
write.csv(projections, file=paste(getwd(), "/Data/Historical Files/simulation-", season, ".csv", sep=""), row.names=FALSE)

#View Data
optimizeData

#Top QBs
head(optimizeData[which(optimizeData$pos == "QB"),])
head(optimizeData[which(optimizeData$pos == "QB" & optimizeData$risk < 5),])

#Top RBs
head(optimizeData[which(optimizeData$pos == "RB"),])
head(optimizeData[which(optimizeData$pos == "RB" & optimizeData$risk < 5),])

#Top WRs
head(optimizeData[which(optimizeData$pos == "WR"),])
head(optimizeData[which(optimizeData$pos == "WR" & optimizeData$risk < 5),])

#Top TEs
head(optimizeData[which(optimizeData$pos == "TE"),])
head(optimizeData[which(optimizeData$pos == "TE" & optimizeData$risk < 5),])

#View Specific Players
projectedWithActualPts[projectedWithActualPts$player == "Shonn Greene",]
projectedWithActualPts[projectedWithActualPts$player == "Ray Rice",]
projectedWithActualPts[projectedWithActualPts$player == "Jermaine Gresham",]
projectedWithActualPts[projectedWithActualPts$player == "Reggie Wayne",]

#Optimize Solution Sum for Cost
optimizeTeam(points=optimizeData$solutionSum, maxRisk=100)
sum(optimizeData[optimizeData$player %in% optimizeTeam(points=optimizeData$solutionSum, maxRisk=100)$players,"projections"]) #pts: 1567

optimizeTeam(points=optimizeData$solutionSum, maxRisk=5.0)
sum(optimizeData[optimizeData$player %in% optimizeTeam(points=optimizeData$solutionSum, maxRisk=5.0)$players,"projections"]) #pts: 1553

optimizeTeam(points=optimizeData$solutionSum, maxRisk=3.8)
sum(optimizeData[optimizeData$player %in% optimizeTeam(points=optimizeData$solutionSum, maxRisk=3.8)$players,"projections"]) #pts: 1526

#Iterate solutions
projectedPoints <- vector(mode="numeric", length=length(seq(min(optimizeData$risk), max(optimizeData$risk), 0.1)))
riskLevel <- vector(mode="numeric", length=length(seq(min(optimizeData$risk), max(optimizeData$risk), 0.1)))
j <- 1
pb <- txtProgressBar(min = 0, max = max(optimizeData$risk), style = 3)
for (i in seq(0, max(optimizeData$risk), 0.1)){
  setTxtProgressBar(pb, i)
  projectedPoints[j] <- sum(optimizeData[optimizeData$player %in% optimizeTeam(points=log(optimizeData$solutionSum + 1), maxRisk=i)$players,"projections"]) #transform with log or cube root to not give so much weight to highest players
  riskLevel[j] <- i
  j <- j+1
}

riskData <- as.data.frame(cbind(riskLevel,projectedPoints))
riskTable <- riskData[match(unique(riskData$projectedPoints),riskData$projectedPoints),c("riskLevel","projectedPoints")]
riskTable$PtsRiskRatio <- riskTable$projectedPoints / riskTable$riskLevel
riskTable[order(riskTable$projectedPoints),]
plot(riskTable$riskLevel, riskTable$projectedPoints)

#Simulation = log(solutionSum)
optimizeTeam(points=optimizeData$simulation, maxRisk=3.3)
optimizeTeam(points=optimizeData$simulation, maxRisk=3.4)
optimizeTeam(points=optimizeData$simulation, maxRisk=3.5)
optimizeTeam(points=optimizeData$simulation, maxRisk=3.6)
optimizeTeam(points=optimizeData$simulation, maxRisk=3.7)
optimizeTeam(points=optimizeData$simulation, maxRisk=4.1)
optimizeTeam(points=optimizeData$simulation, maxRisk=4.3) #optimal
optimizeTeam(points=optimizeData$simulation, maxRisk=4.4)
optimizeTeam(points=optimizeData$simulation, maxRisk=4.8)
optimizeTeam(points=optimizeData$simulation, maxRisk=4.9)
optimizeTeam(points=optimizeData$simulation, maxRisk=5.0)
optimizeTeam(points=optimizeData$simulation, maxRisk=5.7)
optimizeTeam(points=optimizeData$simulation, maxRisk=6.8)

#Raw solutionSum scores
optimizeTeam(points=optimizeData$solutionSum, maxRisk=3.3)
optimizeTeam(points=optimizeData$solutionSum, maxRisk=3.4)
optimizeTeam(points=optimizeData$solutionSum, maxRisk=3.5)
optimizeTeam(points=optimizeData$solutionSum, maxRisk=3.6)
optimizeTeam(points=optimizeData$solutionSum, maxRisk=3.7)
optimizeTeam(points=optimizeData$solutionSum, maxRisk=4.1)
optimizeTeam(points=optimizeData$solutionSum, maxRisk=4.3) #optimal
optimizeTeam(points=optimizeData$solutionSum, maxRisk=4.4)
optimizeTeam(points=optimizeData$solutionSum, maxRisk=4.8)
optimizeTeam(points=optimizeData$solutionSum, maxRisk=4.9)
optimizeTeam(points=optimizeData$solutionSum, maxRisk=6.8)

#Set Optimal Risk
optimalRisk <- 4.2

###Determine Points for Team that Maximizes Log of Solution Sum with Risk < Optimal Risk
#Solution
optimizeTeam(points=optimizeData$simulation, maxRisk=optimalRisk)

#Roster + Projections
optimizeData[optimizeData$player %in% optimizeTeam(points=optimizeData$simulation, maxRisk=optimalRisk)$players, c("player","projections")]

#Sum of Projected Points: 1436
sum(optimizeData[optimizeData$player %in% optimizeTeam(points=optimizeData$simulation, maxRisk=optimalRisk)$players, "projections"])

#Maximum Possible Projected Points with Same Risk: 1477
optimizeTeam(maxRisk=optimalRisk)

#Maximum Possible Projected Points with Maximal Risk: 1491
optimizeTeam(maxRisk=100)

#Chosen Lineup Points: 1454
playersOnTeam <- c("Andrew Luck", "Jamaal Charles", "Doug Martin", "Frank Gore", "Randall Cobb", "Andre Johnson", "Jason Witten")
optimizeData[optimizeData$player %in% playersOnTeam, c("player", "projections", "inflatedCost", "risk")]
sum(optimizeData[optimizeData$player %in% playersOnTeam, "projections"])
sum(optimizeData[optimizeData$player %in% playersOnTeam, "inflatedCost"])
