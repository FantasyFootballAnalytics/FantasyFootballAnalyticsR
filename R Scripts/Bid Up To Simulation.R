###########################
# File: Bid Up To Simulation.R
# Description: Simulates intrinsic value (in cost) of a player to bid up to
# i.e., the maximum bid up to which the player is still on the best team (the team that maximizes your possible points)
# Date: 6/1/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
###########################

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Data
load(paste(getwd(),"/Data/simulation-2013.RData", sep=""))

#Roster Optimization
optimizeData <- na.omit(projections[,c("name","pos","projections","risk","inflatedCost","sdPts")]) #projectedPtsLatent
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

#Set iterations
iterations <- 1000

#Bid Up To (i=player, j=cost, k=iteration)
listOfPlayers <- vector(mode="character", length=numTotalStarters)
#bidUpTo <- vector(mode="numeric", length=length(optimizeData$name))
bidUpTo <- matrix(nrow=length(optimizeData$name), ncol=iterations)
newCost <- optimizeData$inflatedCost

#Simulated Points
simulatedPoints <- matrix(nrow=length(optimizeData$name), ncol=iterations)
for(i in 1:iterations){
  simulatedPoints[,i] <- mapply(function(x,y) rnorm(n=1, mean=x, sd=y), x=optimizeData$projections, y=optimizeData$sdPts)
}

pb <- txtProgressBar(min = 0, max = length(optimizeData$name), style = 3)
for(i in 1:length(optimizeData$name)){
  setTxtProgressBar(pb, i)
  listOfPlayers <- rep(optimizeData$name[i],numTotalStarters)
  newCost <- optimizeData$inflatedCost    
  
  for (k in 1:iterations){
    j <- 1
    listOfPlayers <- optimizeData$name[i]
      
    while(!is.na(match(optimizeData$name[i],listOfPlayers)) & j < maxCost){
      newCost[i] <- j
      
      listOfPlayers <- optimizeTeam(points=simulatedPoints[,k], playerCost=newCost, maxRisk=(max(optimizeData$risk)+1))$players
      bidUpTo[i,k] <- j - 1
      j <- j+1
    }
  }
}

bidUpTo[bidUpTo == (maxCost - 2)] <- NA
bidUpTo[bidUpTo == (maxCost - 1)] <- NA
bidUpTo[bidUpTo == (maxCost)] <- NA

#Calculate Robust Measure of Central Tendency: Hodges-Lehmann estimator (pseudo-median)
#optimizeData$bidUpTo <- rowMeans(bidUpTo)
for (i in 1:dim(bidUpTo)[1]){ 
  error <- try(suppressWarnings(wilcox.test(bidUpTo[i,], conf.int=TRUE)$estimate), silent=T) 
  ifelse(is(error,"try-error"), optimizeData$bidUpToSim[i] <- max(ceiling(mean(bidUpTo[i,], na.rm=TRUE)), 1, na.rm=TRUE), optimizeData$bidUpToSim[i] <- ceiling(suppressWarnings(wilcox.test(bidUpTo[i,], conf.int=TRUE)$estimate)))
}

optimizeData

#Merge with projections
projections <- merge(projections, optimizeData[,c("name","pos","bidUpToSim")], by=c("name","pos"), all=TRUE)

#Convert NAs to Zero
projections$bidUpToSim[is.na(projections$bidUpToSim)] <- 1

#Order players by vor #projections
projections <- projections[order(-projections$vor),] #projections$projections
row.names(projections) <- 1:dim(projections)[1]

#Save file
save(projections, file = paste(getwd(),"/Data/BidUpToSimulation-2013.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/CSV/BidUpToSimulation-2013.csv", sep=""), row.names=FALSE)
