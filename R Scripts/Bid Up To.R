###########################
# File: Bid Up To.R
# Description: Determines intrinsic value (in cost) of a player to bid up to
# i.e., the maximum bid up to which the player is still on the best team (the team that maximizes your possible points)
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These calculations are from last year (they have not yet been updated for the upcoming season)
###########################

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))

#Bid Up To
listOfPlayers <- vector(mode="character", length=numTotalStarters)
bidUpTo <- vector(mode="numeric", length=length(optimizeData$name))
newCost <- optimizeData$inflatedCost

pb <- txtProgressBar(min = 0, max = length(optimizeData$name), style = 3)
for(i in 1:length(optimizeData$name)){
  setTxtProgressBar(pb, i)
  j <- 1
  listOfPlayers <- rep(optimizeData$name[i],numTotalStarters)
  newCost <- optimizeData$inflatedCost
  while(!is.na(match(optimizeData$name[i],listOfPlayers))){
    newCost[i] <- j 
    listOfPlayers <- optimizeTeam(points=optimizeData$projections, playerCost=newCost, maxRisk=(max(optimizeData$risk)+1))$players  #UPDATE: maxrisk
    bidUpTo[i] <- j
    j <- j+1
  }
}

optimizeData$bidUpTo <- bidUpTo - 1
optimizeData$bidUpTo[optimizeData$bidUpTo==0] <- 1

projections <- merge(projections, optimizeData, all.x=TRUE)
projections <- projections[order(projections$overallRank),]
projections$bidUpTo[is.na(projections$bidUpTo)] <- 1
row.names(projections) <- 1:max(as.numeric(row.names(projections)))

#Save file
save(projections, file = paste(getwd(),"/Data/BidUpTo-2013.RData", sep=""))