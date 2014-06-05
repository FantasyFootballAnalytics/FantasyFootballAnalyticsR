###########################
# File: Bid Up To.R
# Description: Determines intrinsic value (in cost) of a player to bid up to
# i.e., the maximum bid up to which the player is still on the best team (the team that maximizes your possible points)
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
load(paste(getwd(),"/Data/AvgCost.RData", sep=""))

#Subset data
optimizeData <- na.omit(projections[,c("name","player","pos","projections","risk","inflatedCost")])

#Bid Up To
listOfPlayers <- vector(mode="character", length=numTotalStarters)
bidUpTo <- vector(mode="numeric", length=length(optimizeData$name))
newCost <- optimizeData$inflatedCost

pb <- txtProgressBar(min = 0, max = length(optimizeData$name), style = 3)
for(i in 1:length(optimizeData$name)){
  setTxtProgressBar(pb, i)
  j <- 1
  listOfPlayers <- rep(optimizeData$player[i],numTotalStarters)
  newCost <- optimizeData$inflatedCost
  while(!is.na(match(optimizeData$player[i],listOfPlayers))){
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
save(projections, file = paste(getwd(),"/Data/BidUpTo.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/BidUpTo.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Files/BidUpTo-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Files/BidUpTo-2014.csv", sep=""), row.names=FALSE)
