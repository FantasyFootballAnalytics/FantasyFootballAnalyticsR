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
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/AvgCost.RData", sep=""))

#Subset data
optimizeData <- na.omit(projections[sourceName == "averageRobust", c("name","player","pos","team","points","risk","inflatedCost"), with=FALSE])

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
    listOfPlayers <- optimizeTeam(points=optimizeData$points, playerCost=newCost, maxRisk=(max(optimizeData$risk)+1))$players  #UPDATE: maxrisk
    bidUpTo[i] <- j
    j <- j+1
  }
}

optimizeData[,bidUpTo := bidUpTo - 1]
optimizeData[bidUpTo == 0, bidUpTo := 1]
optimizeData <- optimizeData[,c("name","pos","team","bidUpTo"), with=FALSE]

projections <- merge(projections, optimizeData, all.x=TRUE, by=c("name","pos","team"), allow.cartesian=TRUE)
projections[is.na(bidUpTo), bidUpTo := 1]

#Save file
save(projections, file = paste(getwd(), "/Data/BidUpTo.RData", sep=""))
write.csv(projections, file=paste(getwd(), "/Data/BidUpTo.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(), "/Data/Historical Files/BidUpTo-", season, ".RData", sep=""))
write.csv(projections, file=paste(getwd(), "/Data/Historical Files/BidUpTo-", season, ".csv", sep=""), row.names=FALSE)
