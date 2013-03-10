###########################
# File: Draft Day.R
# Description: Continually recalculates optimal team given which players are available
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These calculations are from last year (they have not yet been updated for the upcoming season)
###########################

#League Settings
leagueCap <- 225

numQBstarters <- 1
numRBstarters <- 2
numWRstarters <- 2
numTEstarters <- 1
numTotalStarters <- 7
numTotalPlayers <- 20

maxRisk <- 3.8

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))

#Load data
load(paste(getwd(),"/Data/BidUpTo-2012.RData", sep=""))

#Subset data
draftData <- projections[,c("name","pos","team","projectedPtsLatent","vor","sdPick","sdPts","risk","avgCost","inflatedCost","bidUpTo")]
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

draftData

#Day of Draft
removedPlayers <- na.omit(draftData)
removedPlayers

#Update with drafted (i.e., unavailable) players
drafted <- c("Aaron Rodgers","Steven Jackson")

optimizeDraft(maxRisk=3.8)
optimizeDraft(maxRisk=3.8,omit=c("Aaron Rodgers","Steven Jackson"))
optimizeDraft(maxRisk=3.8,omit=drafted)

draftData[!(draftData$name %in% drafted),]