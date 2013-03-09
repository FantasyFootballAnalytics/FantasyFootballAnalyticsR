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

#Load data
load(paste(getwd(),"/Data/BidUpTo-2012.RData", sep=""))

#Subset data
draftData <- projections[,c("name","pos","team","projectedPtsLatent","vor","sdPick","sdPts","risk","avgCost","inflatedCost","bidUpTo")]
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

draftData

#Day of Draft
removedPlayers <- na.omit(draftData)
removedPlayers

#Remove players
#Include Bid Up To
optimizeDraft <- function(points=removedPlayers$projectedPtsLatent, playerCost=removedPlayers$inflatedCost, maxRisk=maxRisk, omit=NULL){ #can change points, cost, or risk
  if (length(omit) > 0){
    for(i in 1:length(omit)){
      removedPlayers <- removedPlayers[-which(removedPlayers$name==omit[i]),]
    }
  } else {}
  
  num.players <- length(removedPlayers$name)
  var.types <- rep("B", num.players)
  
  A <- rbind(as.numeric(removedPlayers$pos == "QB"), # num QB
             as.numeric(removedPlayers$pos == "RB"), # num RB
             as.numeric(removedPlayers$pos == "WR"), # num WR
             as.numeric(removedPlayers$pos == "TE"), # num TE
             diag(removedPlayers$risk),              # player's risk
             playerCost,                           # total cost
             rep(1,num.players))                   # num of players in starting lineup     
  
  dir <- c("==",
           ">=",
           ">=",
           ">=",
           rep("<=", num.players),
           "<=",
           "==")
  
  b <- c(numQBstarters,
         numRBstarters,
         numWRstarters,
         numTEstarters,
         rep(maxRisk, num.players),
         maxCost,
         numTotalStarters)
  
  sol <- Rglpk_solve_LP(obj = points, mat = A, dir = dir, rhs = b,types = var.types, max = TRUE)
  sol$playerInfo <- as.data.frame(cbind(removedPlayers[sol$solution == 1,"name"],round(points[sol$solution == 1],2),round(removedPlayers[sol$solution == 1,"risk"],2),removedPlayers[sol$solution == 1,"avgCost"],playerCost[sol$solution == 1],removedPlayers[sol$solution == 1,"bidUpTo"]))
  names(sol$playerInfo) <- c("name","points","risk","avgCost","inflatedCost","bidUpTo")
  sol$totalCost <- sum(removedPlayers$inflatedCost * sol$solution)
  sol$players <- removedPlayers$name[sol$solution == 1]
  return(sol)
}

#Update with drafted (i.e., unavailable) players
drafted <- c("Aaron Rodgers","Steven Jackson")

optimizeDraft(maxRisk=3.8)
optimizeDraft(maxRisk=3.8,omit=c("Aaron Rodgers","Steven Jackson"))
optimizeDraft(maxRisk=3.8,omit=drafted)

draftData[!(draftData$name %in% drafted),]