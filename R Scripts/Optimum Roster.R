###########################
# File: Optimum Roster.R
# Description: Determines Optimum Roster to Maximize Projected Points and Minimize Risk
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These calculations are from last year (they have not yet been updated for the upcoming season)
###########################

#League settings

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
load(paste(getwd(),"/Data/AvgCost-2012.RData", sep=""))

#Roster Optimization
optimizeData <- na.omit(projections[,c("name","pos","projectedPtsLatent","risk","inflatedCost")])
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

#Create Optimization Function
optimizeTeam <- function(points=optimizeData$projectedPtsLatent, playerCost=optimizeData$inflatedCost, maxRisk=maxRisk){ #can change points, cost, or risk
  num.players <- length(optimizeData$name)
  var.types <- rep("B", num.players)
  
  A <- rbind(as.numeric(optimizeData$pos == "QB"), # num QB
             as.numeric(optimizeData$pos == "RB"), # num RB
             as.numeric(optimizeData$pos == "WR"), # num WR
             as.numeric(optimizeData$pos == "TE"), # num TE
             diag(optimizeData$risk),              # player's risk
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
  sol$playerInfo <- as.data.frame(cbind(optimizeData[sol$solution == 1,"name"],round(points[sol$solution == 1],2),round(optimizeData[sol$solution == 1,"risk"],2),playerCost[sol$solution == 1]))
  names(sol$playerInfo) <- c("name","points","risk","cost")
  sol$totalCost <- sum(optimizeData$inflatedCost * sol$solution)
  sol$players <- optimizeData$name[sol$solution == 1]
  return(sol)
}

optimizeTeam(maxRisk=maxRisk)
optimizeTeam(maxRisk=100)