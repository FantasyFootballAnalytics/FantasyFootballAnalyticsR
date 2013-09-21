###########################
# File: Functions.R
# Description: Fantasy Football Functions
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
###########################

#Library
library("Rglpk")

#Function for calculating Mean Absolute Scaled Error (MASE)
calculateMASE <- function(f,y) { # f = vector with forecasts, y = vector with actuals
  if(length(f)!=length(y)){ stop("Vector length is not equal") }
  n <- length(f)
  return(mean(abs((y - f) / ((1/(n-1)) * sum(abs(y[2:n]-y[1:n-1]))))))
}

#Function for calculating the weighted standard deviation for mean/sd rescaling (used in the function below)
weighted.sd <- function(x, w){
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  x.sd.w <- sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2))
  return(x.sd.w)
}

#Function for rescaling the factor scores to have the same mean and sd as the original projections data
rescaleMeanSD <- function(f.scores, raw.data, loadings){
  fz.scores <- (f.scores + mean(f.scores))/(apply(f.scores, 2, sd)) #(f.scores + mean(f.scores))/(sd(f.scores))
  means <- apply(raw.data, 1, weighted.mean, w = loadings)
  sds <- apply(raw.data, 1, weighted.sd, w = loadings)
  grand.mean <- mean(means)
  grand.sd <- mean(sds)
  final.scores <- ((fz.scores * grand.sd) + grand.mean)
  return(final.scores)
}

#Function for rescaling the factor scores to have the same range as the average projections data
rescaleRange <- function(variable, minOutput, maxOutput){
  minObserved <- min(variable)
  maxObserved <- max(variable)
  values <- (maxOutput-minOutput)/(maxObserved-minObserved)*(variable-maxObserved)+maxOutput
  return(values)
}

#Create Optimization Function
optimizeTeam <- function(points=optimizeData$projections, playerCost=optimizeData$inflatedCost, maxRisk=maxRisk){ #can change points, cost, or risk #projectedPtsLatent
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
  #sol$totalCost <- sum(optimizeData$inflatedCost * sol$solution)
  sol$totalCost <- sum(playerCost * sol$solution)
  sol$players <- optimizeData$name[sol$solution == 1]
  return(sol)
}

#Draft Day Optimization: Allows omitting unavailable (drafted) players and includes BidUpTo in summary table
optimizeDraft <- function(points=removedPlayers$projections, playerCost=removedPlayers$inflatedCost, maxRisk=maxRisk, omit=NULL, team=myteam){ #can change points, cost, or risk #projectedPtsLatent
  #Omit players that have already been drafted
  removedPlayers <- removedPlayers[! removedPlayers$name %in% omit,]
  
  #Calculate how many players to draft at each position  
  numQBsToDraft <- numQBstarters - sum(myteam$pos == "QB")
  numRBsToDraft <- numRBstarters - sum(myteam$pos == "RB")
  numWRsToDraft <- numWRstarters - sum(myteam$pos == "WR")
  numTEsToDraft <- numTEstarters - sum(myteam$pos == "TE")
  
  numToDraft <- numTotalStarters - length(myteam$pos)
  
  #Calculate remaining cost
  remainingCost <- maxCost - sum(myteam$cost)
  
  #Set up matrices  
  num.players <- length(removedPlayers$name)
  var.types <- rep("B", num.players)
  
  A <- rbind(as.numeric(removedPlayers$pos == "QB"), # num QB
             as.numeric(removedPlayers$pos == "RB"), # num RB
             as.numeric(removedPlayers$pos == "WR"), # num WR
             as.numeric(removedPlayers$pos == "TE"), # num TE
             diag(removedPlayers$risk),              # player's risk
             playerCost,                             # total cost
             rep(1,num.players))                     # num of players in starting lineup     
  
  dir <- c("==",
           ">=",
           ">=",
           ">=",
           rep("<=", num.players),
           "<=",
           "==")
  
  b <- c(numQBsToDraft,
         numRBsToDraft,
         numWRsToDraft,
         numTEsToDraft,
         rep(maxRisk, num.players),
         remainingCost,
         numToDraft)
  
  sol <- Rglpk_solve_LP(obj = points, mat = A, dir = dir, rhs = b,types = var.types, max = TRUE)
  sol$playerInfo <- as.data.frame(cbind(removedPlayers[sol$solution == 1,"name"],round(points[sol$solution == 1],2),round(removedPlayers[sol$solution == 1,"risk"],2),removedPlayers[sol$solution == 1,"avgCost"],playerCost[sol$solution == 1],removedPlayers[sol$solution == 1,"bidUpTo"]))
  names(sol$playerInfo) <- c("name","points","risk","avgCost","inflatedCost","bidUpTo")
  sol$totalCost <- sum(removedPlayers$inflatedCost * sol$solution)
  sol$players <- removedPlayers$name[sol$solution == 1]
  return(sol)
}
