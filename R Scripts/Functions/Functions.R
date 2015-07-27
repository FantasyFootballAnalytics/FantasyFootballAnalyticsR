###########################
# File: Functions.R
# Description: Fantasy Football Functions
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Library
library("Rglpk")

#No scientific notation
options(scipen=999)

#Function that takes a row sum and retains NAs when all values in the row are NA
mySum <- function(data){
  dataSum <- rowSums(data, na.rm=T) 
  dataSum[which(rowMeans(is.na(data))==1)] <- NA
  return(dataSum)
}

#Converts variable types of multiple columns of a dataframe at once
convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, function(x) FUN1(as.character(x)))
  as.data.frame(out)
}

#Convert to name for merging by removing all spaces and punctuation and converting to all caps
nameMerge <- function(name){
  newName <- toupper(gsub("Sr|Jr|III|[[:punct:]]| ", "", name))
  return(newName)
}

#Function for calculating Mean Absolute Scaled Error (MASE)
calculateMASE <- function(forecast, actual){
  mydata <- data.frame(na.omit(cbind(forecast, actual)))

  errors <- mydata$actual - mydata$forecast
  scalingFactor <- mean(abs(mydata$actual - mean(mydata$forecast)))
  scaledErrors <- errors/scalingFactor
  
  MASE <- mean(abs(scaledErrors))
  return(MASE)
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

#Function that generates n random positive integers that sum to constrained value
simulateIntegers <- function(n, sum, sd, pos.only = TRUE){
  if(sum == 0 & pos.only == TRUE){
    vec <- rep(0, n)
  } else{
    vec <- rnorm(n, sum/n, sd)
    if (abs(sum(vec)) < 0.01) vec <- vec + 1
    vec <- round(vec / sum(vec) * sum)
    deviation <- sum - sum(vec)
    for (. in seq_len(abs(deviation))){
      vec[i] <- vec[i <- sample(n, 1)] + sign(deviation)
    }
    if (pos.only) while (any(vec < 0)){
      negs <- vec < 0
      pos  <- vec > 0
      vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
      vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
    }
  }
  vec
}

#Function that generates n random positive numbers that sum to constrained value
simulateNumbers <- function(n, sum, sd, pos.only = TRUE){
  if(sum == 0 & pos.only == TRUE){
    vec <- rep(0, n)
  } else{
    vec <- rnorm(n, sum/n, sd)
    vec <- vec / sum(vec) * sum
    if (pos.only) while (any(vec < 0)){
      negs <- vec < 0
      pos  <- vec > 0
      vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
      vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
    }
  }
  vec
}

#Create Optimization Function
optimizeTeam <- function(points=optimizeData$points, playerCost=optimizeData$inflatedCost, maxRisk=maxRisk){ #can change points, cost, or risk
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
  sol$playerInfo <- as.data.frame(cbind(optimizeData[sol$solution == 1, "player", with=FALSE], round(points[sol$solution == 1], 2), round(optimizeData[sol$solution == 1, "risk", with=FALSE], 2), playerCost[sol$solution == 1]))
  names(sol$playerInfo) <- c("player","points","risk","cost")
  sol$totalCost <- sum(playerCost * sol$solution)
  sol$players <- optimizeData$player[sol$solution == 1]
  return(sol)
}

#Draft Day Optimization: Allows omitting unavailable (drafted) players and includes BidUpTo in summary table
optimizeDraft <- function(points=removedPlayers$points, playerCost=removedPlayers$inflatedCost, maxRisk=maxRisk, omit=NULL, team=myteam){ #can change points, cost, or risk
  #Omit players that have already been drafted
  omitName <- toupper(gsub("[[:punct:]]", "", gsub(" ", "", omit)))
  removedPlayers <- removedPlayers[! removedPlayers$name %in% omitName,]
  
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
  sol$playerInfo <- as.data.frame(cbind(removedPlayers[sol$solution == 1, "player", with=FALSE], round(points[sol$solution == 1], 2), round(removedPlayers[sol$solution == 1, "risk", with=FALSE], 2), removedPlayers[sol$solution == 1, "avgCost", with=FALSE], playerCost[sol$solution == 1], removedPlayers[sol$solution == 1, "bidUpTo", with=FALSE]))
  names(sol$playerInfo) <- c("player","points","risk","avgCost","inflatedCost","bidUpTo")
  sol$totalCost <- sum(removedPlayers$inflatedCost * sol$solution)
  sol$players <- removedPlayers$player[sol$solution == 1]
  return(sol)
}

#Convert cities/nicknames to team abbreviations
convertTeamAbbreviation <- function(x){
  x[grep("Arizona", x, ignore.case=TRUE)] <- "ARZ"
  x[grep("Cardinals", x, ignore.case=TRUE)] <- "ARZ"
  
  x[grep("Atlanta", x, ignore.case=TRUE)] <- "ATL"
  x[grep("Falcons", x, ignore.case=TRUE)] <- "ATL"
  
  x[grep("Baltimore", x, ignore.case=TRUE)] <- "BAL"
  x[grep("Ravens", x, ignore.case=TRUE)] <- "BAL"
  
  x[grep("Buffalo", x, ignore.case=TRUE)] <- "BUF"
  x[grep("Bills", x, ignore.case=TRUE)] <- "BUF"
  
  x[grep("Carolina", x, ignore.case=TRUE)] <- "CAR"
  x[grep("Panthers", x, ignore.case=TRUE)] <- "CAR"
  
  x[grep("Chicago", x, ignore.case=TRUE)] <- "CHI"
  x[grep("Bears", x, ignore.case=TRUE)] <- "CHI"
  
  x[grep("Cincinnati", x, ignore.case=TRUE)] <- "CIN"
  x[grep("Bengals", x, ignore.case=TRUE)] <- "CIN"
  
  x[grep("Cleveland", x, ignore.case=TRUE)] <- "CLE"
  x[grep("Browns", x, ignore.case=TRUE)] <- "CLE"
  
  x[grep("Dallas", x, ignore.case=TRUE)] <- "DAL"
  x[grep("Cowboys", x, ignore.case=TRUE)] <- "DAL"
  
  x[grep("Denver", x, ignore.case=TRUE)] <- "DEN"
  x[grep("Broncos", x, ignore.case=TRUE)] <- "DEN"
  
  x[grep("Detroit", x, ignore.case=TRUE)] <- "DET"
  x[grep("Lions", x, ignore.case=TRUE)] <- "DET"
  
  x[grep("Free", x, ignore.case=TRUE)] <- "FA"
  x[grep("Agent", x, ignore.case=TRUE)] <- "FA"
  
  x[grep("Green Bay", x, ignore.case=TRUE)] <- "GB"
  x[grep("Packers", x, ignore.case=TRUE)] <- "GB"
  
  x[grep("Houston", x, ignore.case=TRUE)] <- "HOU"
  x[grep("Texans", x, ignore.case=TRUE)] <- "HOU"
  
  x[grep("Indianapolis", x, ignore.case=TRUE)] <- "IND"
  x[grep("Colts", x, ignore.case=TRUE)] <- "IND"
  
  x[grep("Jacksonville", x, ignore.case=TRUE)] <- "JAC"
  x[grep("Jaguars", x, ignore.case=TRUE)] <- "JAC"
  
  x[grep("Kansas City", x, ignore.case=TRUE)] <- "KC"
  x[grep("Chiefs", x, ignore.case=TRUE)] <- "KC"
  
  x[grep("Miami", x, ignore.case=TRUE)] <- "MIA"
  x[grep("Dolphins", x, ignore.case=TRUE)] <- "MIA"
  
  x[grep("Minnesota", x, ignore.case=TRUE)] <- "MIN"
  x[grep("Vikings", x, ignore.case=TRUE)] <- "MIN"
  
  x[grep("New England", x, ignore.case=TRUE)] <- "NE"
  x[grep("Patriots", x, ignore.case=TRUE)] <- "NE"
  
  x[grep("New Orleans", x, ignore.case=TRUE)] <- "NO"
  x[grep("Saints", x, ignore.case=TRUE)] <- "NO"
  
  x[grep("Jets", x, ignore.case=TRUE)] <- "NYJ"
  
  x[grep("Giants", x, ignore.case=TRUE)] <- "NYG"
  
  x[grep("Oakland", x, ignore.case=TRUE)] <- "OAK"
  x[grep("Raiders", x, ignore.case=TRUE)] <- "OAK"
  
  x[grep("Philadelphia", x, ignore.case=TRUE)] <- "PHI"
  x[grep("Eagles", x, ignore.case=TRUE)] <- "PHI"
  
  x[grep("Pittsburgh", x, ignore.case=TRUE)] <- "PIT"
  x[grep("Steelers", x, ignore.case=TRUE)] <- "PIT"
  
  x[grep("San Diego", x, ignore.case=TRUE)] <- "SD"
  x[grep("Chargers", x, ignore.case=TRUE)] <- "SD"
  
  x[grep("Saint Louis", x, ignore.case=TRUE)] <- "STL"
  x[grep("St Louis", x, ignore.case=TRUE)] <- "STL"
  x[grep("St. Louis", x, ignore.case=TRUE)] <- "STL"
  x[grep("Rams", x, ignore.case=TRUE)] <- "STL"
  
  x[grep("San Francisco", x, ignore.case=TRUE)] <- "SF"
  x[grep("49ers", x, ignore.case=TRUE)] <- "SF"
  
  x[grep("Seattle", x, ignore.case=TRUE)] <- "SEA"
  x[grep("Seahawks", x, ignore.case=TRUE)] <- "SEA"
  
  x[grep("Tampa Bay", x, ignore.case=TRUE)] <- "TB"
  x[grep("Buccaneers", x, ignore.case=TRUE)] <- "TB"
  
  x[grep("Tennessee", x, ignore.case=TRUE)] <- "TEN"
  x[grep("Titans", x, ignore.case=TRUE)] <- "TEN"
  
  x[grep("Washington", x, ignore.case=TRUE)] <- "WAS"
  x[grep("Redskins", x, ignore.case=TRUE)] <- "WAS"
  
  return(x)
}

#Convert team abbreviations to cities/nicknames
convertTeamName <- function(x){
  x[which(toupper(x) == "ARI")] <- "Arizona Cardinals"
  x[which(toupper(x) == "ARZ")] <- "Arizona Cardinals"
  x[which(toupper(x) == "ATL")] <- "Atlanta Falcons"
  x[which(toupper(x) == "BAL")] <- "Baltimore Ravens"
  x[which(toupper(x) == "BUF")] <- "Buffalo Bills"
  x[which(toupper(x) == "CAR")] <- "Carolina Panthers"
  x[which(toupper(x) == "CHI")] <- "Chicago Bears"
  x[which(toupper(x) == "CIN")] <- "Cincinnati Bengals"
  x[which(toupper(x) == "CLE")] <- "Cleveland Browns"
  x[which(toupper(x) == "DAL")] <- "Dallas Cowboys"
  x[which(toupper(x) == "DEN")] <- "Denver Broncos"
  x[which(toupper(x) == "DET")] <- "Detroit Lions"
  x[which(toupper(x) == "FA")] <- "Free Agent"
  x[which(toupper(x) == "GB")] <- "Green Bay Packers"
  x[which(toupper(x) == "GBP")] <- "Green Bay Packers"
  x[which(toupper(x) == "HOU")] <- "Houston Texans"
  x[which(toupper(x) == "IND")] <- "Indianapolis Colts"
  x[which(toupper(x) == "JAC")] <- "Jacksonville Jaguars"
  x[which(toupper(x) == "JAX")] <- "Jacksonville Jaguars"
  x[which(toupper(x) == "KC")] <- "Kansas City Chiefs"
  x[which(toupper(x) == "KCC")] <- "Kansas City Chiefs"
  x[which(toupper(x) == "MIA")] <- "Miami Dolphins"
  x[which(toupper(x) == "MIN")] <- "Minnesota Vikings"
  x[which(toupper(x) == "NEW")] <- "New England Patriots"
  x[which(toupper(x) == "NE")] <- "New England Patriots"
  x[which(toupper(x) == "NOS")] <- "New Orleans Saints"
  x[which(toupper(x) == "NO")] <- "New Orleans Saints"
  x[which(toupper(x) == "NYJ")] <- "New York Jets"
  x[which(toupper(x) == "NYG")] <- "New York Giants"
  x[which(toupper(x) == "OAK")] <- "Oakland Raiders"
  x[which(toupper(x) == "PHI")] <- "Philadelphia Eagles"
  x[which(toupper(x) == "PIT")] <- "Pittsburgh Steelers"
  x[which(toupper(x) == "SD")] <- "San Diego Chargers"
  x[which(toupper(x) == "SDC")] <- "San Diego Chargers"
  x[which(toupper(x) == "SAN")] <- "San Diego Chargers"
  x[which(toupper(x) == "ST")] <- "St. Louis Rams"
  x[which(toupper(x) == "STL")] <- "St. Louis Rams"
  x[which(toupper(x) == "SF")] <- "San Francisco 49ers"
  x[which(toupper(x) == "SFO")] <- "San Francisco 49ers"
  x[which(toupper(x) == "SEA")] <- "Seattle Seahawks"
  x[which(toupper(x) == "TB")] <- "Tampa Bay Buccaneers"
  x[which(toupper(x) == "TBB")] <- "Tampa Bay Buccaneers"
  x[which(toupper(x) == "TEN")] <- "Tennessee Titans"
  x[which(toupper(x) == "WAS")] <- "Washington Redskins"
  x[which(toupper(x) == "WSH")] <- "Washington Redskins"
  
  return(x)
}

#Convert team abbreviations to cities/nicknames
cleanTeamAbbreviations <- function(x){
  x[which(toupper(x) == "ARI")] <- "ARZ"
  x[which(toupper(x) == "GBP")] <- "GB"
  x[which(toupper(x) == "JAX")] <- "JAC"
  x[which(toupper(x) == "KCC")] <- "KC"
  x[which(toupper(x) == "NEW")] <- "NE"
  x[which(toupper(x) == "NOS")] <- "NO"
  x[which(toupper(x) == "SAN")] <- "SD"
  x[which(toupper(x) == "SDC")] <- "SD"
  x[which(toupper(x) == "ST")] <- "STL"
  x[which(toupper(x) == "SFO")] <- "SF"
  x[which(toupper(x) == "TBB")] <- "TB"
  x[which(toupper(x) == "WSH")] <- "WAS"
  
  return(x)
}

#Function for adding missing rows to a data.table by cross-join
CJ.dt = function(...) {
  rows = do.call(CJ, lapply(list(...), function(x) if(is.data.frame(x)) seq_len(nrow(x)) else seq_along(x)));
  do.call(data.table, Map(function(x, y) x[y], list(...), rows))
}

# Function to calculate the location estimate for the wilcox test
wilcox.loc <- function(vec){
  n <- length(vec)
  
  # If number of observations is less than 2 then we just return mean as location estimate
  if(n <= 2){
    return(mean(vec, na.rm = TRUE))
  }
  
  # Calculating the paired avagerages
  pairAvg <- sort(c(vec, combn(vec, 2, function(x)mean(x, na.rm = TRUE))))
  
  return(median(pairAvg, na.rm = TRUE))
}

# Function to calculate DST points from the ptsAllowed brackets
dstPts <- function(ptsAllow, brackets){
  is.season <- all(ptsAllow > 100)
  if(is.season){
    ptsAllow <- ptsAllow / 16
  }
  pts <- rep(0, length(ptsAllow))
  for(r in nrow(brackets):1){
    pts[ptsAllow <= brackets$threshold[r]] <- brackets$points[r]
  }
  
  if(is.season){
    pts <- pts * 16
  }
  return(as.numeric(pts))
}
