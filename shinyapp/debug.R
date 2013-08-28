### Libraries
install.packages('httpuv')
install.packages('devtools')  # Install devtools if needed
devtools::install_github('shiny', 'rstudio')

load_or_install(c("shiny","Rglpk"))
library(shiny)
library(Rglpk)

### Load Data
shinyData <- read.csv(paste(path, "/GitHub/FantasyFootballAnalyticsR/shinyapp/shinyData.csv", sep=""))
#shinyData <- read.csv(paste(getwd(),"/shinyapp/shinyData.csv", sep=""))

### Run
runApp(paste(path, "/GitHub/FantasyFootballAnalyticsR/shinyapp", sep=""))
runApp("./shinyapp")

#setwd("./shinyapp")
#setwd("C:/Users/Isaac/Documents/GitHub/FantasyFootballAnalyticsR/")
#runApp("~/shinyapp")
#runApp()

### Inputs
numericInput("leagueCap", "League Cap:", 200),
numericInput("numTotalPlayers", "Total Number of Players on Roster:", 20),

numericInput("passYdsMultiplier", "Passing Yards Per Point:", 25),
numericInput("passTdsMultiplier", "Points Per Passing TD:", 4),
numericInput("passIntMultiplier", "Points Per Passing INT:", -2),
numericInput("rushYdsMultiplier", "Rushing Yards Per Point:", 10),
numericInput("rushTdsMultiplier", "Points Per Rushing TD:", 6),
numericInput("recMultiplier", "Points Per Reception:", 0),
numericInput("recYdsMultiplier", "Receiving Yards Per Point:", 10),
numericInput("recTdsMultiplier", "Points Per Receiving TD:", 6),
#numericInput("twoPtsMultiplier", "Points Per 2-Pt Conversions:", 0),
numericInput("fumbleMulitplier", "Points Per Fumble:", -2),

sliderInput("maxRisk", "Max Player Risk Tolerance:", 
            min = ceiling(min(shinyData$risk, na.rm=TRUE)), 
            max = ceiling(max(shinyData$risk, na.rm=TRUE)),
            step=.1, value = 5),

sliderInput("numQBs", "Number of Starting Quarterbacks:", min = 0, max = 4, value = 1),
sliderInput("numRBs", "Number of Starting Running Backs:", min = 0, max = 4, value = 2),
sliderInput("numWRs", "Number of Starting Wide Receivers:", min = 0, max = 4, value = 2),
sliderInput("numTEs", "Number of Starting Tight Ends:", min = 0, max = 4, value = 1),
sliderInput("numWRTEs", "Number of Starting WR/TEs:", min = 0, max = 4, value = 0),
sliderInput("numWRRBs", "Number of Starting WR/RBs:", min = 0, max = 4, value = 0),
sliderInput("numWRRBTEs", "Number of Starting WR/RB/TEs:", min = 0, max = 4, value = 1),
sliderInput("numQBWRRBTEs", "Number of Starting QB/WR/RB/TEs:", min = 0, max = 4, value = 0)
),

### Calculations

#Default inputs
leagueCap <- 225 #200
numTotalPlayers <- 20

passYdsMultiplier <- 25
passTdsMultiplier <- 4
passIntMultiplier <- -3 #-2
rushYdsMultiplier <- 10
rushTdsMultiplier <- 6
recMultiplier <- 0
recYdsMultiplier <- 8 #10
recTdsMultiplier <- 6
#twoPtsMultiplier <- 2
fumbleMulitplier <- -3 #-2

maxRisk <- 5

numQBs <- 1
numRBs <- 2
numWRs <- 2
numTEs <- 1
numWRTEs <- 0
numWRRBs <- 0
numWRRBTEs <- 1
numQBWRRBTEs <- 0

#Calculate Position Rank
shinyData$positionRank[shinyData$pos=="QB"] <- rank(-shinyData$projectedPts[shinyData$pos=="QB"], ties.method="min")
shinyData$positionRank[shinyData$pos=="RB"] <- rank(-shinyData$projectedPts[shinyData$pos=="RB"], ties.method="min")
shinyData$positionRank[shinyData$pos=="WR"] <- rank(-shinyData$projectedPts[shinyData$pos=="WR"], ties.method="min")
shinyData$positionRank[shinyData$pos=="TE"] <- rank(-shinyData$projectedPts[shinyData$pos=="TE"], ties.method="min")

#Calculate Overall Rank
shinyData$overallRank <- rank(-shinyData$projectedPts, ties.method="min")

#Apply 10% price premium to 33 players with highest projected points, apply 10% price premium for players lower than rank 66
shinyData$projectedCost[shinyData$overallRank <= 33] <- ceiling(shinyData$cost[shinyData$overallRank <= 33] * (leagueCap/200) * 1.1)
shinyData$projectedCost[shinyData$overallRank >= 34 & shinyData$overallRank <= 66] <- ceiling(shinyData$cost[shinyData$overallRank >= 34 & shinyData$overallRank <= 66] * (leagueCap/200) * 1.0)
shinyData$projectedCost[shinyData$overallRank >= 67] <- ceiling(shinyData$cost[shinyData$overallRank >= 67] * (leagueCap/200) * 0.9)
shinyData$projectedCost[is.na(shinyData$projectedCost)==TRUE] <- 1
shinyData$projectedCost[shinyData$projectedCost==0] <- 1

minQBs <- numQBs
maxQBs <- numQBs + numQBWRRBTEs
minRBs <- numRBs
maxRBs <- numRBs + numWRRBs + numWRRBTEs + numQBWRRBTEs
minWRs <- numWRs
maxWRs <- numWRs + numWRTEs + numWRRBs + numWRRBTEs + numQBWRRBTEs
minTEs <- numTEs
maxTEs <- numTEs + numWRTEs + numWRRBTEs + numQBWRRBTEs

numStarters <- numQBs + numRBs + numWRs + numTEs + numWRRBs + numWRTEs + numWRRBTEs + numQBWRRBTEs
maxAvailable <- leagueCap - (numTotalPlayers - numStarters)

passYdsMultiplier <- 1/passYdsMultiplier
rushYdsMultiplier <- 1/rushYdsMultiplier
recYdsMultiplier <- 1/recYdsMultiplier

shinyData$passYdsPts <- shinyData$passYds*passYdsMultiplier
shinyData$passTdsPts <- shinyData$passTds*passTdsMultiplier
shinyData$passIntPts <- shinyData$passInt*passIntMultiplier
shinyData$rushYdsPts <- shinyData$rushYds*rushYdsMultiplier
shinyData$rushTdsPts <- shinyData$rushTds*rushTdsMultiplier
shinyData$recPts <- shinyData$rec*recMultiplier
shinyData$recYdsPts <- shinyData$recYds*recYdsMultiplier
shinyData$recTdsPts <- shinyData$recTds*recTdsMultiplier
#shinyData$twoPtsPts <- shinyData$twoPts*twoPtsMultiplier
shinyData$fumblesPts <- shinyData$fumbles*fumbleMulitplier

shinyData$projectedPts <- rowSums(shinyData[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","fumblesPts")], na.rm=T) #,"twoPtsPts"

#merge(projections[,c("name","projections")], shinyData[,c("name","projectedPts")], by="name", all=TRUE)

num.players <- length(shinyData$name)
var.types <- rep("B", num.players)

A <- rbind(as.numeric(shinyData$pos == "QB"),
           as.numeric(shinyData$pos == "QB"),
           as.numeric(shinyData$pos == "RB"),
           as.numeric(shinyData$pos == "RB"),
           as.numeric(shinyData$pos == "WR"),
           as.numeric(shinyData$pos == "WR"),
           as.numeric(shinyData$pos == "TE"),
           as.numeric(shinyData$pos == "TE"),
           diag(shinyData$risk),                 # player's risk
           shinyData$projectedCost,              # total cost
           rep(1,num.players))                   # num of players in starting lineup     

dir <- c(">=",
         "<=",
         ">=",
         "<=",
         ">=",
         "<=",
         ">=",
         "<=",
         rep("<=", num.players),
         "<=",
         "==")

b <- c(minQBs,
       maxQBs,
       minRBs,
       maxRBs,
       minWRs,
       maxWRs,
       minTEs,
       maxTEs,
       rep(maxRisk, num.players),
       maxAvailable,
       numStarters)

sol <- Rglpk_solve_LP(obj = shinyData$projectedPts, mat = A, dir = dir, rhs = b,types = var.types, max = TRUE)
sol$playerInfo <- as.data.frame(merge(shinyData[shinyData$name %in% shinyData[sol$solution == 1,"name"],c("name","pos","team")], shinyData[sol$solution == 1,c("name","projectedPts","risk","projectedCost")], by="name"))
sol$playerInfo[,"projectedCost"] <- as.integer(sol$playerInfo[,"projectedCost"])
sol$totalCost <- sum(shinyData$projectedCost * sol$solution)
#sol$totalCost <- sum(sol$playerInfo$cost)
sol$players <- as.character(shinyData$name[sol$solution == 1])

sol
shinyData[,c("name","projectedPts","projectedCost")]
