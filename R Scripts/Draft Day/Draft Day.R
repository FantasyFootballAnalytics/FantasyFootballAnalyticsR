###########################
# File: Draft Day.R
# Description: Continually recalculates optimal team given which players are available
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# -These calculations are from last year (they have not yet been updated for the upcoming season)
###########################

#Specify Maximum Risk
maxRisk <- 4.3

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/BidUpToSimulation.RData", sep=""))
load(paste(getwd(),"/Data/IDP.RData", sep=""))
load(paste(getwd(),"/Data/kickers.RData", sep=""))

#Subset data
draftData <- projections[sourceName == "averageRobust", c("name","player","pos","team","points","vor","simulation","sdPick","sdPts","risk","avgCost","inflatedCost","bidUpTo","bidUpToSim"), with=FALSE]

#Save data
save(draftData, file = paste(getwd(), "/Data/DraftDay.RData", sep=""))
write.csv(draftData, file=paste(getwd(), "/Data/DraftDay.csv", sep=""), row.names=FALSE)

save(draftData, file = paste(getwd(), "/Data/Historical Files/DraftDay-", season, ".RData", sep=""))
write.csv(draftData, file=paste(getwd(), "/Data/Historical Files/DraftDay-", season, ".csv", sep=""), row.names=FALSE)

options(digits=2)
draftData

#Day of Draft
removedPlayers <-  draftData[rowSums(is.na(draftData[,c("points","simulation","risk","inflatedCost"), with=FALSE])) == 0,]

### RUN TO HERE ###

#Example: Update with drafted (i.e., unavailable) players
myteam <- data.frame(
  player = c("DeMarco Murray", "Jordan Reed", "Julio Jones", "Michael Floyd", "Jamaal Charles"),
  pos = c("RB", "TE", "WR", "WR", "RB"),
  cost = c(42, 5, 47, 11, 74)
)
myteam$player <- as.character(myteam$player)

drafted <- c(myteam$player,"Peyton Manning","Colin Kaepernick")

optimizeDraft(maxRisk=4.3)
optimizeDraft(maxRisk=4.3, omit=c("Adrian Peterson","Eric Decker"))
optimizeDraft(maxRisk=4.3, omit=drafted)

draftData[!(draftData$name %in% drafted),]

###################
### Draft Dashboard
###################

###--UPDATE--###
myteam <- data.frame(
  player = c(),
  position = c(),
  cost = c()
  )
myteam$player <- as.character(myteam$player)

drafted <- c(myteam$player)
###----------###

### Optimize Team ###
# Projected Points
optimizeDraft(maxRisk=5.0, omit=drafted)
optimizeDraft(maxRisk=4.1, omit=drafted) #From Optimum Risk.R #1554
optimizeDraft(maxRisk=3.3, omit=drafted) #From Simulation.R   #1532
optimizeDraft(maxRisk=100, omit=drafted)                      #1568

# Simulated Points
optimizeDraft(maxRisk=5.0, omit=drafted, points=removedPlayers$simulation) #From Optimum Risk.R
sum(draftData[player %in% optimizeDraft(maxRisk=5.0, omit=drafted, points=removedPlayers$simulation)$players, "points", with=FALSE]) #1389

optimizeDraft(maxRisk=4.1, omit=drafted, points=removedPlayers$simulation) #From Optimum Risk.R
sum(draftData[player %in% optimizeDraft(maxRisk=4.1, omit=drafted, points=removedPlayers$simulation)$players, "points", with=FALSE]) #1374

optimizeDraft(maxRisk=3.3, omit=drafted, points=removedPlayers$simulation) #From Simulation.R
sum(draftData[player %in% optimizeDraft(maxRisk=3.3, omit=drafted, points=removedPlayers$simulation)$players, "points", with=FALSE]) #1178

optimizeDraft(maxRisk=100, omit=drafted, points=removedPlayers$simulation)
sum(draftData[player %in% optimizeDraft(maxRisk=100, omit=drafted, points=removedPlayers$simulation)$players, "points", with=FALSE]) #1330

### Remaining Players ###
#Player Info
draftData[draftData$player == "Adrian Peterson",]

#All
draftData[!(player %in% drafted) & !is.na(points),]

#QB
draftData[!(player %in% drafted) & !is.na(points) & draftData$pos == "QB",]

#RB
draftData[!(player %in% drafted) & !is.na(points) & draftData$pos == "RB",]

#WR
draftData[!(player %in% drafted) & !is.na(points) & draftData$pos == "WR",]

#TE
draftData[!(player %in% drafted) & !is.na(points) & draftData$pos == "TE",]

### Starters ###
#All
draftData[!(player %in% drafted) & vor > 0 & risk < 5 & !is.na(points),]

#QB
draftData[!(player %in% drafted) & vor > 0 & risk < 5 & !is.na(points) & draftData$pos == "QB",]

#RB
draftData[!(player %in% drafted) & vor > 0 & risk < 5 & !is.na(points) & draftData$pos == "RB",]

#WR
draftData[!(player %in% drafted) & vor > 0 & risk < 5 & !is.na(points) & draftData$pos == "WR",]

#TE
draftData[!(player %in% drafted) & vor > 0 & risk < 5 & !is.na(points) & draftData$pos == "TE",]

### Sleepers ###
#All
draftData[!(player %in% drafted) & risk >= 6 & !is.na(points),]

#QB
draftData[!(player %in% drafted) & risk >= 6 & !is.na(points) & draftData$pos == "QB",]

#RB
draftData[!(player %in% drafted) & risk >= 6 & !is.na(points) & draftData$pos == "RB",]

#WR
draftData[!(player %in% drafted) & risk >= 6 & !is.na(points) & draftData$pos == "WR",]

#TE
draftData[!(player %in% drafted) & risk >= 6 & !is.na(points) & draftData$pos == "TE",]

### Kickers ###
kickers[!(player %in% drafted),]

### Defensive Players ###

#D
IDP[!(player %in% drafted),]

#DL
IDP[!(player %in% drafted) & (pos=="DE" | pos=="DT"),]

#DB
IDP[!(player %in% drafted) & (pos=="S" | pos=="CB"),]

#DB
IDP[!(player %in% drafted) & pos=="LB",]
