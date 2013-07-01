###########################
# File: Draft Day.R
# Description: Continually recalculates optimal team given which players are available
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These calculations are from last year (they have not yet been updated for the upcoming season)
###########################

#Specify Maximum Risk
maxRisk <- 4.6

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/BidUpToSimulation-2013.RData", sep=""))
load(paste(getwd(),"/Data/IDP-2013.RData", sep=""))
load(paste(getwd(),"/Data/kickers-2013.RData", sep=""))

#Subset data
draftData <- projections[,c("name","pos","team","projections","vor","simulation","sdPick","sdPts","risk","avgCost","inflatedCost","bidUpTo","bidUpToSim")] #projectedPtsLatent
draftData <- draftData[order(-draftData$vor),]
row.names(draftData) <- 1:dim(draftData)[1]

#Save data
save(draftData, file = paste(getwd(),"/Data/DraftDay-2013.RData", sep=""))
write.csv(draftData, file=paste(getwd(),"/Data/CSV/DraftDay-2013.csv", sep=""), row.names=FALSE)

options(digits=2)
draftData

#Day of Draft
removedPlayers <-  draftData[row.names(na.omit(draftData[,c("projections","simulation","risk","inflatedCost")])),] #projectedPtsLatent
row.names(removedPlayers) <- 1:dim(removedPlayers)[1]
removedPlayers

### RUN TO HERE ###

#Example: Update with drafted (i.e., unavailable) players
myteam <- data.frame(
  player = c("Arian Foster", "Tom Brady", "Jacob Tamme"),
  pos = c("RB", "QB", "TE"),
  cost = c(64, 46, 5)
)
myteam$player <- as.character(myteam$player)

drafted <- c(myteam$player,"Vincent Jackson","Eric Decker")

optimizeDraft(maxRisk=4.6)
optimizeDraft(maxRisk=4.6, omit=c("Vincent Jackson","Eric Decker"))
optimizeDraft(maxRisk=4.6, omit=drafted)

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

drafted <- c(myteam$player,"")
###----------###

### Optimize Team ###
# Projected Points
optimizeDraft(maxRisk=3.5, omit=drafted) #From Optimum Risk.R
optimizeDraft(maxRisk=4.4, omit=drafted) #From Simulation.R
optimizeDraft(maxRisk=100, omit=drafted)

# Simulated Points
optimizeDraft(maxRisk=3.5, omit=drafted, points=removedPlayers$simulation) #From Optimum Risk.R
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=3.5, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1528

optimizeDraft(maxRisk=4.4, omit=drafted, points=removedPlayers$simulation) #From Simulation.R
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=4.4, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1540

optimizeDraft(maxRisk=100, omit=drafted, points=removedPlayers$simulation)
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=100, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1536

### Remaining Players ###
#All
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections),]

#QB
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections) & draftData$pos=="QB",]

#RB
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections) & draftData$pos=="RB",]

#WR
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections) & draftData$pos=="WR",]

#TE
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections) & draftData$pos=="TE",]

### Starters ###
#All
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections),]

#QB
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections) & draftData$pos=="QB",]

#RB
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections) & draftData$pos=="RB",]

#WR
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections) & draftData$pos=="WR",]

#TE
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections) & draftData$pos=="TE",]

### Sleepers ###
#All
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections),]

#QB
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections) & draftData$pos=="QB",]

#RB
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections) & draftData$pos=="RB",]

#WR
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections) & draftData$pos=="WR",]

#TE
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections) & draftData$pos=="TE",]

### Kickers ###
kickers[!(kickers$name %in% drafted),]

### Defensive Players ###

#D
IDP[!(IDP$name %in% drafted),]

#DL
IDP[!(IDP$name %in% drafted) & (IDP$pos=="DE" | IDP$pos=="DT"),]

#DB
IDP[!(IDP$name %in% drafted) & (IDP$pos=="S" | IDP$pos=="CB"),]

#DB
IDP[!(IDP$name %in% drafted) & IDP$pos=="LB",]
