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
draftData <- projections[,c("name","pos","team","projections","vor","sdPick","sdPts","risk","avgCost","inflatedCost","bidUpTo","bidUpToSim")] #projectedPtsLatent

#Save data
save(draftData, file = paste(getwd(),"/Data/DraftDay-2013.RData", sep=""))
write.csv(draftData, file=paste(getwd(),"/Data/CSV/DraftDay-2013.csv", sep=""), row.names=FALSE)

options(digits=2)
draftData

#Day of Draft
removedPlayers <-  draftData[row.names(na.omit(draftData[,c("projections","risk","inflatedCost")])),] #projectedPtsLatent
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
optimizeDraft(maxRisk=4.6, omit=drafted)
optimizeDraft(maxRisk=100, omit=drafted)

### Remaining Players ###
#All
draftData[!(draftData$name %in% drafted),]

#QB
draftData[!(draftData$name %in% drafted) & draftData$pos=="QB",]

#RB
draftData[!(draftData$name %in% drafted) & draftData$pos=="RB",]

#WR
draftData[!(draftData$name %in% drafted) & draftData$pos=="WR",]

#TE
draftData[!(draftData$name %in% drafted) & draftData$pos=="TE",]

### Starters ###
#All
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5,]

#QB
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & draftData$pos=="QB",]

#RB
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & draftData$pos=="RB",]

#WR
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & draftData$pos=="WR",]

#TE
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & draftData$pos=="TE",]

### Sleepers ###
#All
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)),]

#QB
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & draftData$pos=="QB",]

#RB
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & draftData$pos=="RB",]

#WR
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & draftData$pos=="WR",]

#TE
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & draftData$pos=="TE",]

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
