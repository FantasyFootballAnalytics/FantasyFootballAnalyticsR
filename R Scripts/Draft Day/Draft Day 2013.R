###########################
# File: Draft Day.R
# Description: Continually recalculates optimal team given which players are available
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Specify Maximum Risk
maxRisk <- 4.3

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/Historical Files/BidUpToSimulation-2013.RData", sep=""))
load(paste(getwd(),"/Data/Historical Files/IDP-2013.RData", sep=""))
load(paste(getwd(),"/Data/Historical Files/kickers-2013.RData", sep=""))

#Subset data
draftData <- projections[,c("name","pos","team","projections","vor","simulation","sdPick","sdPts","risk","avgCost","inflatedCost","bidUpTo","bidUpToSim")] #projectedPtsLatent
draftData <- draftData[order(-draftData$vor),]
row.names(draftData) <- 1:dim(draftData)[1]

#Save data
save(draftData, file = paste(getwd(),"/Data/Historical Files/DraftDay-2013.RData", sep=""))
write.csv(draftData, file=paste(getwd(),"/Data/Historical Files/DraftDay-2013.csv", sep=""), row.names=FALSE)

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

optimizeDraft(maxRisk=4.3)
optimizeDraft(maxRisk=4.3, omit=c("Adrian Peterson","Eric Decker"))
optimizeDraft(maxRisk=4.3, omit=drafted)

draftData[!(draftData$name %in% drafted),]

###################
### Draft Dashboard
###################

###--UPDATE--###
myteam <- data.frame(
  player = c("Adrian Peterson","Aaron Rodgers","Brandon Marshall",
             "DeMarco Murray","Lamar Miller","Antonio Gates",
             "Mike Williams"), #,"Darren Sproles","Jason Witten"
  position = c("RB","QB","WR","RB","RB","TE","WR"), #,"RB","TE"
  cost = c(70,55,40,15,21,2,6) #,5,10
)
myteam$player <- as.character(myteam$player)

drafted <- c(myteam$player,
             "Russell Wilson","Arian Foster","Doug Martin","LeSean McCoy","Dez Bryant","J.J. Watt",
             "Marshawn Lynch","Drew Brees","Larry Fitzgerald","C.J. Spiller","Calvin Johnson",
             "Cam Newton","Chris Johnson","Jimmy Graham","Matt Forte","Cameron Wake","Steven Jackson",
             "Jamaal Charles","Jordy Nelson","Ray Rice","Victor Cruz","Rob Gronkowski",
             "Maurice Jones-Drew","Julius Peppers","Andre Johnson","Morgan Burnett","Trent Richardson",
             "Luke Kuechly","Brandon Marshall","Alfred Morris","Tom Brady","Darren McFadden",
             "Stephen Gostkowski","Reggie Bush","Frank Gore","Eric Berry","Justin Houston",
             "Lavonte David","A.J. Green","Stevan Ridley","Colin Kaepernick","Peyton Manning",
             "David Wilson","Jacquizz Rodgers","Le'Veon Bell","Eric Weddle","Blair Walsh","Aldon Smith",
             "Demaryius Thomas","Paul Posluszny","Randall Cobb","Matt Ryan","Matt Bryant","Hakeem Nicks",
             "Vincent Jackson","Tyvon Branch","Wes Welker","Janoris Jenkins","Dan Bailey","Vernon Davis",
             "Julio Jones","Robert Griffin III","Patrick Peterson","James Jones","Marques Colston",
             "James Laurinaitis","Montee Ball","Geno Atkins","DeMarco Murray","Tony Gonzalez",
             "Dwayne Bowe","Matthew Stafford","Ryan Mathews","Danny Amendola","Mike Wallace",
             "NaVorro Bowman","Richard Sherman","Roddy White","Jason Witten","Darren Sproles",
             "Reggie Wayne","Eric Decker","Eddie Lacy","Miles Austin","Derrick Johnson","Pierre Garcon",
             "Andrew Luck","Randy Bullock","Greg Olsen","Bryce Brown","Bobby Wagner","Fred Jackson",
             "Matt Prater","Rashard Mendenhall","Sean Lee","DeSean Jackson","Jerod Mayo","Curtis Lofton",
             "Jermichael Finley","Sidney Rice","D'Qwell Jackson","Lamar Miller","Jason Pierre-Paul",
             "Jonathon Stewart","Patrick Willis","Eli Manning","Justin Tucker","William Moore",
             "Giovani Bernard","Ahmad Bradshaw","Daryl Richardson","Dashon Goldson","Mark Ingram",
             "Chad Greenway","Greg Jennings","Stevie Johnson","Charles Johnson","Phil Dawson",
             "Mario Williams","Lawrence Timmons","Owen Daniels","Kenbrell Thompkins",
             "BenJarvus Green-Ellis","London Fletcher","Ben Roethlisberger","Jared Allen","Lance Moore",
             "Tavon Austin","Chris Ivory","Daryl Washington","Shane Vereen","Michael Vick",
             "Brian Cushing","Antonio Gates","Steve Smith","Torrey Smith","Cecil Shorts","Jared Cook",
             "Aaron Dobson","TY Hilton","T.Y. Hilton","Antonio Brown","Steve Johnson","Martellus Bennett",
             "Anquan Boldin","DeAngelo Williams","Josh Gordon","Mike Williams")
###----------###

### Optimize Team ###
# Projected Points
optimizeDraft(maxRisk=5.0, omit=drafted)
optimizeDraft(maxRisk=7.0, omit=drafted)
optimizeDraft(maxRisk=4.1, omit=drafted) #From Optimum Risk.R #1554
optimizeDraft(maxRisk=3.3, omit=drafted) #From Simulation.R   #1532
optimizeDraft(maxRisk=100, omit=drafted)                      #1568

# Simulated Points
optimizeDraft(maxRisk=5.0, omit=drafted, points=removedPlayers$simulation) #From Optimum Risk.R
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=5.0, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1522

optimizeDraft(maxRisk=4.1, omit=drafted, points=removedPlayers$simulation) #From Optimum Risk.R
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=4.1, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1494

optimizeDraft(maxRisk=3.3, omit=drafted, points=removedPlayers$simulation) #From Simulation.R
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=3.3, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1514

optimizeDraft(maxRisk=100, omit=drafted, points=removedPlayers$simulation)
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=100, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1563

### Remaining Players ###
#Player Info
draftData[draftData$name == "Adrian Peterson",]

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

### Kickers ###
kickers[!(kickers$name %in% drafted),]
