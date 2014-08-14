#Run fantasy football analysis files in order

###DON'T FORGET TO MANUALLY DOWNLOAD AVG COST FROM YAHOO

###############
# Functions
###############

source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

###############
# Rankings
###############

source(paste(getwd(),"/R Scripts/Rankings/IDP.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Rankings/Kickers.R", sep=""), echo=TRUE)

###############
# Projections
###############

source(paste(getwd(),"/R Scripts/Projections/Accuscore Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/CBS1 Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/CBS2 Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/ESPN Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/FantasyFootballNerd Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/FantasyPros Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/FantasySharks Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/FFtoday Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/Footballguys1 Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/Footballguys2 Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/Footballguys3 Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/Footballguys4 Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/FOX Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/NFL Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/WalterFootball Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/Yahoo Projections.R", sep=""), echo=TRUE)

###############
# Calculations
###############

source(paste(getwd(),"/R Scripts/Calculations/Calculate League Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Wisdom of the Crowd.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Risk.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Value Over Replacement.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Avg Cost.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Optimum Roster.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Optimum Risk.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Bid Up To.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Simulation.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Bid Up To Simulation.R", sep=""), echo=TRUE)

###############
# Draft Day
###############

source(paste(getwd(),"/R Scripts/Draft Day/Draft Day.R", sep=""), echo=TRUE)

###############
# Other
###############

source(paste(getwd(),"/R Scripts/Posts/Evaluate Projections.R", sep=""), echo=TRUE)
