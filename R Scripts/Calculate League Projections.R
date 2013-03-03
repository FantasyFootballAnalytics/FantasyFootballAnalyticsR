###########################
# File: Calculate League Projections.R
# Description: Calculates league projections based on league settings
# Date: 3/2/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These projections are from last year (ESPN has not yet updated them for the upcoming season)
# -ESPN projections do not include fumbles!
###########################

#Customize your league settings
passYdsMultiplier <- (1/25) #1 pt per 25 pass yds
passTdsMultiplier <- 4      #4 pts per pass td
passIntMultiplier <- -3     #-3 pts per INT
rushYdsMultiplier <- (1/10) #1 pt per 10 rush yds
rushTdsMultiplier <- 6      #6 pts per rush td
recYdsMultiplier <- (1/8)   #1 pt per 8 rec yds
recTdsMultiplier <- 6       #6 pts per rec td
fumlMultiplier <- -3        #-3 pts per fumble (not included in ESPN projections, however)

#Load data
load(paste(getwd(),"/Data/ESPN-Projections-2012.RData", sep=""))

#Calculate projected points for your league
projections$projectedPts <- (projections$passYds*passYdsMultiplier) + (projections$passTds*passTdsMultiplier) + (projections$passInt*passIntMultiplier) + (projections$rushYds*rushYdsMultiplier) + (projections$rushTds*rushTdsMultiplier) + (projections$recYds*recYdsMultiplier) + (projections$recTds*recTdsMultiplier)

#Save file
save(projections, file = paste(getwd(),"/Data/ESPN-LeagueProjections-2012.RData", sep=""))
