###########################
# File: League Settings.R
# Description: User sets league settings
# Date: 6/1/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Roster
numQBstarters <- 1
numRBstarters <- 2
numWRstarters <- 2
numTEstarters <- 1
numTotalStarters <- 7
numTotalPlayers <- 20

#League settings
defaultCap <- 200           #what the typical cap is for your service (ESPN, Yahoo, etc.) -- used for placing "avg cost" in context
leagueCap <- 225            #your league's cap
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

#Scoring
passYdsMultiplier <- (1/25) #1 pt per 25 pass yds
passTdsMultiplier <- 4      #4 pts per pass td
passIntMultiplier <- -3     #-3 pts per INT
rushYdsMultiplier <- (1/10) #1 pt per 10 rush yds
rushTdsMultiplier <- 6      #6 pts per rush td
recMultiplier <- 0          #0 pts per rec
recYdsMultiplier <- (1/8)   #1 pt per 8 rec yds
recTdsMultiplier <- 6       #6 pts per rec td
twoPtsMultiplier <- 2       #2 pts per 2-point conversion (not included in ESPN or CBS projections)
fumlMultiplier <- -3        #-3 pts per fumble lost (not included in ESPN projections)