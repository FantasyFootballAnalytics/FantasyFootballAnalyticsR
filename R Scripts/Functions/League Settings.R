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
twoPtsMultiplier <- 2       #2 pts per 2-point conversion
fumlMultiplier <- -3        #-3 pts per fumble lost

#Projections
sourcesOfProjections <- c("CBS1", "CBS2", "ESPN", "FantasyPros", "FantasySharks", "FFtoday", "NFL", "Yahoo") #c("Accuscore", "CBS1", "CBS2", "ESPN", "FantasyPros", "FantasySharks", "FFtoday", "FOX", "NFL", "Yahoo")
sourcesOfProjectionsAbbreviation <- c("cbs1", "cbs2", "espn", "fp", "fs", "fftoday", "nfl", "yahoo") #c("accu", "cbs1", "cbs2", "espn", "fp", "fs", "fftoday", "fox", "nfl", "yahoo")

#Number of players at each position drafted in Top 100 (adjust for your league)
qbReplacements <- 15
rbReplacements <- 37
wrReplacements <- 36
teReplacements <- 11

#Alternative way of calculating the number of players at each position drafted in Top 100 based on league settings
#numTeams <- 10  #number of teams in league
#numQB <- 1      #number of avg QBs in starting lineup
#numRB <- 2.5    #number of avg RBs in starting lineup
#numWR <- 2.5    #number of avg WRs in starting lineup
#numTE <- 1      #number of avg TEs in starting lineup

#qbReplacements <- print(ceiling(numQB*numTeams*1.7))
#rbReplacements <- print(ceiling(numRB*numTeams*1.4))
#wrReplacements <- print(ceiling(numWR*numTeams*1.4))
#teReplacements <- print(ceiling(numTE*numTeams*1.3))