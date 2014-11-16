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

#Variable names
prefix <- c("name","pos")
sourceSpecific <- c("name","team")
scoreCategories <- c("points",
                     "passAtt","passComp","passYds","passTds","passInt",
                     "rushAtt","rushYds","rushTds","rec","recYds","recTds",
                     "returnTds","twoPts","fumbles",
                     "idpSolo","idpAst","idpSack","idpFumlRec","idpFumlForce","idpInt","idpPD",
                     "dstPtsAllowed","dstSack","dstSafety","dstInt","dstFumlRec","dstBlk","dstTo","dstIntTd","kRetTd","pRetTd",
                     "fg","fg019","fg2029","fg3039","fg4049","fg50","xp")
rankVars <- c("positionRank","overallRank")
varNames <- c(rankVars, scoreCategories)

#Scoring
passAttMultiplier <- 0      #0 pts per passing attempt
passCompMultiplier <- 0     #0 pts per passing completion
passIncompMultiplier <- 0     #0 pts per passing incompletion
passYdsMultiplier <- (1/25) #1 pt per 25 passing yds
passTdsMultiplier <- 4      #4 pts per passing td
passIntMultiplier <- -3     #-3 pts per passing interception
rushAttMultiplier <- 0      #0 pts per rushing attempt
rushYdsMultiplier <- (1/10) #1 pt per 10 rushing yds
rushTdsMultiplier <- 6      #6 pts per rushing touchdown
recMultiplier <- 0          #0 pts per reception
recYdsMultiplier <- (1/8)   #1 pt per 8 receiving yds
recTdsMultiplier <- 6       #6 pts per receiving touchdown
returnTdsMultiplier <- 6    #6 pts per return touchdown
twoPtsMultiplier <- 2       #2 pts per 2-point conversion
fumlMultiplier <- -3        #-3 pts per fumble lost

#Projections
sourcesOfProjections <- c("Accuscore", "CBS1", "CBS2", "EDSfootball", "ESPN", "FantasyFootballNerd", "FantasyPros", "FantasySharks", "FFtoday", "Footballguys1", "Footballguys2", "Footballguys3", "Footballguys4", "FOX", "NFL", "numberFire", "WalterFootball", "Yahoo")
sourcesOfProjectionsAbbreviation <- c("accu", "cbs1", "cbs2", "eds", "espn", "ffn", "fp", "fs", "fftoday", "fbg1", "fbg2", "fbg3", "fbg4", "fox", "nfl", "nf", "wf", "yahoo")

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