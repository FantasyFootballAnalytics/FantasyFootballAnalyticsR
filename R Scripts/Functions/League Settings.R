###########################
# File: League Settings.R
# Description: User sets league settings
# Date: 6/1/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Season
season <- 2016
weekNo <- 0   # Set weekNo = 0 for seasonal projections

#Roster
numQBstarters <- 1
numRBstarters <- 2
numWRstarters <- 3
numTEstarters <- 0
numTotalStarters <- 8
numTotalPlayers <- 14

#League settings
defaultCap <- 0          #what the typical cap is for your service (ESPN, Yahoo, etc.) -- used for placing "avg cost" in context
leagueCap <- 0            #your league's cap
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

#Variable names
prefix <- c("name","pos","sourceName")
sourceSpecific <- c("name","team")
scoreCategories <- c("passAtt","passComp","passIncomp","passYds","passTds","passInt",
                     "rushAtt","rushYds","rushTds",
                     "rec","recTgt","recYds","recTds",
                     "returnTds","twoPts","fumbles",
                     "idpSolo","idpAst","idpSack","idpFumlRec","idpFumlForce","idpInt","idpPD",
                     "dstPtsAllow","dstYdsAllowed","dstSack","dstSafety","dstInt","dstFumlRec","dstFumlForce","dstBlk","dstTd",
                     "fg","fgAtt","fg0019","fg2029","fg3039","fg4049","fg50","xp")
calculatedVars <- c("positionRank","overallRank","points","pointsLo","pointsHi","vor","pick","risk","sdPts","sdPick")
varNames <- c(calculatedVars, scoreCategories)
finalVarNames <- c("name","pos","team","sourceName","player","playerID","season", "playerId", "analystId", varNames)

#Scoring
passAttMultiplier <- 0      #0 pts per passing attempt
passCompMultiplier <- 0     #0 pts per passing completion
passIncompMultiplier <- 0   #0 pts per passing incompletion
passYdsMultiplier <- (1/25) #1 pt per 25 passing yds
passTdsMultiplier <- 4      #4 pts per passing td
passIntMultiplier <- -2     #-3 pts per passing interception
rushAttMultiplier <- 0      #0 pts per rushing attempt
rushYdsMultiplier <- (1/10) #1 pt per 10 rushing yds
rushTdsMultiplier <- 6      #6 pts per rushing touchdown
recMultiplier <- 0          #0 pts per reception
recYdsMultiplier <- (1/10)   #1 pt per 8 receiving yds
recTdsMultiplier <- 6       #6 pts per receiving touchdown
returnTdsMultiplier <- 6    #6 pts per return touchdown
twoPtsMultiplier <- 2       #2 pts per 2-point conversion
fumlMultiplier <- -2        #-3 pts per fumble lost

scoringRules <- list(
  QB = data.table::data.table(dataCol = c("passYds", "passTds", "passInt", "rushYds", "rushTds", "twoPts", "fumbles"),
                  multiplier = c(1/25, 4, -2, 1/10, 6, 2, -2 )),
  RB = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                  multiplier = c(1/10, 6, 0, 1/10, 6, 6, 2, -2)),
  WR = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                  multiplier = c(1/10, 6, 0, 1/10, 6, 6, 2, -2)),
  TE = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                  multiplier = c(1/10, 6, 0, 1/10, 6, 6, 2, -2)),
  K = data.table::data.table(dataCol = c("xp", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"),
                 multiplier = c(1,  3, 3, 3, 3, 5)),
  DST = data.table::data.table(dataCol = c("dstFumlRec", "dstInt", "dstSafety", "dstSack", "dstTd", "dstBlk"),
                   multiplier = c(2, 2, 2, 1, 6, 2)),
  ptsBracket = data.table::data.table(threshold = c(0, 6, 13, 20, 27, 34, 99),
                          points = c(10, 8, 6, 4, 0, -2, -4))
)


#Projections
#c("CBS", "ESPN", "Yahoo") #c("Accuscore", "CBS1", "CBS2", "EDSfootball", "ESPN", "FantasyFootballNerd", "FantasyPros", "FantasySharks", "FFtoday", "Footballguys1", "Footballguys2", "Footballguys3", "Footballguys4", "FOX", "NFL", "numberFire", "WalterFootball", "Yahoo")
sourcesOfProjections <- c("Jamey Eisenberg", "Dave Richard", #"Yahoo Sports" , 
                          "ESPN", "NFL", "FOX Sports", "FFToday", "FFToday - IDP",
                          "NumberFire", "FantasyPros") #, "Dodds-Norton", "Dodds", "Tremblay", "Herman", "Henry", "Wood", "Bloom") 
sourcesOfProjectionsAbbreviation <- c("cbs", "espn", "yahoo") #c("accu", "cbs1", "cbs2", "eds", "espn", "ffn", "fp", "fs", "fftoday", "fbg1", "fbg2", "fbg3", "fbg4", "fox", "nfl", "nf", "wf", "yahoo")

#Weights applied to each source in calculation of weighted average of projections
weight_accu <- 1    #Accuscore
weight_cbs1 <- 1    #Jamey Eisenberg
weight_cbs2 <- 1    #Dave Richard"
weight_eds <- 1     #EDS Football
weight_espn <- 1    #ESPN
weight_ffn <- 1     #Fantasy Football Nerd
weight_fbg1 <- 1    #Footballguys: David Dodds
weight_fbg2 <- 1    #Footballguys: Bob Henry
weight_fbg3 <- 1    #Footballguys: Maurile Tremblay
weight_fbg4 <- 1    #Footballguys: Jason Wood
weight_fox <- 1    #FOX
weight_fp <- 1      #FantasyPros
weight_fs <- 1      #FantasySharks
weight_fftoday <- 1 #FFtoday
weight_nfl <- 1     #NFL.com
weight_nf <- 1      #numberFire
weight_wf <- 1      #WalterFootball
weight_yahoo <- 1   #Yahoo 

sourceWeights <- c(
  "Jamey Eisenberg"   = 1, 
  "Dave Richard"      = 1, 
  "Yahoo Sports"      = 1, 
  "ESPN"              = 1, 
  "NFL"               = 1, 
  "FOX Sports"        = 1, 
  "FFtoday"           = 1,
  "NumberFire"        = 1, 
  "FantasyPros"       = 1,
  "Dodds-Norton"      = 1, 
  "Dodds"             = 1, 
  "Tremblay"          = 1, 
  "Herman"            = 1, 
  "Henry"             = 1, 
  "Wood"              = 1, 
  "Bloom"             = 1
  ) 


#Number of players at each position drafted in Top 100 (adjust for your league)
qbReplacements <- 19
rbReplacements <- 42
wrReplacements <- 50
teReplacements <- 12

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
