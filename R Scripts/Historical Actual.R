###########################
# File: Historical Actual.R
# Description: Scrapes Historical Performance of Players
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
###########################

#Libraries
load_or_install(c("XML","stringr","plyr"))

#Data
years <- 1999:2012

#Typical replacement/baseline player for VORP
qbReplacements <- 15
rbReplacements <- 36
wrReplacements <- 38
teReplacements <- 8
kReplacements <- 1
defReplacements <- 2

#Scoring
passYdsMultiplier <- (1/25) #1 pt per 25 pass yds
passTdsMultiplier <- 4      #4 pts per pass td
passIntMultiplier <- -2     #-2 pts per INT
rushYdsMultiplier <- (1/10) #1 pt per 10 rush yds
rushTdsMultiplier <- 6      #6 pts per rush td
recYdsMultiplier <- (1/10)  #1 pt per 10 rec yds
recTdsMultiplier <- 6       #6 pts per rec td
twoPtsMultiplier <- 2       #2 pts per 2-point conversion
fumlMultiplier <- -2        #-3 pts per fumble lost

fg30Multiplier <- 3         #3 pts per fg from 0-39 yds
fg40Multiplier <- 4         #4 pts per fg from 40-49 yds
fg50Multiplier <- 5         #5 pts per fg from 50+ yds
fgMissedMultiplier <- -1    #-1 pt per missed fg
patMultiplier <- 1          #1 pt per pat
patMissedMultiplier <- -1   #-1 pt per missed pat

fumlRecoveryMultiplier <- 2 #2 pts per fumble recovery
intCaughtMultiplier <- 2    #2 pts per int caught
fgBlockedMultiplier <- 2    #2 pts per fumble blocked
sackMultiplier <- 1         #1 pts per sack
safetyMultiplier <- 2       #2 pts per safety
pa0Multiplier <- 10         #10 pts per 0 points allowed
pa6Multiplier <- 7          #7 pts per 1-6 points allowed
pa20Multiplier <- 4         #4 pts per 7-20 points allowed
pa34Multiplier <- 0         #0 pts per 21-34 points allowed
pa35Multiplier <- -4        #-4 pts per 35+ points allowed
tdMultiplier <- 6           #6 pts per td

#Functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Loop to import, process, merge, and save historical actual data
actualList <- list()

for(i in 1:length(years)){
  actual <- readHTMLTable(paste("http://www.pro-football-reference.com/years/", years[i], "/fantasy.htm", sep=""), stringsAsFactors = FALSE)$fantasy
  
  names(actual) <- c("overall_rank", "name_info", "team", "age", "games", "games_start", "pass_comp", "pass_att", "pass_yds", "pass_td", "pass_int", "rush_att", 
                       "rush_yds", "rush_yds_att", "rush_td", "receptions", "rec_yds", "yds_rec", "rec_td", "pos", "fant_points", "vbd", "pos_rank", "ovrank")
  
  #Remove headings within data set
  actual <- actual[which(actual$overall_rank!="Rk"), ]
  actual <- actual[which(actual$name_info!="Passing"), ]
  
  #Clean-up name field
  actual$name <- str_replace_all(actual$name_info, "[[:punct:]]", "")
  actual$name <- toupper(actual$name)
  
  #Rename players
  ifelse(length(which(actual$name == "Chris Wells")) > 0, actual$name[actual$name == "Chris Wells"] <- "Beanie Wells", doNothing <- 0)
  ifelse(length(which(actual$name == "Chad Johnson")) > 0, actual$name[actual$name == "Chad Johnson"] <- "Chad Ochocinco", doNothing <- 0)
  ifelse(length(which(actual$name == "Steve Johnson")) > 0, actual$name[actual$name == "Steve Johnson"] <- "Stevie Johnson", doNothing <- 0)
  
  #Convert variables from character strings to numeric
  actual$overall_rank <- as.numeric(actual$overall_rank)
  actual$age <- as.numeric(actual$age)
  actual$games <- as.numeric(actual$games)
  actual$games_start <- as.numeric(actual$games_start)
  actual$pass_comp <- as.numeric(actual$pass_comp)
  actual$pass_att <- as.numeric(actual$pass_att)
  actual$pass_yds <- as.numeric(actual$pass_yds)
  actual$pass_td <- as.numeric(actual$pass_td)
  actual$pass_int <- as.numeric(actual$pass_int)
  actual$rush_att <- as.numeric(actual$rush_att)
  actual$rush_yds <- as.numeric(actual$rush_yds)
  actual$rush_yds_att <- as.numeric(actual$rush_yds_att)
  actual$rush_td <- as.numeric(actual$rush_td)
  actual$receptions <- as.numeric(actual$receptions)
  actual$rec_yds <- as.numeric(actual$rec_yds)
  actual$yds_rec <- as.numeric(actual$yds_rec)
  actual$rec_td <- as.numeric(actual$rec_td)
  actual$fant_points <- as.numeric(actual$fant_points)
  actual$vbd <- as.numeric(actual$vbd)
  actual$pos_rank <- as.numeric(actual$pos_rank)
  actual$ovrank <- as.numeric(actual$ovrank)
  
  #Fantasy Points
  actual$passYdsPts <- actual$pass_yds*passYdsMultiplier
  actual$passTdsPts <- actual$pass_td*passTdsMultiplier
  actual$passIntPts <- actual$pass_int*passIntMultiplier
  actual$rushYdsPts <- actual$rush_yds*rushYdsMultiplier
  actual$rushTdsPts <- actual$rush_td*rushTdsMultiplier
  actual$recYdsPts <- actual$rec_yds*recYdsMultiplier
  actual$recTdsPts <- actual$rec_td*recTdsMultiplier
  
  actual$points <- rowSums(actual[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recYdsPts","recTdsPts")], na.rm=T)
  
  #Subset data
  actual <- actual[,c("name","pos","team","points")]
  
  ### Kickers
  actualK <- readHTMLTable(paste("http://www.pro-football-reference.com/years/", years[i], "/kicking.htm", sep=""), stringsAsFactors = FALSE)$kicking
  
  names(actualK) <- c("overall_rank", "name_info", "team", "age", "games", "games_start", "fga19", "fgm19", "fga29", "fgm29", "fga39", "fgm39", "fga49", "fgm49", "fga50", "fgm50",
                      "fga", "fgm", "fg_pct", "xpa", "xpm", "xp_pct", NA, NA, NA, NA, NA)
  
  #Remove headings within data set
  actualK <- actualK[which(actualK$overall_rank!="Rk"), ]
  actualK <- actualK[which(actualK$name_info!="0-19"), ]
  
  #Clean-up name field
  actualK$name <- str_replace_all(actualK$name_info, "[[:punct:]]", "")
  actualK$name <- toupper(actualK$name)
  
  #Remove punting columns
  actualK <- actualK[,!is.na(names(actualK))]
  actualK$pos <- "K"
  
  #Convert to numeric
  actualK$overall_rank <- as.numeric(actualK$overall_rank)
  actualK$age <- as.numeric(actualK$age)
  actualK$games <- as.numeric(actualK$games)
  actualK$games_start <- as.numeric(actualK$games_start)
  actualK$fga19 <- as.numeric(actualK$fga19)
  actualK$fgm19 <- as.numeric(actualK$fgm19)
  actualK$fga29 <- as.numeric(actualK$fga29)
  actualK$fgm29 <- as.numeric(actualK$fgm29)
  actualK$fga39 <- as.numeric(actualK$fga39)
  actualK$fgm39 <- as.numeric(actualK$fgm39)
  actualK$fga49 <- as.numeric(actualK$fga49)
  actualK$fgm49 <- as.numeric(actualK$fgm49)
  actualK$fga50 <- as.numeric(actualK$fga50)
  actualK$fgm50 <- as.numeric(actualK$fgm50)
  actualK$fga <- as.numeric(actualK$fga)
  actualK$fgm <- as.numeric(actualK$fgm)
  actualK$xpa <- as.numeric(actualK$xpa)
  actualK$xpm <- as.numeric(actualK$xpm)
  
  #Calculate FG scoring categories
  actualK$fg30 <- rowSums(actualK[,c("fgm19","fgm29","fgm39")], na.rm=TRUE)
  actualK$fg40 <- actualK$fgm49
  actualK$fg50 <- actualK$fgm50
  actualK$fgMissed <- actualK$fga - actualK$fgm
  actualK$pat <- actualK$xpm
  actualK$patMissed <- actualK$xpa - actualK$xpm
  
  #Subset data
  actualK <- actualK[!is.na(actualK$fg40) | !is.na(actualK$fg50) | !is.na(actualK$fgMissed) | !is.na(actualK$pat) | !is.na(actualK$patMissed), c("name","pos","team","fg30","fg40","fg50","fgMissed","pat","patMissed")]
  
  #Fantasy Points
  actualK$fg30Pts <- actualK$fg30*fg30Multiplier
  actualK$fg40Pts <- actualK$fg40*fg40Multiplier
  actualK$fg50Pts <- actualK$fg50*fg50Multiplier
  actualK$fgMissedPts <- actualK$fgMissed*fgMissedMultiplier
  actualK$patPts <- actualK$pat*patMultiplier
  actualK$patMissedPts <- actualK$patMissed*patMissedMultiplier
  
  actualK$points <- rowSums(actualK[,c("fg30Pts","fg40Pts","fg50Pts","fgMissedPts","patPts","patMissedPts")], na.rm=T)
  
  #Subset data
  actualK <- actualK[,c("name","pos","team","points")]
  
  ### Defense
  if(years[i] >= 2003 & years[i] <= 2011){
    actualDef <- readHTMLTable(paste("http://www.fantasyplaymakers.com/historical_fantasy_pts.php?year=", years[i], "&position=8", sep=""), stringsAsFactors = FALSE)[[20]]
    
    names(actualDef) <- c("overall_rank", "name_info", "pts", "ydsA", "passYdsA", "rushYdsA", "ptsA", "int", "fumlRec", "sack", "safety", "blockfg", "td")
    actualDef$pos <- "Def"
    
    #Cleanup name field
    actualDef$name <- str_replace_all(actualDef$name_info, "[[:punct:]]", "")
    actualDef$name <- toupper(actualDef$name)
    
    #Convert to numeric
    actualDef$pts <- as.numeric(actualDef$pts)
    actualDef$ydsA <- as.numeric(actualDef$ydsA)
    actualDef$passYdsA <- as.numeric(actualDef$passYdsA)
    actualDef$rushYdsA <- as.numeric(actualDef$rushYdsA)
    actualDef$ptsA <- as.numeric(actualDef$ptsA)
    actualDef$int <- as.numeric(actualDef$int)
    actualDef$fumlRec <- as.numeric(actualDef$fumlRec)
    actualDef$sack <- as.numeric(actualDef$sack)
    actualDef$safety <- as.numeric(actualDef$safety)
    actualDef$blockfg <- as.numeric(actualDef$blockfg)
    actualDef$td <- as.numeric(actualDef$td)
    
    #Calculate avg pts allowed per game
    actualDef$ptsAPG <- actualDef$ptsA/16
    
    #Fantasy Points
    actualDef$fumlRecoveryPts <- actualDef$fumlRec*fumlRecoveryMultiplier
    actualDef$intCaughtPts <- actualDef$int*intCaughtMultiplier
    actualDef$fgBlockedPts <- actualDef$blockfg*fgBlockedMultiplier
    actualDef$sackPts <- actualDef$sack*sackMultiplier
    actualDef$safetyPts <- actualDef$safety*safetyMultiplier
    actualDef$tdPts <- actualDef$td*tdMultiplier
    
    #Calculate points allowed per game based on SD of 10 (from: http://www.advancednflstats.com/2009/05/are-nfl-coaches-too-timid.html)
    set.seed(47401)
    sim <- sapply(actualDef$ptsAPG, function(x) rnorm(n=100000, mean=x, sd=10))
    simPApts <- matrix(nrow=dim(sim)[1], ncol=dim(sim)[2])
    
    simPApts[sim <= 0] <- pa0Multiplier
    simPApts[sim > 0 & sim <= 6] <- pa6Multiplier
    simPApts[sim > 6 & sim <= 20] <- pa20Multiplier
    simPApts[sim > 20 & sim <= 34] <- pa34Multiplier
    simPApts[sim > 34] <- pa35Multiplier
    
    actualDef$paPts <- 16*colMeans(simPApts, na.rm=TRUE)
    
    actualDef$points <- rowSums(actualDef[,c("fumlRecoveryPts","intCaughtPts","fgBlockedPts","sackPts","safetyPts","tdPts","paPts")], na.rm=T)
    
    #Subset data
    actualDef <- actualDef[,c("name","pos","points")]
  } else if(years[i] >= 2012){
    actualDef <- readHTMLTable(paste("http://fftoday.com/stats/playerstats.php?Season=", years[i], "&GameWeek=&PosID=99", sep=""), stringsAsFactors = FALSE)[[11]]
    
    names(actualDef) <- c("name_info", "games", "sack", "fumlRec", "int", "defTD", "ptsA", "passYdsAPG", "rushYdsAPG", "safety", "kickTD", "pts", "ptsPG")
    actualDef$pos <- "Def"
    
    actualDef <- actualDef[-1,]
    
    #Cleanup name
    actualDef$name <- str_sub(actualDef$name_info, start=6)
    actualDef$name <- str_trim(actualDef$name)
    actualDef$name <- str_replace_all(actualDef$name, "[[:punct:]]", "")
    actualDef$name <- toupper(actualDef$name)
    
    #Convert to numeric
    actualDef$pts <- as.numeric(actualDef$pts)
    actualDef$passYdsA <- as.numeric(actualDef$passYdsA)
    actualDef$rushYdsA <- as.numeric(actualDef$rushYdsA)
    actualDef$ptsA <- as.numeric(actualDef$ptsA)
    actualDef$int <- as.numeric(actualDef$int)
    actualDef$fumlRec <- as.numeric(actualDef$fumlRec)
    actualDef$sack <- as.numeric(actualDef$sack)
    actualDef$safety <- as.numeric(actualDef$safety)
    actualDef$defTD <- as.numeric(actualDef$defTD)
    actualDef$kickTD <- as.numeric(actualDef$kickTD)
    
    #Calculate avg pts allowed per game
    actualDef$ptsAPG <- actualDef$ptsA/16
    
    #Calculate TDs
    actualDef$td <- rowSums(actualDef[,c("defTD","kickTD")], na.rm=TRUE)
    
    #Fantasy Points
    actualDef$fumlRecoveryPts <- actualDef$fumlRec*fumlRecoveryMultiplier
    actualDef$intCaughtPts <- actualDef$int*intCaughtMultiplier
    actualDef$sackPts <- actualDef$sack*sackMultiplier
    actualDef$safetyPts <- actualDef$safety*safetyMultiplier
    actualDef$tdPts <- actualDef$td*tdMultiplier
    
    #Calculate points allowed per game based on SD of 10 (from: http://www.advancednflstats.com/2009/05/are-nfl-coaches-too-timid.html)
    set.seed(47401)
    sim <- sapply(actualDef$ptsAPG, function(x) rnorm(n=100000, mean=x, sd=10))
    simPApts <- matrix(nrow=dim(sim)[1], ncol=dim(sim)[2])
    
    simPApts[sim <= 0] <- pa0Multiplier
    simPApts[sim > 0 & sim <= 6] <- pa6Multiplier
    simPApts[sim > 6 & sim <= 20] <- pa20Multiplier
    simPApts[sim > 20 & sim <= 34] <- pa34Multiplier
    simPApts[sim > 34] <- pa35Multiplier
    
    actualDef$paPts <- 16*colMeans(simPApts, na.rm=TRUE)
    
    actualDef$points <- rowSums(actualDef[,c("fumlRecoveryPts","intCaughtPts","sackPts","safetyPts","tdPts","paPts")], na.rm=T)
    
    #Subset data
    actualDef <- actualDef[,c("name","pos","points")]
  }
  
  #Merge across positions
  merged <- merge(actual, actualK, all=TRUE)
  
  if(years[i] >= 2003){
    merged <- merge(merged, actualDef, all=TRUE)
  }

  merged$year <- years[i]
  
  #Calculate overall rank
  merged$overallRank <- rank(-merged$points, ties.method="min")
  
  #VORP
  qb <- merged[merged$pos=="QB",][order(merged[merged$pos=="QB",]$overallRank),]
  rb <- merged[merged$pos=="RB",][order(merged[merged$pos=="RB",]$overallRank),]
  wr <- merged[merged$pos=="WR",][order(merged[merged$pos=="WR",]$overallRank),]
  te <- merged[merged$pos=="TE",][order(merged[merged$pos=="TE",]$overallRank),]
  k <- merged[merged$pos=="K",][order(merged[merged$pos=="K",]$overallRank),]
  
  if(years[i] >= 2003){
    def <- merged[merged$pos=="Def",][order(merged[merged$pos=="Def",]$overallRank),]
  }

  
  qb$positionRank <- rank(-qb$points, ties.method="min")
  rb$positionRank <- rank(-rb$points, ties.method="min")
  wr$positionRank <- rank(-wr$points, ties.method="min")
  te$positionRank <- rank(-te$points, ties.method="min")
  k$positionRank <- rank(-k$points, ties.method="min")
  
  if(years[i] >= 2003){
    def$positionRank <- rank(-def$points, ties.method="min")
  }
  
  qbValueOfReplacement <- qb$points[which.min(abs(qb$positionRank - qbReplacements))] #mean(c(qb$projections[qb$positionRank==qbReplacements],qb$projections[qb$positionRank==(qbReplacements-1)],qb$projections[qb$positionRank==(qbReplacements+1)])))
  rbValueOfReplacement <- rb$points[which.min(abs(rb$positionRank - rbReplacements))] #mean(c(rb$projections[rb$positionRank==rbReplacements],rb$projections[rb$positionRank==(rbReplacements-1)],rb$projections[rb$positionRank==(rbReplacements+1)])))
  wrValueOfReplacement <- wr$points[which.min(abs(wr$positionRank - wrReplacements))] #mean(c(wr$projections[wr$positionRank==wrReplacements],wr$projections[wr$positionRank==(wrReplacements-1)],wr$projections[wr$positionRank==(wrReplacements+1)])))
  teValueOfReplacement <- te$points[which.min(abs(te$positionRank - teReplacements))] #mean(c(te$projections[te$positionRank==teReplacements],te$projections[te$positionRank==(teReplacements-1)],te$projections[te$positionRank==(teReplacements+1)])))
  kValueOfReplacement <- k$points[which.min(abs(k$positionRank - kReplacements))]
  
  if(years[i] >= 2003){
    defValueOfReplacement <- def$points[which.min(abs(def$positionRank - defReplacements))]
  }
  
  qb$vor <- qb$points - qbValueOfReplacement
  rb$vor <- rb$points - rbValueOfReplacement
  wr$vor <- wr$points - wrValueOfReplacement
  te$vor <- te$points - teValueOfReplacement
  k$vor <- k$points - kValueOfReplacement
  
  if(years[i] >= 2003){
    def$vor <- def$points - defValueOfReplacement
  }
  
  #Merge across positions
  if(years[i] >= 2003){
    merged <- rbind(qb,rb,wr,te,k,def)
  } else{
    merged <- rbind(qb,rb,wr,te,k)
  }
  
  #Order by vor
  merged <- merged[order(-merged$vor),]
  row.names(merged) <- 1:dim(merged)[1]
  
  #Save data
  write.csv(merged, file=paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/actual_", years[i], ".csv", sep=""), row.names=FALSE)
  
  #Merge in List
  actualList[[i]] <- merged
}

#Merge across list
actualMerged <- merge_recurse(actualList)

#Save data
write.csv(actualMerged, file=paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/actual.csv", sep=""), row.names=FALSE)