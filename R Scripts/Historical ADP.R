#Info
#ADP (from 1999): http://football.myfantasyleague.com/1999/adp?COUNT=500&POS=*&CUTOFF=5&FRANCHISES=-1&IS_PPR=-1&IS_KEEPER=0&IS_MOCK=-1&TIME=
#Actual (from 1999): http://www.pro-football-reference.com/years/1999/fantasy.htm
#Actual - Kickers (from 1999): http://www.pro-football-reference.com/years/1999/kicking.htm
#Actual - Defense (from 2003): http://www.fantasyplaymakers.com/historical_fantasy_pts.php?year=2003&position=8
#Standard Scoring Settings: http://www.fantasypros.com/scoring-settings/

#Libraries
load_or_install(c("XML","stringr"))

#Data
years <- 1999:2012

#Loop to import, process, merge, and save historical ADP data
adpList <- list()

for(i in 1:length(years)){
  adp <- players <- NULL
  #adp <- readHTMLTable(paste("http://football.myfantasyleague.com/", i, "/adp?COUNT=500&POS=*&CUTOFF=5&FRANCHISES=-1&IS_PPR=-1&IS_KEEPER=0&IS_MOCK=-1&TIME=", sep=""), header=TRUE)[[2]] #to scrape
  adp <- xmlToList(xmlParse(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/adp_", years[i], ".xml", sep="")))
  adp$.attrs <- NULL
  adp <- data.frame(do.call(rbind, adp))
  
  players <- xmlToList(xmlParse(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/players_", years[i], ".xml", sep="")))
  players$.attrs <- NULL
  players <- data.frame(do.call(rbind, players))
  
  merged <- merge(players, adp, by="id", all.y=TRUE)
  
  #Change player naming style
  playerNames <- strsplit(as.character(merged$name), ", ")
  
  merged$name <- unlist(lapply(playerNames, 
                                    function(x) paste(x[1:length(x) %% 2 == 0], 
                                                      x[1:length(x) %% 2 != 0])))
  
  merged$name <- str_replace_all(merged$name, "[[:punct:]]", "")
  merged$name <- toupper(merged$name)
  
  #Keep only QBs, RBs, WRs, TEs, Ks, and Defs
  merged$pos <- as.character(merged$position)
  merged$pos[merged$pos == "PK"] <- "K"
  merged <- merged[merged$pos %in% c("QB","RB","WR","TE","K","Def"),]
  
  #Calculate overall rank
  merged$pick <- as.numeric(as.character(merged$averagePick))
  merged$overallRank <- rank(merged$pick, ties.method="min")
  
  #Calculate position rank
  qb <- merged[merged$pos=="QB",]
  rb <- merged[merged$pos=="RB",]
  wr <- merged[merged$pos=="WR",]
  te <- merged[merged$pos=="TE",]
  k <- merged[merged$pos=="K",]
  def <- merged[merged$pos=="Def",]
  
  qb$positionRank <- rank(qb$pick, ties.method="min")
  rb$positionRank <- rank(rb$pick, ties.method="min")
  wr$positionRank <- rank(wr$pick, ties.method="min")
  te$positionRank <- rank(te$pick, ties.method="min")
  k$positionRank <- rank(k$pick, ties.method="min")
  def$positionRank <- rank(def$pick, ties.method="min")
  
  #Merge across positions
  merged <- rbind(qb,rb,wr,te,k,def)
  
  #Order by average pick
  merged <- merged[order(merged$pick),]
  row.names(merged) <- 1:dim(merged)[1]
  
  #Add year variable
  merged$year <- years[i]
  
  #Subset data
  merged <- merged[,c("name","year","pos","team","pick","overallRank","positionRank")]
  
  #Save data
  write.csv(merged, file=paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/adp_", years[i], ".csv", sep=""), row.names=FALSE)

  #Merge in List
  adpList[[i]] <- merged
}

#Merge across list
adpMerged <- merge_recurse(adpList)

#Save data
write.csv(adpMerged, file=paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/adp.csv", sep=""), row.names=FALSE)
