##############
# File: ESPN Projections.R
# Description: Downloads Fantasy Football Projections from ESPN.com
# Date: 3/2/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -ESPN projections do not include fumbles!
##############

#Load libraries
library("XML")
library("stringr")

#Download fantasy football projections from ESPN.com
qb <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2012&slotCategoryId=0", stringsAsFactors = FALSE)$playertable_0
rb1 <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2012&slotCategoryId=2", stringsAsFactors = FALSE)$playertable_0
rb2 <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2012&slotCategoryId=2&startIndex=40", stringsAsFactors = FALSE)$playertable_0
rb3 <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2012&slotCategoryId=2&startIndex=80", stringsAsFactors = FALSE)$playertable_0
wr1 <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2012&slotCategoryId=4", stringsAsFactors = FALSE)$playertable_0
wr2 <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2012&slotCategoryId=4&startIndex=40", stringsAsFactors = FALSE)$playertable_0
wr3 <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2012&slotCategoryId=4&startIndex=80", stringsAsFactors = FALSE)$playertable_0
te <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2012&slotCategoryId=6", stringsAsFactors = FALSE)$playertable_0

#Add variable names for each object
fileList <- c("qb","rb1","rb2","rb3","wr1","wr2","wr3","te")

for(i in 1:length(fileList)){
  assign(fileList[i],get(fileList[i])[2:dim(get(fileList[i]))[1],])
  t <- get(fileList[i])
  names(t) <-  c("positionRank","player","passCompAtt","passYds","passTds","passInt","rush","rushYds","rushTds","rec","recYds","recTds","pts")
  assign(fileList[i], t)
}

#Merge players within position
rb <- rbind(rb1,rb2,rb3)
wr <- rbind(wr1,wr2,wr3)

#Add variable for player position
qb$pos <- as.factor("QB")
rb$pos <- as.factor("RB")
wr$pos <- as.factor("WR")
te$pos <- as.factor("TE")

#Merge players across positions
projections <- rbind(qb,rb,wr,te)

#Separate pass completions from attempts
projections$passComp <- as.numeric(str_sub(string=projections$passCompAtt, end=str_locate(string=projections$passCompAtt, '/')[,1]-1))
projections$passAtt <- as.numeric(str_sub(string=projections$passCompAtt, start=str_locate(string=projections$passCompAtt, '/')[,1]+1))

#Convert variables from character strings to numeric
projections$positionRank <- as.numeric(projections$positionRank)
projections$passYds <- as.numeric(projections$passYds)
projections$passTds <- as.numeric(projections$passTds)
projections$passInt <- as.numeric(projections$passInt)
projections$rush <- as.numeric(projections$rush)
projections$rushYds <- as.numeric(projections$rushYds)
projections$rushTds <- as.numeric(projections$rushTds)
projections$rec <- as.numeric(projections$rec)
projections$recYds <- as.numeric(projections$recYds)
projections$recTds <- as.numeric(projections$recTds)
projections$pts <- as.numeric(projections$pts)

#Player names
projections$name <- str_sub(projections$player, end=str_locate(string=projections$player, ',')[,1]-1)
projections$name <- str_replace_all(projections$name, "\\*", "")

projections[which(projections$name=="Steve Johnson"),"name"] <- "Stevie Johnson"

#Player teams
projections$team <- str_sub(projections$player, start=str_locate(string=projections$player, ',')[,1]+2, end = str_locate(string=projections$player, ',')[,1]+4)
projections$team <- str_trim(projections$team, side="right")

#Calculate overall rank
projections$overallRank <- rank(-projections$pts, ties.method="min")

#Order variables in data set
projections <- projections[,c("name","pos","team","positionRank","overallRank","passAtt","passComp","passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","pts")]

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]