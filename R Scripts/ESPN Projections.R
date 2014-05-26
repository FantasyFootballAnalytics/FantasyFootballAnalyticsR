###########################
# File: ESPN Projections.R
# Description: Downloads Fantasy Football Projections from ESPN.com
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# -ESPN projections do not include fumbles!
# To do:
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Download fantasy football projections from ESPN.com
qb_espn <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2014&slotCategoryId=0", stringsAsFactors = FALSE)$playertable_0
rb1_espn <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2014&slotCategoryId=2", stringsAsFactors = FALSE)$playertable_0
rb2_espn <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2014&slotCategoryId=2&startIndex=40", stringsAsFactors = FALSE)$playertable_0
rb3_espn <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2014&slotCategoryId=2&startIndex=80", stringsAsFactors = FALSE)$playertable_0
wr1_espn <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2014&slotCategoryId=4", stringsAsFactors = FALSE)$playertable_0
wr2_espn <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2014&slotCategoryId=4&startIndex=40", stringsAsFactors = FALSE)$playertable_0
wr3_espn <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2014&slotCategoryId=4&startIndex=80", stringsAsFactors = FALSE)$playertable_0
te_espn <- readHTMLTable("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2014&slotCategoryId=6", stringsAsFactors = FALSE)$playertable_0

#Add variable names for each object
fileList <- c("qb_espn","rb1_espn","rb2_espn","rb3_espn","wr1_espn","wr2_espn","wr3_espn","te_espn")

for(i in 1:length(fileList)){
  assign(fileList[i],get(fileList[i])[2:dim(get(fileList[i]))[1],])
  t <- get(fileList[i])
  names(t) <-  c("positionRank_espn","player_espn","passCompAtt_espn","passYds_espn","passTds_espn","passInt_espn","rush_espn","rushYds_espn","rushTds_espn","rec_espn","recYds_espn","recTds_espn","pts_espn")
  assign(fileList[i], t)
}

#Merge players within position
rb_espn <- rbind(rb1_espn,rb2_espn,rb3_espn)
wr_espn <- rbind(wr1_espn,wr2_espn,wr3_espn)

#Add variable for player position
qb_espn$pos <- as.factor("QB")
rb_espn$pos <- as.factor("RB")
wr_espn$pos <- as.factor("WR")
te_espn$pos <- as.factor("TE")

#Merge players across positions
projections_espn <- rbind(qb_espn,rb_espn,wr_espn,te_espn)

#Replace symbols with value of zero
projections_espn$passCompAtt_espn[projections_espn$passCompAtt_espn == "--/--"] <- "0/0"
projections_espn$passYds_espn[projections_espn$passYds_espn == "--"] <- "0"
projections_espn$passTds_espn[projections_espn$passTds_espn == "--"] <- "0"
projections_espn$passInt_espn[projections_espn$passInt_espn == "--"] <- "0"
projections_espn$rush_espn[projections_espn$rush_espn == "--"] <- "0"
projections_espn$rushYds_espn[projections_espn$rushYds_espn == "--"] <- "0"
projections_espn$rushTds_espn[projections_espn$rushTds_espn == "--"] <- "0"
projections_espn$rec_espn[projections_espn$rec_espn == "--"] <- "0"
projections_espn$recYds_espn[projections_espn$recYds_espn == "--"] <- "0"
projections_espn$recTds_espn[projections_espn$recTds_espn == "--"] <- "0"
projections_espn$pts_espn[projections_espn$pts_espn == "--"] <- "0"

#Separate pass completions from attempts
projections_espn$passComp_espn <- as.numeric(str_sub(string=projections_espn$passCompAtt_espn, end=str_locate(string=projections_espn$passCompAtt_espn, '/')[,1]-1))
projections_espn$passAtt_espn <- as.numeric(str_sub(string=projections_espn$passCompAtt_espn, start=str_locate(string=projections_espn$passCompAtt_espn, '/')[,1]+1))

#Convert variables from character strings to numeric
projections_espn$positionRank_espn <- as.numeric(projections_espn$positionRank_espn)
projections_espn$passYds_espn <- as.numeric(projections_espn$passYds_espn)
projections_espn$passTds_espn <- as.numeric(projections_espn$passTds_espn)
projections_espn$passInt_espn <- as.numeric(projections_espn$passInt_espn)
projections_espn$rush_espn <- as.numeric(projections_espn$rush_espn)
projections_espn$rushYds_espn <- as.numeric(projections_espn$rushYds_espn)
projections_espn$rushTds_espn <- as.numeric(projections_espn$rushTds_espn)
projections_espn$rec_espn <- as.numeric(projections_espn$rec_espn)
projections_espn$recYds_espn <- as.numeric(projections_espn$recYds_espn)
projections_espn$recTds_espn <- as.numeric(projections_espn$recTds_espn)
projections_espn$pts_espn <- as.numeric(projections_espn$pts_espn)

#Add variables from other projection sources
projections_espn$fumbles_espn <- NA
projections_espn$twoPts_espn <- NA

#Player names
projections_espn$name_espn <- str_sub(projections_espn$player_espn, end=str_locate(string=projections_espn$player_espn, ',')[,1]-1)
projections_espn$name_espn <- str_replace_all(projections_espn$name_espn, "\\*", "")

#projections_espn[which(projections_espn$name_espn=="Steve Johnson"),"name_espn"] <- "Stevie Johnson"

#Player teams
projections_espn$team_espn <- str_sub(projections_espn$player_espn, start=str_locate(string=projections_espn$player_espn, ',')[,1]+2, end = str_locate(string=projections_espn$player_espn, ',')[,1]+4)
projections_espn$team_espn <- str_trim(projections_espn$team_espn, side="right")
projections_espn$team_espn <- toupper(projections_espn$team_espn)
projections_espn$team_espn[projections_espn$team_espn=="WSH"] <- "WAS"

#Remove duplicate cases
projections_espn[duplicated(projections_espn$name_espn),]
#projections_espn <- projections_espn[-which(projections_espn$name_espn=="Dexter McCluster" & projections_espn$pos=="RB"),]

#Rename players
projections_espn[projections_espn$name_espn=="EJ Manuel", "name_espn"] <- "E.J. Manuel"

#Calculate overall rank
projections_espn$overallRank_espn <- rank(-projections_espn$pts_espn, ties.method="min")

#Name for merging
projections_espn$name <- toupper(gsub("[[:punct:]]", "", gsub(" ", "", projections_espn$name_espn)))

#Order variables in data set
projections_espn <- projections_espn[,c("name","name_espn","pos","team_espn","positionRank_espn","overallRank_espn",
                                        "passAtt_espn","passComp_espn","passYds_espn","passTds_espn","passInt_espn",
                                        "rushYds_espn","rushTds_espn","recYds_espn","recTds_espn","twoPts_espn","fumbles_espn","pts_espn")]

#Order players by overall rank
projections_espn <- projections_espn[order(projections_espn$overallRank_espn),]
row.names(projections_espn) <- 1:dim(projections_espn)[1]

#Density Plot
ggplot(projections_espn, aes(x=pts_espn)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of ESPN Projected Points")
ggsave(paste(getwd(),"/Figures/ESPN projections.jpg", sep=""))
dev.off()

#Save file
save(projections_espn, file = paste(getwd(),"/Data/ESPN-Projections.RData", sep=""))
write.csv(projections_espn, file=paste(getwd(),"/Data/ESPN-Projections.csv", sep=""), row.names=FALSE)

save(projections_espn, file = paste(getwd(),"/Data/Historical Projections/ESPN-Projections-2014.RData", sep=""))
write.csv(projections_espn, file=paste(getwd(),"/Data/Historical Projections/ESPN-Projections-2014.csv", sep=""), row.names=FALSE)
