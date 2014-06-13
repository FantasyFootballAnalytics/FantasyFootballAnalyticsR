###########################
# File: FFtoday Projections.R
# Description: Downloads Fantasy Football Projections from fftoday.com
# Date: 6/7/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Download fantasy football projections from FFtoday.com
qb1_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?PosID=10&LeagueID=1", stringsAsFactors = FALSE)[11]$'NULL'
qb2_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2014&PosID=10&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1", stringsAsFactors = FALSE)[11]$'NULL'
rb1_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?PosID=20&LeagueID=1", stringsAsFactors = FALSE)[11]$'NULL'
rb2_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2014&PosID=20&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1", stringsAsFactors = FALSE)[11]$'NULL'
wr1_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?PosID=30&LeagueID=1", stringsAsFactors = FALSE)[11]$'NULL'
wr2_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2014&PosID=30&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1", stringsAsFactors = FALSE)[11]$'NULL'
wr3_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2014&PosID=30&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=2", stringsAsFactors = FALSE)[11]$'NULL'
te1_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?PosID=40&LeagueID=1", stringsAsFactors = FALSE)[11]$'NULL'
te2_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2014&PosID=40&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1", stringsAsFactors = FALSE)[11]$'NULL'

#Add variable names for each object
names(qb1_fftoday) <- names(qb2_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","passComp_fftoday","passAtt_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","pts_fftoday")
names(rb1_fftoday) <- names(rb2_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday")
names(wr1_fftoday) <- names(wr2_fftoday) <- names(wr3_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","pts_fftoday")
names(te1_fftoday) <- names(te2_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday")

#Trim dimensions
qb1_fftoday <- qb1_fftoday[2:(dim(qb1_fftoday)[1]-1),]
qb2_fftoday <- qb2_fftoday[2:(dim(qb2_fftoday)[1]-1),]
rb1_fftoday <- rb1_fftoday[2:(dim(rb1_fftoday)[1]-1),]
rb2_fftoday <- rb2_fftoday[2:(dim(rb2_fftoday)[1]-1),]
wr1_fftoday <- wr1_fftoday[2:(dim(wr1_fftoday)[1]-1),]
wr2_fftoday <- wr2_fftoday[2:(dim(wr2_fftoday)[1]-1),]
wr3_fftoday <- wr3_fftoday[2:(dim(wr3_fftoday)[1]-1),]
te1_fftoday <- te1_fftoday[2:(dim(te1_fftoday)[1]-1),]
te2_fftoday <- te2_fftoday[2:(dim(te2_fftoday)[1]-1),]

#Merge within position
qb_fftoday <- rbind(qb1_fftoday,qb2_fftoday)
rb_fftoday <- rbind(rb1_fftoday,rb2_fftoday)
wr_fftoday <- rbind(wr1_fftoday,wr2_fftoday,wr3_fftoday)
te_fftoday <- rbind(te1_fftoday,te2_fftoday)

#Add variable for player position
qb_fftoday$pos <- as.factor("QB")
rb_fftoday$pos <- as.factor("RB")
wr_fftoday$pos <- as.factor("WR")
te_fftoday$pos <- as.factor("TE")

#Merge across positions
projections_fftoday <- rbind.fill(qb_fftoday, rb_fftoday, wr_fftoday, te_fftoday)

#Add missing variables
projections_fftoday$twoPts_fftoday <- NA
projections_fftoday$fumbles_fftoday <- NA

#Remove special characters(commas)
projections_fftoday[,c("passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday","twoPts_fftoday","fumbles_fftoday")] <-
  apply(projections_fftoday[,c("passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday","twoPts_fftoday","fumbles_fftoday")], 2, function(x) gsub("\\,", "", x))

#Convert variables from character strings to numeric
projections_fftoday[,c("passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday","twoPts_fftoday","fumbles_fftoday")] <- 
  convert.magic(projections_fftoday[,c("passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday","twoPts_fftoday","fumbles_fftoday")], "numeric")

#Player name, position, and team
projections_fftoday$name_fftoday <- str_trim(str_sub(projections_fftoday$player, start=2))
projections_fftoday$name <- nameMerge(projections_fftoday$name_fftoday)

#Remove duplicate cases
projections_fftoday[projections_fftoday$name %in% projections_fftoday[duplicated(projections_fftoday$name),"name"],]
#projections_fftoday <- projections_fftoday[-which(projections_fftoday$name_fftoday=="Dexter McCluster" & projections_fftoday$pos=="RB"),]

#Rename players
#projections_fftoday[projections_fftoday$name_fftoday=="EJ Manuel", "name_fftoday"] <- "E.J. Manuel"

#Calculate overall rank
projections_fftoday$overallRank_fftoday <- rank(-projections_fftoday$pts_fftoday, ties.method="min")

#Calculate Position Rank
projections_fftoday$positionRank_fftoday <- NA
projections_fftoday[which(projections_fftoday$pos == "QB"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "QB"), "pts_fftoday"], ties.method="min")
projections_fftoday[which(projections_fftoday$pos == "RB"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "RB"), "pts_fftoday"], ties.method="min")
projections_fftoday[which(projections_fftoday$pos == "WR"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "WR"), "pts_fftoday"], ties.method="min")
projections_fftoday[which(projections_fftoday$pos == "TE"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "TE"), "pts_fftoday"], ties.method="min")

#Order variables in data set
projections_fftoday <- projections_fftoday[,c("name","name_fftoday","pos","team_fftoday","positionRank_fftoday","overallRank_fftoday",
                                              "passYds_fftoday","passTds_fftoday","passInt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday",
                                              "twoPts_fftoday","fumbles_fftoday","pts_fftoday")]

#Order players by overall rank
projections_fftoday <- projections_fftoday[order(projections_fftoday$overallRank_fftoday),]
row.names(projections_fftoday) <- 1:dim(projections_fftoday)[1]

#Density Plot
ggplot(projections_fftoday, aes(x=pts_fftoday)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FFtoday Projected Points")
ggsave(paste(getwd(),"/Figures/FFtoday projections.jpg", sep=""))
dev.off()

#Save file
save(projections_fftoday, file = paste(getwd(),"/Data/FFtoday-Projections.RData", sep=""))
write.csv(projections_fftoday, file=paste(getwd(),"/Data/FFtoday-Projections.csv", sep=""), row.names=FALSE)

save(projections_fftoday, file = paste(getwd(),"/Data/Historical Projections/FFtoday-Projections-2014.RData", sep=""))
write.csv(projections_fftoday, file=paste(getwd(),"/Data/Historical Projections/FFtoday-Projections-2014.csv", sep=""), row.names=FALSE)
