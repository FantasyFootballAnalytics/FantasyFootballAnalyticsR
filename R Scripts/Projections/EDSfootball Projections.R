###########################
# File: EDSfootball Projections.R
# Description: Downloads Fantasy Football Projections from EDSfootball.com
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

#Suffix
suffix <- "eds"

#Download fantasy football projections from EDSfootball.com
qb_eds <- readHTMLTable("http://www.eatdrinkandsleepfootball.com/fantasy/projections/2014/qb/", stringsAsFactors = FALSE)[2]$'NULL'
rb_eds <- readHTMLTable("http://www.eatdrinkandsleepfootball.com/fantasy/projections/2014/rb/", stringsAsFactors = FALSE)[2]$'NULL'
wr_eds <- readHTMLTable("http://www.eatdrinkandsleepfootball.com/fantasy/projections/2014/wr/", stringsAsFactors = FALSE)[2]$'NULL'
te_eds <- readHTMLTable("http://www.eatdrinkandsleepfootball.com/fantasy/projections/2014/te/", stringsAsFactors = FALSE)[2]$'NULL'

#Add variable names for each object
names(qb_eds) <- c("positionRank_eds","player_eds","team_eds","passComp_eds","passAtt_eds","passYds_eds","passTds_eds","passInt_eds","rushAtt_eds","rushYds_eds","rushTds_eds","pts_eds")
names(rb_eds) <- c("positionRank_eds","player_eds","team_eds","rushAtt_eds","rushYds_eds","rushTds_eds","rec_eds","recYds_eds","recTds_eds","pts_eds")
names(wr_eds) <- c("positionRank_eds","player_eds","team_eds","rec_eds","recYds_eds","recTds_eds","rushAtt_eds","rushYds_eds","rushTds_eds","pts_eds")
names(te_eds) <- c("positionRank_eds","player_eds","team_eds","rec_eds","recYds_eds","recTds_eds","pts_eds")

#Add variable for player position
qb_eds$pos <- as.factor("QB")
rb_eds$pos <- as.factor("RB")
wr_eds$pos <- as.factor("WR")
te_eds$pos <- as.factor("TE")

#Merge across positions
projections_eds <- rbind.fill(qb_eds, rb_eds, wr_eds, te_eds)

#Add missing variables
projections_eds$returnTds_eds <- NA
projections_eds$twoPts_eds <- NA
projections_eds$fumbles_eds <- NA

#Remove special characters

#Convert variables from character strings to numeric
projections_eds[,c("passComp_eds","passAtt_eds","passYds_eds","passTds_eds","passInt_eds","rushAtt_eds","rushYds_eds","rushTds_eds","rec_eds","recYds_eds","recTds_eds","pts_eds","returnTds_eds","twoPts_eds","fumbles_eds")] <- 
  convert.magic(projections_eds[,c("passComp_eds","passAtt_eds","passYds_eds","passTds_eds","passInt_eds","rushAtt_eds","rushYds_eds","rushTds_eds","rec_eds","recYds_eds","recTds_eds","pts_eds","returnTds_eds","twoPts_eds","fumbles_eds")], "numeric")

#Player name, position, and team
projections_eds$name_eds <- projections_eds$player
projections_eds$name <- nameMerge(projections_eds$name_eds)

projections_eds$team_eds <- convertTeamAbbreviation(projections_eds$team_eds)

#Remove duplicate cases
projections_eds[projections_eds$name %in% projections_eds[duplicated(projections_eds$name),"name"],]

#Rename players

#Calculate overall rank
projections_eds$overallRank_eds <- rank(-projections_eds$pts_eds, ties.method="min")

#Calculate Position Rank
projections_eds$positionRank_eds <- NA
projections_eds[which(projections_eds$pos == "QB"), "positionRank_eds"] <- rank(-projections_eds[which(projections_eds$pos == "QB"), "pts_eds"], ties.method="min")
projections_eds[which(projections_eds$pos == "RB"), "positionRank_eds"] <- rank(-projections_eds[which(projections_eds$pos == "RB"), "pts_eds"], ties.method="min")
projections_eds[which(projections_eds$pos == "WR"), "positionRank_eds"] <- rank(-projections_eds[which(projections_eds$pos == "WR"), "pts_eds"], ties.method="min")
projections_eds[which(projections_eds$pos == "TE"), "positionRank_eds"] <- rank(-projections_eds[which(projections_eds$pos == "TE"), "pts_eds"], ties.method="min")

#Order variables in data set
projections_eds <- projections_eds[,c(prefix, paste(varNames, suffix, sep="_"))]

#Order players by overall rank
projections_eds <- projections_eds[order(projections_eds$overallRank_eds),]
row.names(projections_eds) <- 1:dim(projections_eds)[1]

#Density Plot
ggplot(projections_eds, aes(x=pts_eds)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of EDSfootball Projected Points")
ggsave(paste(getwd(),"/Figures/EDSfootball projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_eds, file = paste(getwd(), "/Data/EDSfootball-Projections.RData", sep=""))
write.csv(projections_eds, file=paste(getwd(), "/Data/EDSfootball-Projections.csv", sep=""), row.names=FALSE)

save(projections_eds, file = paste(getwd(), "/Data/Historical Projections/EDSfootball-Projections-", season, ".RData", sep=""))
write.csv(projections_eds, file=paste(getwd(), "/Data/Historical Projections/EDSfootball-Projections-", season, ".csv", sep=""), row.names=FALSE)
