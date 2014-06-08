###########################
# File: Accuscore Projections.R
# Description: Downloads Fantasy Football Projections from Accuscore.com
# Date: 5/26/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("RCurl")
library("XML")
library("stringr")
library("ggplot2")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Download fantasy football projections from Accuscore.com
qb <- readHTMLTable("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/", header=1, stringsAsFactors = FALSE)$fantasy_table
rb <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-RB"), header = 1, stringsAsFactors = FALSE)$fantasy_table
wr <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-WR"), header = 1, stringsAsFactors = FALSE)$fantasy_table
te <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-TE"), header = 1, stringsAsFactors = FALSE)$fantasy_table

#Variable names
names(qb) <- c("name_accu","team_accu","pts_accu","passComp_accu","passAtt_accu","passCompPct_accu","passYds_accu","passTds_accu","passInt_accu","rushYds_accu","rushTds_accu","fumbles_accu")
names(rb) <- c("name_accu","team_accu","pts_accu","rush_accu","rushYds_accu","rushYPC_accu","rushTds_accu","rec_accu","recYds_accu","recTds_accu","fumbles_accu")
names(wr) <- c("name_accu","team_accu","pts_accu","rec_accu","recYds_accu","recYPR_accu","recTds_accu","fumbles_accu")
names(te) <- c("name_accu","team_accu","pts_accu","rec_accu","recYds_accu","recYPR_accu","recTds_accu","fumbles_accu")

#Create variable for position
qb$pos <- as.factor("QB")
rb$pos <- as.factor("RB")
wr$pos <- as.factor("WR")
te$pos <- as.factor("TE")

#Merge
projections_accu <- rbind.fill(qb, rb, wr, te)

#Convert NAs to 0
#projections_accu[is.na(projections_accu)] <- 0

#Convert variable types to numeric
projections_accu[,c("pts_accu",
                    "passComp_accu","passAtt_accu","passCompPct_accu","passYds_accu","passTds_accu","passInt_accu",
                    "rush_accu","rushYds_accu","rushYPC_accu","rushTds_accu",
                    "rec_accu","recYds_accu","recYPR_accu","recTds_accu",
                    "fumbles_accu")] <- convert.magic(projections_accu[,c("pts_accu",
                                                                          "passComp_accu","passAtt_accu","passCompPct_accu","passYds_accu","passTds_accu","passInt_accu",
                                                                          "rush_accu","rushYds_accu","rushYPC_accu","rushTds_accu",
                                                                          "rec_accu","recYds_accu","recYPR_accu","recTds_accu",
                                                                          "fumbles_accu")], "numeric")

#Player names
projections_accu$name <- nameMerge(projections_accu$name_accu)

#Variables
projections_accu$twoPts_accu <- NA

#Remove duplicate cases
projections_accu[projections_accu$name %in% projections_accu[duplicated(projections_accu$name),"name"],]
#projections_accu <- projections_accu[-which(projections_accu$name_accu=="Dexter McCluster" & projections_accu$pos=="RB"),]

#Rename players
#projections_accu[projections_accu$name_accu=="EJ Manuel", "name_accu"] <- "E.J. Manuel"

#Calculate overall rank
projections_accu$overallRank_accu <- rank(-projections_accu$pts_accu, ties.method="min")

#Calculate Position Rank
projections_accu$positionRank_accu <- NA
projections_accu[which(projections_accu$pos == "QB"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "QB"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "RB"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "RB"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "WR"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "WR"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "TE"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "TE"), "pts_accu"], ties.method="min")

#Order variables in data set
projections_accu <- projections_accu[,c("name","name_accu","pos","team_accu","positionRank_accu","overallRank_accu",
                                        "passAtt_accu","passComp_accu","passYds_accu","passTds_accu","passInt_accu",
                                        "rushYds_accu","rushTds_accu","rec_accu","recYds_accu","recTds_accu","twoPts_accu","fumbles_accu","pts_accu")]

#Order players by overall rank
projections_accu <- projections_accu[order(projections_accu$overallRank_accu),]
row.names(projections_accu) <- 1:dim(projections_accu)[1]

#Density Plot
ggplot(projections_accu, aes(x=pts_accu)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Accuscore Projected Points")
ggsave(paste(getwd(),"/Figures/Accuscore projections.jpg", sep=""))
dev.off()

#Save file
save(projections_accu, file = paste(getwd(),"/Data/Accuscore-Projections.RData", sep=""))
write.csv(projections_accu, file=paste(getwd(),"/Data/Accuscore-Projections.csv", sep=""), row.names=FALSE)

save(projections_accu, file = paste(getwd(),"/Data/Historical Projections/Accuscore-Projections-2014.RData", sep=""))
write.csv(projections_accu, file=paste(getwd(),"/Data/Historical Projections/Accuscore-Projections-2014.csv", sep=""), row.names=FALSE)
