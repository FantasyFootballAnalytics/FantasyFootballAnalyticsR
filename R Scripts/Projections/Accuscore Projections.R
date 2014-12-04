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
library("plyr")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Suffix
suffix <- "accu"

#Download fantasy football projections from Accuscore.com
qb <- readHTMLTable("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/", header=1, stringsAsFactors = FALSE)$fantasy_table
rb <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-RB"), header = 1, stringsAsFactors = FALSE)$fantasy_table
wr <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-WR"), header = 1, stringsAsFactors = FALSE)$fantasy_table
te <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-TE"), header = 1, stringsAsFactors = FALSE)$fantasy_table
lb <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-LB"), header = 1, stringsAsFactors = FALSE)$fantasy_table
dl <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-DL"), header = 1, stringsAsFactors = FALSE)$fantasy_table
db <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-DB"), header = 1, stringsAsFactors = FALSE)$fantasy_table
dst <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-DEF-ST"), header = 1, stringsAsFactors = FALSE)$fantasy_table
kickers <- readHTMLTable(getURL("http://accuscore.com/fantasy-sports/nfl-fantasy-sports/Rest-of-Season-K"), header = 1, stringsAsFactors = FALSE)$fantasy_table

#Variable names
names(qb) <- c("name_accu","team_accu","pts_accu","passComp_accu","passAtt_accu","passCompPct_accu","passYds_accu","passTds_accu","passInt_accu","rushYds_accu","rushTds_accu","fumbles_accu")
names(rb) <- c("name_accu","team_accu","pts_accu","rushAtt_accu","rushYds_accu","rushYPC_accu","rushTds_accu","rec_accu","recYds_accu","recTds_accu","fumbles_accu")
names(wr) <- names(te) <- c("name_accu","team_accu","pts_accu","rec_accu","recYds_accu","recYPR_accu","recTds_accu","fumbles_accu")
names(lb) <- names(dl) <- c("name_accu","team_accu","pts_accu","solo_accu","ast_accu","idpSack_accu","idpFumlRec_accu","idpFumlForce_accu","idpInt","idpPD")
names(db) <- c("name_accu","team_accu","pts_accu","idpInt","idpPD","solo_accu","ast_accu","idpSack_accu","idpFumlRec_accu","idpFumlForce_accu")
names(dst) <- c("name_accu","team_accu","pts_accu","ptsAllowed_accu","dstSack_accu","dstInt_accu","dstFumlRec_accu","blk_accu","to_accu","intTd_accu","kRetTd_accu","pRetTd_accu")
names(kickers) <- c("name_accu","team_accu","pts_accu","fg_accu","fga_accu","fg3039_accu","fg4049_accu","fg50_accu","xp_accu","xpa_accu")

#Create variable for position
qb$pos <- as.factor("QB")
rb$pos <- as.factor("RB")
wr$pos <- as.factor("WR")
te$pos <- as.factor("TE")
lb$pos <- as.factor("LB")
dl$pos <- as.factor("DL")
db$pos <- as.factor("DB")
dst$pos <- as.factor("DST")
kickers$pos <- as.factor("K")

#Merge
projections_accu <- rbind.fill(qb, rb, wr, te, lb, dl, db, dst, kickers)

#Convert NAs to 0
#projections_accu[is.na(projections_accu)] <- 0

#Variables
projections_accu$returnTds_accu <- NA
projections_accu$twoPts_accu <- NA

#Convert variable types to numeric
projections_accu[,c("pts_accu",
                    "passComp_accu","passAtt_accu","passCompPct_accu","passYds_accu","passTds_accu","passInt_accu",
                    "rushAtt_accu","rushYds_accu","rushYPC_accu","rushTds_accu",
                    "rec_accu","recYds_accu","recYPR_accu","recTds_accu",
                    "returnTds_accu","twoPts_accu","fumbles_accu",
                    "solo_accu","ast_accu","idpSack_accu","idpFumlRec_accu","idpFumlForce_accu","idpInt","idpPD",
                    "ptsAllowed_accu","dstSack_accu","dstInt_accu","dstFumlRec_accu","blk_accu","to_accu","intTd_accu","kRetTd_accu","pRetTd_accu",
                    "fg_accu","fga_accu","fg3039_accu","fg4049_accu","fg50_accu","xp_accu","xpa_accu")] <- 
  convert.magic(projections_accu[,c("pts_accu",
                                    "passComp_accu","passAtt_accu","passCompPct_accu","passYds_accu","passTds_accu","passInt_accu",
                                    "rushAtt_accu","rushYds_accu","rushYPC_accu","rushTds_accu",
                                    "rec_accu","recYds_accu","recYPR_accu","recTds_accu",
                                    "returnTds_accu","twoPts_accu","fumbles_accu",
                                    "solo_accu","ast_accu","idpSack_accu","idpFumlRec_accu","idpFumlForce_accu","idpInt","idpPD",
                                    "ptsAllowed_accu","dstSack_accu","dstInt_accu","dstFumlRec_accu","blk_accu","to_accu","intTd_accu","kRetTd_accu","pRetTd_accu",
                                    "fg_accu","fga_accu","fg3039_accu","fg4049_accu","fg50_accu","xp_accu","xpa_accu")], "numeric")

#Player names
projections_accu$name <- nameMerge(projections_accu$name_accu)

#Remove duplicate cases
projections_accu[projections_accu$name %in% projections_accu[duplicated(projections_accu$name),"name"],]

#Same name, different player
projections_accu <- projections_accu[-which(projections_accu$name=="ALEXSMITH" & projections_accu$pos=="TE"),]
projections_accu <- projections_accu[-which(projections_accu$name=="STEVESMITH" & projections_accu$team_accu=="TB"),]

#Rename Players
projections_accu[projections_accu$name=="CHRISTOPHERIVORY", "name"] <- "CHRISIVORY"
projections_accu[projections_accu$name=="MERCEDESLEWIS", "name"] <- "MARCEDESLEWIS"

#Calculate overall rank
projections_accu$overallRank_accu <- rank(-projections_accu$pts_accu, ties.method="min")

#Calculate Position Rank
projections_accu$positionRank_accu <- NA
projections_accu[which(projections_accu$pos == "QB"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "QB"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "RB"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "RB"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "WR"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "WR"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "TE"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "TE"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "K"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "K"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "DST"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "DST"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "DL"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "DL"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "LB"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "LB"), "pts_accu"], ties.method="min")
projections_accu[which(projections_accu$pos == "DB"), "positionRank_accu"] <- rank(-projections_accu[which(projections_accu$pos == "DB"), "pts_accu"], ties.method="min")

#Order variables in data set
projections_accu <- projections_accu[,c(prefix, paste(varNames, suffix, sep="_"))]

#Order players by overall rank
projections_accu <- projections_accu[order(projections_accu$overallRank_accu),]
row.names(projections_accu) <- 1:dim(projections_accu)[1]

#Density Plot
ggplot(projections_accu, aes(x=pts_accu)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Accuscore Projected Points")
ggsave(paste(getwd(),"/Figures/Accuscore projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_accu, file = paste(getwd(), "/Data/Accuscore-Projections.RData", sep=""))
write.csv(projections_accu, file=paste(getwd(), "/Data/Accuscore-Projections.csv", sep=""), row.names=FALSE)

save(projections_accu, file = paste(getwd(), "/Data/Historical Projections/Accuscore-Projections-", season, ".RData", sep=""))
write.csv(projections_accu, file=paste(getwd(), "/Data/Historical Projections/Accuscore-Projections-", season, ".csv", sep=""), row.names=FALSE)
