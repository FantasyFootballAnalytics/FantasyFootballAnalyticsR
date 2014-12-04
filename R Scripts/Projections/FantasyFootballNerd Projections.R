###########################
# File: FantasyFootballNerd Projections.R
# Description: Downloads Fantasy Football Projections from FantasyFootballNerd.com
# Date: 3/3/2013
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
suffix <- "ffn"

#Download fantasy football projections from FantasyFootballNerd.com
qb_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections", stringsAsFactors = FALSE)$projections
rb_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/RB", stringsAsFactors = FALSE)$projections
wr_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/WR", stringsAsFactors = FALSE)$projections
te_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/TE", stringsAsFactors = FALSE)$projections
kickers_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/K", stringsAsFactors = FALSE)$projections
dst_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/DEF", stringsAsFactors = FALSE)$projections

#Add variable names for each object
names(qb_ffn) <- c("name_ffn","team_ffn","passComp_ffn","passAtt_ffn","passCompPct_ffn","passYds_ffn","passTds_ffn","passInt_ffn","rushYds_ffn","rushTds_ffn","pts_ffn")
names(rb_ffn) <- c("name_ffn","team_ffn","rushAtt_ffn","rushYds_ffn","rushYpc_ffn","rushTds_ffn","rec_ffn","recYds_ffn","recTds_ffn","fumbles_ffn","pts_ffn")
names(wr_ffn) <- c("name_ffn","team_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","rushAtt_ffn","rushYds_ffn","rushTds_ffn","fumbles_ffn","pts_ffn")
names(te_ffn) <- c("name_ffn","team_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","rushAtt_ffn","rushYds_ffn","rushTds_ffn","fumbles_ffn","pts_ffn")
names(kickers_ffn) <- c("name_ffn","team_ffn","xp_ffn","fg_ffn","pts_ffn")
names(dst_ffn) <- c("name_ffn","dstSack_ffn","dstInt_ffn","dstFumlRec_ffn","dstDefTd_ffn","dstStTd_ffn","pts_ffn")

#Add variable for player position
qb_ffn$pos <- as.factor("QB")
rb_ffn$pos <- as.factor("RB")
wr_ffn$pos <- as.factor("WR")
te_ffn$pos <- as.factor("TE")
kickers_ffn$pos <- as.factor("K")
dst_ffn$pos <- as.factor("DST")

#Merge players across positions
projections_ffn <- rbind.fill(qb_ffn, rb_ffn, wr_ffn, te_ffn, kickers_ffn, dst_ffn)

#Add variables from other projection sources
projections_ffn$returnTds_ffn <- NA
projections_ffn$twoPts_ffn <- NA

#Remove special characters (percentage sign)
projections_ffn[,c("passComp_ffn","passAtt_ffn","passCompPct_ffn","passYds_ffn","passTds_ffn","passInt_ffn","rushAtt_ffn","rushYds_ffn","rushYpc_ffn","rushTds_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","returnTds_ffn","twoPts_ffn","fumbles_ffn","pts_ffn",
                   "xp_ffn","fg_ffn",
                   "dstSack_ffn","dstInt_ffn","dstFumlRec_ffn","dstDefTd_ffn","dstStTd_ffn")] <-
  apply(projections_ffn[,c("passComp_ffn","passAtt_ffn","passCompPct_ffn","passYds_ffn","passTds_ffn","passInt_ffn","rushAtt_ffn","rushYds_ffn","rushYpc_ffn","rushTds_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","returnTds_ffn","twoPts_ffn","fumbles_ffn","pts_ffn",
                           "xp_ffn","fg_ffn",
                           "dstSack_ffn","dstInt_ffn","dstFumlRec_ffn","dstDefTd_ffn","dstStTd_ffn")], 2, function(x) gsub("\\%", "", x))

#Convert variables from character strings to numeric
projections_ffn[,c("passComp_ffn","passAtt_ffn","passCompPct_ffn","passYds_ffn","passTds_ffn","passInt_ffn","rushAtt_ffn","rushYds_ffn","rushYpc_ffn","rushTds_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","returnTds_ffn","twoPts_ffn","fumbles_ffn","pts_ffn",
                   "xp_ffn","fg_ffn",
                   "dstSack_ffn","dstInt_ffn","dstFumlRec_ffn","dstDefTd_ffn","dstStTd_ffn")] <-
  convert.magic(projections_ffn[,c("passComp_ffn","passAtt_ffn","passCompPct_ffn","passYds_ffn","passTds_ffn","passInt_ffn","rushAtt_ffn","rushYds_ffn","rushYpc_ffn","rushTds_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","returnTds_ffn","twoPts_ffn","fumbles_ffn","pts_ffn",
                                   "xp_ffn","fg_ffn",
                                   "dstSack_ffn","dstInt_ffn","dstFumlRec_ffn","dstDefTd_ffn","dstStTd_ffn")], "numeric")

#Name for merging
projections_ffn$name <- nameMerge(projections_ffn$name_ffn)

#Remove duplicate cases
projections_ffn[projections_ffn$name %in% projections_ffn[duplicated(projections_ffn$name),"name"],]

#Same name, different player

#Same player, different position

#Calculate overall rank
projections_ffn$overallRank_ffn <- rank(-projections_ffn$pts_ffn, ties.method="min")

#Calculate Position Rank
projections_ffn$positionRank_ffn <- NA
projections_ffn[which(projections_ffn$pos == "QB"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "QB"), "pts_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "RB"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "RB"), "pts_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "WR"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "WR"), "pts_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "TE"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "TE"), "pts_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "K"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "K"), "pts_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "DST"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "DST"), "pts_ffn"], ties.method="min")

#Order variables in data set
projections_ffn <- projections_ffn[,c(prefix, paste(varNames, suffix, sep="_"))]

#Order players by overall rank
projections_ffn <- projections_ffn[order(projections_ffn$overallRank_ffn),]
row.names(projections_ffn) <- 1:dim(projections_ffn)[1]

#Density Plot
ggplot(projections_ffn, aes(x=pts_ffn)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FantasyFootballNerd Projected Points")
ggsave(paste(getwd(),"/Figures/FantasyFootballNerd projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_ffn, file = paste(getwd(), "/Data/FantasyFootballNerd-Projections.RData", sep=""))
write.csv(projections_ffn, file=paste(getwd(), "/Data/FantasyFootballNerd-Projections.csv", sep=""), row.names=FALSE)

save(projections_ffn, file = paste(getwd(), "/Data/Historical Projections/FantasyFootballNerd-Projections-", season, ".RData", sep=""))
write.csv(projections_ffn, file=paste(getwd(), "/Data/Historical Projections/FantasyFootballNerd-Projections-", season, ".csv", sep=""), row.names=FALSE)
