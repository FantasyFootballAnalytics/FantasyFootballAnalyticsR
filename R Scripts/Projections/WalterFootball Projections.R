###########################
# File: WalterFootball Projections.R
# Description: Downloads Fantasy Football Projections from WalterFootball.com
# Date: 5/26/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("RCurl")
library("XLConnect")
library("stringr")
library("ggplot2")
library("plyr")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Download fantasy football projections from WalterFootball.com
url_wf <- "http://walterfootball.com/fantasy2014rankingsexcel.xls"
f = CFILE(paste(getwd(),"/Data/WalterFootball-Projections.xls", sep=""), mode="wb")
curlPerform(url = url_wf, writedata = f@ref, ssl.verifypeer = FALSE)
close(f)

qb_wf <- readWorksheetFromFile(file = paste(getwd(),"/Data/WalterFootball-Projections.xls", sep=""), sheet = "QBs", header = TRUE)
rb_wf <- readWorksheetFromFile(file = paste(getwd(),"/Data/WalterFootball-Projections.xls", sep=""), sheet = "RBs", header = TRUE)
wr_wf <- readWorksheetFromFile(file = paste(getwd(),"/Data/WalterFootball-Projections.xls", sep=""), sheet = "WRs", header = TRUE)
te_wf <- readWorksheetFromFile(file = paste(getwd(),"/Data/WalterFootball-Projections.xls", sep=""), sheet = "TEs", header = TRUE)
k_wf <- readWorksheetFromFile(file = paste(getwd(),"/Data/WalterFootball-Projections.xls", sep=""), sheet = "Ks", header = TRUE)
dst_wf <- readWorksheetFromFile(file = paste(getwd(),"/Data/WalterFootball-Projections.xls", sep=""), sheet = "DEFs", header = TRUE)

#Variable names
names(qb_wf) <- names(rb_wf) <- names(wr_wf) <- names(te_wf) <- c("lastName_wf","firstName_wf","team_wf","pos","bye_wf","passYds_wf","passTds_wf","passInt_wf","rushYds_wf","rec_wf","recYds_wf","rushRecTds_wf","bonus_wf","pts_wf","vbdReg_wf","pprPts_wf","pprVBD_wf","tdPts_wf","vbdTd_wf","pts2QB_wf","vbd2QB_wf","ptsCustom_wf","vbdCustom_wf","NA1","NA2","NA3","NA4","NA5")
names(k_wf) <- c("lastName_wf","firstName_wf","team_wf","pos","bye_wf","fg0039_wf","fg4049_wf","fg50_wf","xp_wf","pts_wf","vbdReg_wf","pprPts_wf","pprVBD_wf","tdPts_wf","vbdTd_wf","pts2QB_wf","vbd2QB_wf","ptsCustom_wf","vbdCustom_wf","NA1","NA2","NA3","NA4","NA5")
names(dst_wf) <- c("team_wf","bye_wf","regScore_wf","vbdReg_wf","pprPts_wf","pprVBD_wf","tdPts_wf","vbdTd_wf","pts2QB_wf","vbd2QB_wf","ptsCustom_wf","vbdCustom_wf","NA1","NA2","NA3","NA4","NA5")

#Create variable for position
qb_wf$pos <- as.factor("QB")
rb_wf$pos <- as.factor("RB")
wr_wf$pos <- as.factor("WR")
te_wf$pos <- as.factor("TE")
k_wf$pos <- as.factor("K")
dst_wf$pos <- as.factor("DST")

#Merge
projections_wf <- rbind.fill(qb_wf, rb_wf, wr_wf, te_wf, k_wf, dst_wf)

#Variables
projections_wf$passComp_wf <- NA
projections_wf$passAtt_wf <- NA
projections_wf$rushAtt_wf <- NA
projections_wf$rushTds_wf <- NA
projections_wf$recTds_wf <- NA
projections_wf$fumbles_wf <- NA
projections_wf$twoPts_wf <- NA

#Convert variable types to numeric
projections_wf[,c("bye_wf","passComp_wf","passAtt_wf","passYds_wf","passTds_wf","passInt_wf",
                  "rushAtt_wf","rushYds_wf","rushTds_wf",
                  "rec_wf","recYds_wf","recTds_wf",
                  "fumbles_wf","twoPts_wf",
                  "fg0039_wf","fg4049_wf","fg50_wf","xp_wf",
                  "regScore_wf","rushRecTds_wf","bonus_wf","pts_wf","vbdReg_wf","pprPts_wf","pprVBD_wf","tdPts_wf","vbdTd_wf","pts2QB_wf","vbd2QB_wf","ptsCustom_wf","vbdCustom_wf")] <- 
  convert.magic(projections_wf[,c("bye_wf","passComp_wf","passAtt_wf","passYds_wf","passTds_wf","passInt_wf",
                                  "rushAtt_wf","rushYds_wf","rushTds_wf",
                                  "rec_wf","recYds_wf","recTds_wf",
                                  "fumbles_wf","twoPts_wf",
                                  "fg0039_wf","fg4049_wf","fg50_wf","xp_wf",
                                  "regScore_wf","rushRecTds_wf","bonus_wf","pts_wf","vbdReg_wf","pprPts_wf","pprVBD_wf","tdPts_wf","vbdTd_wf","pts2QB_wf","vbd2QB_wf","ptsCustom_wf","vbdCustom_wf")], "numeric")

#Player names
projections_wf$name_wf <- paste(projections_wf$firstName_wf, projections_wf$lastName_wf, sep=" ")
projections_wf$name_wf[which(projections_wf$pos == "DST")] <- projections_wf$team_wf[which(projections_wf$pos == "DST")]
projections_wf$name <- nameMerge(projections_wf$name_wf)

#Player teams
projections_wf$team_wf <- convertTeamAbbreviation(projections_wf$team_wf)

#Calculate other variables
projections_wf$fg_wf <- mySum(projections_wf[,c("fg0039_wf","fg4049_wf","fg50_wf")])

#Remove duplicate cases
projections_wf[projections_wf$name %in% projections_wf[duplicated(projections_wf$name),"name"],]

#Same name, different player

#Rename Players

#Calculate overall rank
projections_wf$overallRank_wf <- rank(-projections_wf$pts_wf, ties.method="min")

#Calculate Position Rank
projections_wf$positionRank_wf <- NA
projections_wf[which(projections_wf$pos == "QB"), "positionRank_wf"] <- rank(-projections_wf[which(projections_wf$pos == "QB"), "pts_wf"], ties.method="min")
projections_wf[which(projections_wf$pos == "RB"), "positionRank_wf"] <- rank(-projections_wf[which(projections_wf$pos == "RB"), "pts_wf"], ties.method="min")
projections_wf[which(projections_wf$pos == "WR"), "positionRank_wf"] <- rank(-projections_wf[which(projections_wf$pos == "WR"), "pts_wf"], ties.method="min")
projections_wf[which(projections_wf$pos == "TE"), "positionRank_wf"] <- rank(-projections_wf[which(projections_wf$pos == "TE"), "pts_wf"], ties.method="min")
projections_wf[which(projections_wf$pos == "K"), "positionRank_wf"] <- rank(-projections_wf[which(projections_wf$pos == "K"), "pts_wf"], ties.method="min")
projections_wf[which(projections_wf$pos == "DST"), "positionRank_wf"] <- rank(-projections_wf[which(projections_wf$pos == "DST"), "pts_wf"], ties.method="min")

#Order variables in data set
projections_wf <- projections_wf[,c("name","name_wf","pos","team_wf","positionRank_wf","overallRank_wf","pts_wf",
                                    "passAtt_wf","passComp_wf","passYds_wf","passTds_wf","passInt_wf",
                                    "rushAtt_wf","rushYds_wf","rushTds_wf","rec_wf","recYds_wf","recTds_wf","twoPts_wf","fumbles_wf",
                                    "fg_wf","fg0039_wf","fg4049_wf","fg50_wf","xp_wf")]

#Order players by overall rank
projections_wf <- projections_wf[order(projections_wf$overallRank_wf),]
row.names(projections_wf) <- 1:dim(projections_wf)[1]

#Density Plot
ggplot(projections_wf, aes(x=pts_wf)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of WalterFootball Projected Points")
ggsave(paste(getwd(),"/Figures/WalterFootball projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_wf, file = paste(getwd(),"/Data/WalterFootball-Projections.RData", sep=""))
write.csv(projections_wf, file=paste(getwd(),"/Data/WalterFootball-Projections.csv", sep=""), row.names=FALSE)

save(projections_wf, file = paste(getwd(),"/Data/Historical Projections/WalterFootball-Projections-2014.RData", sep=""))
write.csv(projections_wf, file=paste(getwd(),"/Data/Historical Projections/WalterFootball-Projections-2014.csv", sep=""), row.names=FALSE)
