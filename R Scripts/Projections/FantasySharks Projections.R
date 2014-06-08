###########################
# File: FantasySharks Projections.R
# Description: Downloads Fantasy Football Projections from FantasySharks.com
# Date: 5/26/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("stringr")
library("ggplot2")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Download fantasy football projections from FantasySharks.com
projections_fs <- read.csv("http://www.fantasysharks.com/apps/Projections/SeasonCSVProjections.php?l=11")

#Player position
projections_fs$pos <- projections_fs$Pos

#Keep only QB, RB, WR, TE
projections_fs <- projections_fs[which(projections_fs$pos %in% c("QB","RB","WR","TE")),]

#Player names
projections_fs$name_fs <- paste(projections_fs$FirstName, projections_fs$LastName, sep=" ")
projections_fs$name <- nameMerge(projections_fs$name_fs)

#Team
projections_fs$team_fs <- as.character(projections_fs$Team)

#Variables
projections_fs$passAtt_fs <- NA
projections_fs$passComp_fs <- projections_fs$PassCmps
projections_fs$passYds_fs <- projections_fs$PassYards
projections_fs$passTds_fs <- projections_fs$PassTDTotal
projections_fs$passInt_fs <- projections_fs$PassInt
projections_fs$rushYds_fs <- projections_fs$RushYards
projections_fs$rushTds_fs <- projections_fs$RushTDTotal
projections_fs$rec_fs <- projections_fs$Receptions
projections_fs$recYds_fs <- projections_fs$RecYards
projections_fs$recTds_fs <- projections_fs$RecTDTotal
projections_fs$fumbles_fs <- projections_fs$Fumbles
projections_fs$twoPts_fs <- NA
projections_fs$pts_fs <- projections_fs$FantasyPts

#Remove duplicate cases
projections_fs[projections_fs$name %in% projections_fs[duplicated(projections_fs$name),"name"],]
#projections_fs <- projections_fs[-which(projections_fs$name_fs=="Dexter McCluster" & projections_fs$pos=="RB"),]

#Rename players
#projections_fs[projections_fs$name_espn=="EJ Manuel", "name_espn"] <- "E.J. Manuel"

#Calculate overall rank
projections_fs$overallRank_fs <- rank(-projections_fs$pts_fs, ties.method="min")

#Calculate Position Rank
projections_fs$positionRank_fs <- NA
projections_fs[which(projections_fs$pos == "QB"), "positionRank_fs"] <- rank(-projections_fs[which(projections_fs$pos == "QB"), "pts_fs"], ties.method="min")
projections_fs[which(projections_fs$pos == "RB"), "positionRank_fs"] <- rank(-projections_fs[which(projections_fs$pos == "RB"), "pts_fs"], ties.method="min")
projections_fs[which(projections_fs$pos == "WR"), "positionRank_fs"] <- rank(-projections_fs[which(projections_fs$pos == "WR"), "pts_fs"], ties.method="min")
projections_fs[which(projections_fs$pos == "TE"), "positionRank_fs"] <- rank(-projections_fs[which(projections_fs$pos == "TE"), "pts_fs"], ties.method="min")

#Order variables in data set
projections_fs <- projections_fs[,c("name","name_fs","pos","team_fs","positionRank_fs","overallRank_fs",
                                    "passAtt_fs","passComp_fs","passYds_fs","passTds_fs","passInt_fs",
                                    "rushYds_fs","rushTds_fs","rec_fs","recYds_fs","recTds_fs","twoPts_fs","fumbles_fs","pts_fs")]

#Order players by overall rank
projections_fs <- projections_fs[order(projections_fs$overallRank_fs),]
row.names(projections_fs) <- 1:dim(projections_fs)[1]

#Density Plot
ggplot(projections_fs, aes(x=pts_fs)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FantasySharks Projected Points")
ggsave(paste(getwd(),"/Figures/FantasySharks projections.jpg", sep=""))
dev.off()

#Save file
save(projections_fs, file = paste(getwd(),"/Data/FantasySharks-Projections.RData", sep=""))
write.csv(projections_fs, file=paste(getwd(),"/Data/FantasySharks-Projections.csv", sep=""), row.names=FALSE)

save(projections_fs, file = paste(getwd(),"/Data/Historical Projections/FantasySharks-Projections-2014.RData", sep=""))
write.csv(projections_fs, file=paste(getwd(),"/Data/Historical Projections/FantasySharks-Projections-2014.csv", sep=""), row.names=FALSE)
