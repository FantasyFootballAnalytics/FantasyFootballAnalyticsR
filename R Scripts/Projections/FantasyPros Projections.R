###########################
# File: FantasyPros Projections.R
# Description: Downloads Fantasy Football Projections from FantasyPros.com
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

#Download fantasy football projections from FantasyPros.com
qb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/qb.php", stringsAsFactors = FALSE)$data
rb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/rb.php", stringsAsFactors = FALSE)$data
wr_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/wr.php", stringsAsFactors = FALSE)$data
te_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/te.php", stringsAsFactors = FALSE)$data

#Add variable names for each object
names(qb_fp) <- c("player_fp","passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","fumbles_fp","pts_fp")
names(rb_fp) <- c("player_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","fumbles_fp","pts_fp")
names(wr_fp) <- c("player_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","fumbles_fp","pts_fp")
names(te_fp) <- c("player_fp","rec_fp","recYds_fp","recTds_fp","fumbles_fp","pts_fp")

#Add variable for player position
qb_fp$pos <- as.factor("QB")
rb_fp$pos <- as.factor("RB")
wr_fp$pos <- as.factor("WR")
te_fp$pos <- as.factor("TE")

#Merge players across positions
projections_fp <- rbind.fill(qb_fp, rb_fp, wr_fp, te_fp)

#Add variables from other projection sources
projections_fp$twoPts_fp <- NA

#Remove special characters(commas)
projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","twoPts_fp","fumbles_fp","pts_fp")] <-
  apply(projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","twoPts_fp","fumbles_fp","pts_fp")], 2, function(x) gsub("\\,", "", x))

#Convert variables from character strings to numeric
projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","twoPts_fp","fumbles_fp","pts_fp")] <-
  convert.magic(projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","twoPts_fp","fumbles_fp","pts_fp")], "numeric")

#Player names
projections_fp <- adply(projections_fp, 1, function(x) {
  if(is.na(str_locate(string=x$player_fp, '\\(')[,1]) == TRUE){
    x$name_fp <- x$player_fp
  } else{
    x$name_fp <- str_sub(x$player_fp, end=str_locate(string=x$player_fp, '\\(')[,1]-2)
  }
  x
}         
)

#Name for merging
projections_fp$name <- nameMerge(projections_fp$name_fp)

#Player teams
projections_fp$team_fp <- str_sub(projections_fp$player_fp, start=str_locate(string=projections_fp$player_fp, '\\(')[,1]+1, end = str_locate(string=projections_fp$player_fp, '\\)')[,1]-1)

#Remove rows with all NAs
projections_fp <- projections_fp[apply(projections_fp, 1, function(x) any(!is.na(x))),]

#Remove rows with missing player name
projections_fp <- projections_fp[-which(projections_fp$name_fp == ""),]

#Rename players
projections_fp$name[which(projections_fp$name == "CHRISTOPHERIVORY")] <- "CHRISIVORY"

#Remove duplicate cases
projections_fp[projections_fp$name %in% projections_fp[duplicated(projections_fp$name),"name"],]

#Same name, different player
projections_fp <- projections_fp[-which(projections_fp$name=="ZACHMILLER" & projections_fp$team_fp=="CHI"),]

#Same player, different position
dropNames <- c("DEXTERMCCLUSTER")
dropVariables <- c("pos")
dropLabels <- c("WR")

projections_fp2 <- ddply(projections_fp, .(name), numcolwise(mean), na.rm=TRUE)

for(i in 1:length(dropNames)){
  if(dim(projections_fp[-which(projections_fp[,"name"] == dropNames[i] & projections_fp[,dropVariables[i]] == dropLabels[i]),])[1] > 0){
    projections_fp <- projections_fp[-which(projections_fp[,"name"] == dropNames[i] & projections_fp[,dropVariables[i]] == dropLabels[i]),]
  }
}

projections_fp <- merge(projections_fp2, projections_fp[,c("name","name_fp","player_fp","pos","team_fp")], by="name")

#projections_fp[projections_fp$name_fp=="Alex Smith",][1,] <- NA
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Alex Smith" & projections_fp$pos=="TE"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Charles Clay" & projections_fp$pos=="RB"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Chris Givens" & projections_fp$team_fp=="NO"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Clay Harbor" & projections_fp$pos=="WR"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="David Johnson" & projections_fp$pos=="TE"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Dexter McCluster" & projections_fp$pos=="RB"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Dorin Dickerson" & projections_fp$pos=="WR"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Dorin Dickerson" & projections_fp$pos=="RB"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Evan Rodriguez" & projections_fp$pos=="RB"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Jamie McCoy" & projections_fp$pos=="RB"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="James Casey" & projections_fp$pos=="RB"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Niles Paul" & projections_fp$pos=="WR"),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Steve Smith" & is.na(projections_fp$team_fp)),]
#projections_fp <- projections_fp[-which(projections_fp$name_fp=="Zach Miller" & projections_fp$team_fp=="CHI"),]

#Rename Players
#projections_fp[projections_fp$name_fp=="Christopher Ivory", "name_fp"] <- "Chris Ivory"
#projections_fp[projections_fp$name_fp=="Ty Hilton", "name_fp"] <- "T.Y. Hilton"
#projections_fp[projections_fp$name_fp=="Robert Housler", "name_fp"] <- "Rob Housler"
#projections_fp[projections_fp$name_fp=="Reuben Randle", "name_fp"] <- "Rueben Randle"
#projections_fp[projections_fp$name_fp=="Joseph Morgan", "name_fp"] <- "Joe Morgan"

#Calculate overall rank
projections_fp$overallRank_fp <- rank(-projections_fp$pts_fp, ties.method="min")

#Order variables in data set
projections_fp <- projections_fp[,c("name","name_fp","pos","team_fp","overallRank_fp",
                                    "passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp",
                                    "rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","twoPts_fp","fumbles_fp","pts_fp")]

#Order players by overall rank
projections_fp <- projections_fp[order(projections_fp$overallRank_fp),]
row.names(projections_fp) <- 1:dim(projections_fp)[1]

#Density Plot
ggplot(projections_fp, aes(x=pts_fp)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FantasyPros Projected Points")
ggsave(paste(getwd(),"/Figures/FantasyPros projections.jpg", sep=""))
dev.off()

#Save file
save(projections_fp, file = paste(getwd(),"/Data/FantasyPros-Projections.RData", sep=""))
write.csv(projections_fp, file=paste(getwd(),"/Data/FantasyPros-Projections.csv", sep=""), row.names=FALSE)

save(projections_fp, file = paste(getwd(),"/Data/Historical Projections/FantasyPros-Projections-2014.RData", sep=""))
write.csv(projections_fp, file=paste(getwd(),"/Data/Historical Projections/FantasyPros-Projections-2014.csv", sep=""), row.names=FALSE)
