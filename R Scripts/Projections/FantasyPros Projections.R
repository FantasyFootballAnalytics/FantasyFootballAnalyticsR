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

#Suffix
suffix <- "fp"

#Download fantasy football projections from FantasyPros.com
qb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/qb.php", stringsAsFactors = FALSE)$data
rb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/rb.php", stringsAsFactors = FALSE)$data
wr_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/wr.php", stringsAsFactors = FALSE)$data
te_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/te.php", stringsAsFactors = FALSE)$data
k_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/k.php", stringsAsFactors = FALSE)$data

#Add variable names for each object
names(qb_fp) <- c("player_fp","passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","fumbles_fp","pts_fp")
names(rb_fp) <- c("player_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","fumbles_fp","pts_fp")
names(wr_fp) <- c("player_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","fumbles_fp","pts_fp")
names(te_fp) <- c("player_fp","rec_fp","recYds_fp","recTds_fp","fumbles_fp","pts_fp")
names(k_fp) <- c("player_fp","fg_fp","fga_fp","xp_fp","pts_fp")

#Add variable for player position
qb_fp$pos <- as.factor("QB")
rb_fp$pos <- as.factor("RB")
wr_fp$pos <- as.factor("WR")
te_fp$pos <- as.factor("TE")
k_fp$pos <- as.factor("K")

#Merge players across positions
projections_fp <- rbind.fill(qb_fp, rb_fp, wr_fp, te_fp, k_fp)

#Add variables from other projection sources
projections_fp$returnTds_fp <- NA
projections_fp$twoPts_fp <- NA

#Remove special characters(commas)
projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","returnTds_fp","twoPts_fp","fumbles_fp","pts_fp",
                  "fg_fp","fga_fp","xp_fp")] <-
  apply(projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","returnTds_fp","twoPts_fp","fumbles_fp","pts_fp",
                          "fg_fp","fga_fp","xp_fp")], 2, function(x) gsub("\\,", "", x))

#Convert variables from character strings to numeric
projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","returnTds_fp","twoPts_fp","fumbles_fp","pts_fp",
                  "fg_fp","fga_fp","xp_fp")] <-
  convert.magic(projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","returnTds_fp","twoPts_fp","fumbles_fp","pts_fp",
                                  "fg_fp","fga_fp","xp_fp")], "numeric")

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
if(length(which(projections_fp$name_fp == "")) > 0){
  projections_fp <- projections_fp[-which(projections_fp$name_fp == ""),]
}

#Remove duplicate cases
projections_fp[projections_fp$name %in% projections_fp[duplicated(projections_fp$name),"name"],]

#Same name, different player
projections_fp <- projections_fp[-which(projections_fp$name=="ALEXSMITH" & projections_fp$team_fp=="CIN"),]
projections_fp <- projections_fp[-which(projections_fp$name=="RYANGRIFFIN" & projections_fp$team_fp=="HOU"),]

#Same player, different position
dropNames <- c("DEXTERMCCLUSTER","DENARDROBINSON","JAMESCASEY","CORYHARKEY","RYANHEWITT","NILESPAUL")
dropVariables <- c("pos","pos","pos","pos","pos","pos")
dropLabels <- c("WR","RB","RB","RB","RB","TE")

projections_fp2 <- ddply(projections_fp, .(name), numcolwise(mean), na.rm=TRUE)

for(i in 1:length(dropNames)){
  if(dim(projections_fp[-which(projections_fp[,"name"] == dropNames[i] & projections_fp[,dropVariables[i]] == dropLabels[i]),])[1] > 0){
    projections_fp <- projections_fp[-which(projections_fp[,"name"] == dropNames[i] & projections_fp[,dropVariables[i]] == dropLabels[i]),]
  }
}

projections_fp <- merge(projections_fp2, projections_fp[,c("name","name_fp","player_fp","pos","team_fp")], by="name")

#Rename Players
if(length(projections_fp[projections_fp$name == "CHRISTOPHERIVORY", "name"]) > 0){projections_fp[projections_fp$name == "CHRISTOPHERIVORY", "name"] <- "CHRISIVORY"}
if(length(projections_fp[projections_fp$name == "DOMANIQUEDAVIS", "name"]) > 0){projections_fp[projections_fp$name == "DOMANIQUEDAVIS", "name"] <- "DOMINIQUEDAVIS"}

#Calculate overall rank
projections_fp$overallRank_fp <- rank(-projections_fp$pts_fp, ties.method="min")

#Calculate Position Rank
projections_fp$positionRank_fp <- NA
projections_fp[which(projections_fp$pos == "QB"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "QB"), "pts_fp"], ties.method="min")
projections_fp[which(projections_fp$pos == "RB"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "RB"), "pts_fp"], ties.method="min")
projections_fp[which(projections_fp$pos == "WR"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "WR"), "pts_fp"], ties.method="min")
projections_fp[which(projections_fp$pos == "TE"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "TE"), "pts_fp"], ties.method="min")
projections_fp[which(projections_fp$pos == "K"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "K"), "pts_fp"], ties.method="min")

#Order variables in data set
projections_fp <- projections_fp[,c(prefix, paste(varNames, suffix, sep="_"))]

#Order players by overall rank
projections_fp <- projections_fp[order(projections_fp$overallRank_fp),]
row.names(projections_fp) <- 1:dim(projections_fp)[1]

#Density Plot
ggplot(projections_fp, aes(x=pts_fp)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FantasyPros Projected Points")
ggsave(paste(getwd(),"/Figures/FantasyPros projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fp, file = paste(getwd(), "/Data/FantasyPros-Projections.RData", sep=""))
write.csv(projections_fp, file=paste(getwd(), "/Data/FantasyPros-Projections.csv", sep=""), row.names=FALSE)

save(projections_fp, file = paste(getwd(), "/Data/Historical Projections/FantasyPros-Projections-", season, ".RData", sep=""))
write.csv(projections_fp, file=paste(getwd(), "/Data/Historical Projections/FantasyPros-Projections-", season, ".csv", sep=""), row.names=FALSE)
