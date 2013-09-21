###########################
# File: FantasyPros Projections.R
# Description: Downloads Fantasy Football Projections from FantasyPros.com
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

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
projections_fp <- merge(qb_fp,rb_fp, all=TRUE)
projections_fp <- merge(projections_fp,wr_fp, all=TRUE)
projections_fp <- merge(projections_fp,te_fp, all=TRUE)

#Convert variables from character strings to numeric
projections_fp$passAtt_fp <- as.numeric(projections_fp$passAtt_fp)
projections_fp$passComp_fp <- as.numeric(projections_fp$passComp_fp)
projections_fp$passYds_fp <- as.numeric(gsub(",", "", projections_fp$passYds_fp, fixed = TRUE))
projections_fp$passTds_fp <- as.numeric(projections_fp$passTds_fp)
projections_fp$passInt_fp <- as.numeric(projections_fp$passInt_fp)
projections_fp$rushAtt_fp <- as.numeric(projections_fp$rushAtt_fp)
projections_fp$rushYds_fp <- as.numeric(gsub(",", "", projections_fp$rushYds_fp, fixed = TRUE))
projections_fp$rushTds_fp <- as.numeric(projections_fp$rushTds_fp)
projections_fp$rec_fp <- as.numeric(projections_fp$rec_fp)
projections_fp$recYds_fp <- as.numeric(gsub(",", "", projections_fp$recYds_fp, fixed = TRUE))
projections_fp$recTds_fp <- as.numeric(projections_fp$recTds_fp)
projections_fp$fumbles_fp <- as.numeric(projections_fp$fumbles_fp)
projections_fp$pts_fp <- as.numeric(projections_fp$pts_fp)

#Add variables from other projection sources
projections_fp$twoPts_fp <- NA

#Player names
projections_fp <- adply(projections_fp, 1, function(x) {
  if(is.na(str_locate(string=x$player_fp, '\\(')[,1]) == TRUE){
    x$name <- x$player_fp
  } else{
    x$name <- str_sub(x$player_fp, end=str_locate(string=x$player_fp, '\\(')[,1]-2)
  }
  x
}         
)

#projections_fp[grep("Beanie", projections_fp$name),"name"] <- "Beanie Wells"
#projections_fp[is.na(projections_fp$name==TRUE),"name"] <- projections_fp[is.na(projections_fp$name==TRUE),"player_fp"]

#Player teams
#projections_fp$team_fp <- str_sub(projections_fp$player_fp, start=str_locate(string=projections_fp$player_fp, '\\(')[,1]+1, end = str_locate(string=projections_fp$player_fp, ',')[,1]-1)
projections_fp$team_fp <- str_sub(projections_fp$player_fp, start=str_locate(string=projections_fp$player_fp, '\\(')[,1]+1, end = str_locate(string=projections_fp$player_fp, '\\)')[,1]-1)

#Remove duplicate cases
#projections_fp[duplicated(projections_fp$name),]
projections_fp[projections_fp$name %in% projections_fp[duplicated(projections_fp$name),"name"],]

#projections_fp[projections_fp$name=="Alex Smith",][1,] <- NA
projections_fp <- projections_fp[-which(projections_fp$name=="Alex Smith" & projections_fp$pos=="TE"),]
projections_fp <- projections_fp[-which(projections_fp$name=="Charles Clay" & projections_fp$pos=="RB"),]
#projections_fp <- projections_fp[-which(projections_fp$name=="Chris Givens" & projections_fp$team_fp=="NO"),]
projections_fp <- projections_fp[-which(projections_fp$name=="Clay Harbor" & projections_fp$pos=="WR"),]
projections_fp <- projections_fp[-which(projections_fp$name=="David Johnson" & projections_fp$pos=="TE"),]
projections_fp <- projections_fp[-which(projections_fp$name=="Dexter McCluster" & projections_fp$pos=="RB"),]
projections_fp <- projections_fp[-which(projections_fp$name=="Dorin Dickerson" & projections_fp$pos=="WR"),]
projections_fp <- projections_fp[-which(projections_fp$name=="Dorin Dickerson" & projections_fp$pos=="RB"),]
projections_fp <- projections_fp[-which(projections_fp$name=="Evan Rodriguez" & projections_fp$pos=="RB"),]
projections_fp <- projections_fp[-which(projections_fp$name=="Jamie McCoy" & projections_fp$pos=="RB"),]
#projections_fp <- projections_fp[-which(projections_fp$name=="James Casey" & projections_fp$pos=="RB"),]
#projections_fp <- projections_fp[-which(projections_fp$name=="Niles Paul" & projections_fp$pos=="WR"),]
projections_fp <- projections_fp[-which(projections_fp$name=="Steve Smith" & is.na(projections_fp$team_fp)),]
projections_fp <- projections_fp[-which(projections_fp$name=="Zach Miller" & projections_fp$team_fp=="TB"),]

#Remove rows with all NAs
projections_fp <- projections_fp[apply(projections_fp, 1, function(x) any(!is.na(x))),]

#Rename Players
projections_fp[projections_fp$name=="Christopher Ivory", "name"] <- "Chris Ivory"
projections_fp[projections_fp$name=="Ty Hilton", "name"] <- "T.Y. Hilton"
projections_fp[projections_fp$name=="Robert Housler", "name"] <- "Rob Housler"
projections_fp[projections_fp$name=="Reuben Randle", "name"] <- "Rueben Randle"
projections_fp[projections_fp$name=="Joseph Morgan", "name"] <- "Joe Morgan"

#Calculate overall rank
projections_fp$overallRank_fp <- rank(-projections_fp$pts_fp, ties.method="min")

#Order variables in data set
projections_fp <- projections_fp[,c("name","pos","team_fp","overallRank_fp",
                                        "passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp",
                                        "rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","twoPts_fp","fumbles_fp","pts_fp")]

#Order players by overall rank
projections_fp <- projections_fp[order(projections_fp$overallRank_fp),]
row.names(projections_fp) <- 1:dim(projections_fp)[1]

#Density Plot
ggplot(projections_fp, aes(x=pts_fp)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FantasyPros Projected Points from 2013")
ggsave(paste(getwd(),"/Figures/FantasyPros projections 2013.jpg", sep=""))
dev.off()

#Save file
save(projections_fp, file = paste(getwd(),"/Data/FantasyPros-Projections-2013.RData", sep=""))
write.csv(projections_fp, file=paste(getwd(),"/Data/CSV/FantasyPros-Projections-2013.csv", sep=""), row.names=FALSE)
