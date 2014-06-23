###########################
# File: CBS2 Projections.R
# Description: Downloads Fantasy Football Projections from cbssports.com
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

#Download fantasy football projections from cbssports.com
qb_cbs2 <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/QB/season/dave_richard/standard", stringsAsFactors = FALSE)[7]$'NULL'
rb1_cbs2 <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/season/dave_richard/standard", stringsAsFactors = FALSE)[7]$'NULL'
rb2_cbs2 <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/season/dave_richard/standard?&start_row=51", stringsAsFactors = FALSE)[7]$'NULL'
rb3_cbs2 <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/season/dave_richard/standard?&start_row=101", stringsAsFactors = FALSE)[7]$'NULL'
wr1_cbs2 <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/season/dave_richard/standard", stringsAsFactors = FALSE)[7]$'NULL'
wr2_cbs2 <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/season/dave_richard/standard?&start_row=51", stringsAsFactors = FALSE)[7]$'NULL'
wr3_cbs2 <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/season/dave_richard/standard?&start_row=101", stringsAsFactors = FALSE)[7]$'NULL'
te_cbs2 <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/TE/season/dave_richard/standard", stringsAsFactors = FALSE)[7]$'NULL'

#Add variable names for each object
names(qb_cbs2) <- c("player_cbs2","passAtt_cbs2","passComp_cbs2","passYds_cbs2","passTds_cbs2","passInt_cbs2","passCompPct_cbs2","passYdsPerAtt_cbs2","rushAtt_cbs2","rushYds_cbs2","rushYdsPerAtt_cbs2","rushTds_cbs2","fumbles_cbs2","pts_cbs2")
names(rb1_cbs2) <- names(rb2_cbs2) <- names(rb3_cbs2) <- c("player_cbs2","rushAtt_cbs2","rushYds_cbs2","rushYdsPerAtt_cbs2","rushTds_cbs2","rec_cbs2","recYds_cbs2","recYdsPerRec_cbs2","recTds_cbs2","fumbles_cbs2","pts_cbs2")
names(wr1_cbs2) <- names(wr2_cbs2) <- names(wr3_cbs2) <- c("player_cbs2","rec_cbs2","recYds_cbs2","recYdsPerRec_cbs2","recTds_cbs2","fumbles_cbs2","pts_cbs2")
names(te_cbs2) <- c("player_cbs2","rec_cbs2","recYds_cbs2","recYdsPerRec_cbs2","recTds_cbs2","fumbles_cbs2","pts_cbs2")

#Trim dimensions
qb_cbs2 <- qb_cbs2[3:(dim(qb_cbs2)[1]-1),]
rb1_cbs2 <- rb1_cbs2[3:(dim(rb1_cbs2)[1]-1),]
rb2_cbs2 <- rb2_cbs2[3:(dim(rb2_cbs2)[1]-1),]
rb3_cbs2 <- rb3_cbs2[3:(dim(rb3_cbs2)[1]-1),]
wr1_cbs2 <- wr1_cbs2[3:(dim(wr1_cbs2)[1]-1),]
wr2_cbs2 <- wr2_cbs2[3:(dim(wr2_cbs2)[1]-1),]
wr3_cbs2 <- wr3_cbs2[3:(dim(wr3_cbs2)[1]-1),]
te_cbs2 <- te_cbs2[3:(dim(te_cbs2)[1]-1),]

#Merge within position
rb_cbs2 <- rbind(rb1_cbs2,rb2_cbs2,rb3_cbs2)
wr_cbs2 <- rbind(wr1_cbs2,wr2_cbs2,wr3_cbs2)

#Add variable for player position
qb_cbs2$pos <- as.factor("QB")
rb_cbs2$pos <- as.factor("RB")
wr_cbs2$pos <- as.factor("WR")
te_cbs2$pos <- as.factor("TE")

#Merge across positions
projections_cbs2 <- rbind.fill(qb_cbs2, rb_cbs2, wr_cbs2, te_cbs2)

#Add variables from other projection sources
projections_cbs2$twoPts_cbs2 <- NA

#Convert variables from character strings to numeric
projections_cbs2[,c("twoPts_cbs2","fumbles_cbs2","pts_cbs2",
                    "rec_cbs2","recYds_cbs2","recYdsPerRec_cbs2","recTds_cbs2",
                    "rushAtt_cbs2","rushYds_cbs2","rushYdsPerAtt_cbs2","rushTds_cbs2",
                    "passAtt_cbs2","passComp_cbs2","passYds_cbs2","passTds_cbs2","passInt_cbs2","passCompPct_cbs2","passYdsPerAtt_cbs2")] <- convert.magic(projections_cbs2[,c("twoPts_cbs2","fumbles_cbs2","pts_cbs2",
                                                                                                                                                                               "rec_cbs2","recYds_cbs2","recYdsPerRec_cbs2","recTds_cbs2",
                                                                                                                                                                               "rushAtt_cbs2","rushYds_cbs2","rushYdsPerAtt_cbs2","rushTds_cbs2",
                                                                                                                                                                               "passAtt_cbs2","passComp_cbs2","passYds_cbs2","passTds_cbs2","passInt_cbs2","passCompPct_cbs2","passYdsPerAtt_cbs2")], "numeric")

#Player names
projections_cbs2$name_cbs2 <- str_sub(projections_cbs2$player, end=str_locate(string=projections_cbs2$player, ',')[,1]-1)
projections_cbs2$name <- nameMerge(projections_cbs2$name_cbs2)

#Remove Duplicates
projections_cbs2[projections_cbs2$name %in% projections_cbs2[duplicated(projections_cbs2$name),"name"],]
#projections_cbs2[projections_cbs2$name_cbs2 == "James Casey","pos"] <- "TE"

#Rename Players
#projections_cbs2[projections_cbs2$name_cbs2=="EJ Manuel", "name_cbs2"] <- "E.J. Manuel"

#Player teams
projections_cbs2$team_cbs2 <- str_trim(str_sub(projections_cbs2$player, start= -3))

#Calculate overall rank
projections_cbs2$overallRank_cbs2 <- rank(-projections_cbs2$pts_cbs2, ties.method="min")

#Calculate Position Rank
projections_cbs2$positionRank_cbs2 <- NA
projections_cbs2[which(projections_cbs2$pos == "QB"), "positionRank_cbs2"] <- rank(-projections_cbs2[which(projections_cbs2$pos == "QB"), "pts_cbs2"], ties.method="min")
projections_cbs2[which(projections_cbs2$pos == "RB"), "positionRank_cbs2"] <- rank(-projections_cbs2[which(projections_cbs2$pos == "RB"), "pts_cbs2"], ties.method="min")
projections_cbs2[which(projections_cbs2$pos == "WR"), "positionRank_cbs2"] <- rank(-projections_cbs2[which(projections_cbs2$pos == "WR"), "pts_cbs2"], ties.method="min")
projections_cbs2[which(projections_cbs2$pos == "TE"), "positionRank_cbs2"] <- rank(-projections_cbs2[which(projections_cbs2$pos == "TE"), "pts_cbs2"], ties.method="min")

#Order variables in data set
projections_cbs2 <- projections_cbs2[,c("name","name_cbs2","pos","team_cbs2","positionRank_cbs2","overallRank_cbs2",
                                        "passAtt_cbs2","passComp_cbs2","passYds_cbs2","passTds_cbs2","passInt_cbs2","passCompPct_cbs2","passYdsPerAtt_cbs2",
                                        "rushAtt_cbs2","rushYds_cbs2","rushYdsPerAtt_cbs2","rushTds_cbs2",
                                        "rec_cbs2","recYds_cbs2","recYdsPerRec_cbs2","recTds_cbs2","twoPts_cbs2","fumbles_cbs2","pts_cbs2")]

#Order players by overall rank
projections_cbs2 <- projections_cbs2[order(projections_cbs2$overallRank_cbs2),]
row.names(projections_cbs2) <- 1:dim(projections_cbs2)[1]

#Density Plot
ggplot(projections_cbs2, aes(x=pts_cbs2)) + geom_density(fill="red", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of CBS2 Projected Points from")
ggsave(paste(getwd(),"/Figures/CBS2 projections.jpg", sep=""))
dev.off()

#Save file
save(projections_cbs2, file = paste(getwd(),"/Data/CBS2-Projections.RData", sep=""))
write.csv(projections_cbs2, file=paste(getwd(),"/Data/CBS2-Projections.csv", sep=""), row.names=FALSE)

save(projections_cbs2, file = paste(getwd(),"/Data/Historical Projections/CBS2-Projections-2014.RData", sep=""))
write.csv(projections_cbs2, file=paste(getwd(),"/Data/Historical Projections/CBS2-Projections-2014.csv", sep=""), row.names=FALSE)
