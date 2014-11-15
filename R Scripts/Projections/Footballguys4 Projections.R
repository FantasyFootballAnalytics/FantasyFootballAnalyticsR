###########################
# File: Footballguys Projections.R
# Description: Downloads Fantasy Football Projections from Footballguys.com
# Date: 7/13/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library(httr)
library(XML)
library(stringr)
library(ggplot2)
library(plyr)

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Suffix
suffix <- "fbg4"

#Download fantasy football projections from Footballguys.com (password-protected)
qbURL_fbg4 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projector=50"
rbURL_fbg4 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=rb&projector=50&profile=0"
wrURL_fbg4 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=wr&projector=50&profile=0"
teURL_fbg4 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=te&projector=50&profile=0"

handle_fbg <- handle("http://subscribers.footballguys.com") 
path_fbg   <- "amember/login.php"

qbLogin_fbg4 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = qbURL_fbg4
)

rbLogin_fbg4 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = rbURL_fbg4
)

wrLogin_fbg4 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = wrURL_fbg4
)

teLogin_fbg4 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = teURL_fbg4
)

qbContent_fbg4 <- POST(handle = handle_fbg, path = path_fbg, body = qbLogin_fbg4)
rbContent_fbg4 <- POST(handle = handle_fbg, path = path_fbg, body = rbLogin_fbg4)
wrContent_fbg4 <- POST(handle = handle_fbg, path = path_fbg, body = wrLogin_fbg4)
teContent_fbg4 <- POST(handle = handle_fbg, path = path_fbg, body = teLogin_fbg4)

qb_fbg4 <- readHTMLTable(content(qbContent_fbg4), stringsAsFactors = FALSE)$'NULL'
rb_fbg4 <- readHTMLTable(content(rbContent_fbg4), stringsAsFactors = FALSE)$'NULL'
wr_fbg4 <- readHTMLTable(content(wrContent_fbg4), stringsAsFactors = FALSE)$'NULL'
te_fbg4 <- readHTMLTable(content(teContent_fbg4), stringsAsFactors = FALSE)$'NULL'

#Add variable names for each object
names(qb_fbg4) <- c("rank_fbg4","name_fbg4","teamBye_fbg4","age_fbg4","exp_fbg4","passComp_fbg4","passAtt_fbg4","passCompPct_fbg4","passYds_fbg4","passYdsPerAtt_fbg4","passTds_fbg4","passInt_fbg4","rushAtt_fbg4","rushYds_fbg4","rushTds_fbg4","pts_fbg4")
names(rb_fbg4) <- c("rank_fbg4","name_fbg4","teamBye_fbg4","age_fbg4","exp_fbg4","rushAtt_fbg4","rushYds_fbg4","rushYdsPerAtt_fbg4","rushTds_fbg4","rec_fbg4","recYds_fbg4","recTds_fbg4","pts_fbg4")
names(wr_fbg4) <- c("rank_fbg4","name_fbg4","teamBye_fbg4","age_fbg4","exp_fbg4","rushAtt_fbg4","rushYds_fbg4","rushTds_fbg4","rec_fbg4","recYds_fbg4","recYdsPerRec_fbg4","recTds_fbg4","pts_fbg4")
names(te_fbg4) <- c("rank_fbg4","name_fbg4","teamBye_fbg4","age_fbg4","exp_fbg4","rec_fbg4","recYds_fbg4","recYdsPerRec_fbg4","recTds_fbg4","pts_fbg4")

#Add variable for player position
qb_fbg4$pos <- as.factor("QB")
rb_fbg4$pos <- as.factor("RB")
wr_fbg4$pos <- as.factor("WR")
te_fbg4$pos <- as.factor("TE")

#Merge players across positions
projections_fbg4 <- rbind.fill(qb_fbg4, rb_fbg4, wr_fbg4, te_fbg4)

#Add variables from other projection sources
projections_fbg4$returnTds_fbg4 <- NA
projections_fbg4$twoPts_fbg4 <- NA
projections_fbg4$fumbles_fbg4 <- NA

#Convert variables from character strings to numeric
projections_fbg4[,c("rank_fbg4","age_fbg4","exp_fbg4","passComp_fbg4","passAtt_fbg4","passCompPct_fbg4","passYds_fbg4","passYdsPerAtt_fbg4","passTds_fbg4","passInt_fbg4","rushAtt_fbg4","rushYds_fbg4","rushYdsPerAtt_fbg4","rushTds_fbg4","rec_fbg4","recYds_fbg4","recTds_fbg4","recYdsPerRec_fbg4","returnTds_fbg4","twoPts_fbg4","fumbles_fbg4","pts_fbg4")] <-
  convert.magic(projections_fbg4[,c("rank_fbg4","age_fbg4","exp_fbg4","passComp_fbg4","passAtt_fbg4","passCompPct_fbg4","passYds_fbg4","passYdsPerAtt_fbg4","passTds_fbg4","passInt_fbg4","rushAtt_fbg4","rushYds_fbg4","rushYdsPerAtt_fbg4","rushTds_fbg4","rec_fbg4","recYds_fbg4","recTds_fbg4","recYdsPerRec_fbg4","returnTds_fbg4","twoPts_fbg4","fumbles_fbg4","pts_fbg4")], "numeric")

#Team
projections_fbg4$team_fbg4 <- str_trim(sapply(str_split(projections_fbg4$teamBye_fbg4, "\\/"), "[", 1))

#Name for merging
projections_fbg4$name <- nameMerge(projections_fbg4$name_fbg4)

#Remove duplicate cases
projections_fbg4[projections_fbg4$name %in% projections_fbg4[duplicated(projections_fbg4$name),"name"],]

#Same name, different player
projections_fbg4 <- projections_fbg4[-which(projections_fbg4$name=="ALEXSMITH" & projections_fbg4$team_fbg4=="CIN"),]
projections_fbg4 <- projections_fbg4[-which(projections_fbg4$name=="RYANGRIFFIN" & projections_fbg4$team_fbg4=="NO"),]

#Same player, different position

#Rename players
projections_fbg4[projections_fbg4$name=="TIMOTHYWRIGHT", "name"] <- "TIMWRIGHT"
projections_fbg4[projections_fbg4$name=="BENWATSON", "name"] <- "BENJAMINWATSON"

#Calculate overall rank
projections_fbg4$overallRank_fbg4 <- rank(-projections_fbg4$pts_fbg4, ties.method="min")

#Calculate Position Rank
projections_fbg4$positionRank_fbg4 <- NA
projections_fbg4[which(projections_fbg4$pos == "QB"), "positionRank_fbg4"] <- rank(-projections_fbg4[which(projections_fbg4$pos == "QB"), "pts_fbg4"], ties.method="min")
projections_fbg4[which(projections_fbg4$pos == "RB"), "positionRank_fbg4"] <- rank(-projections_fbg4[which(projections_fbg4$pos == "RB"), "pts_fbg4"], ties.method="min")
projections_fbg4[which(projections_fbg4$pos == "WR"), "positionRank_fbg4"] <- rank(-projections_fbg4[which(projections_fbg4$pos == "WR"), "pts_fbg4"], ties.method="min")
projections_fbg4[which(projections_fbg4$pos == "TE"), "positionRank_fbg4"] <- rank(-projections_fbg4[which(projections_fbg4$pos == "TE"), "pts_fbg4"], ties.method="min")

#Order variables in data set
projections_fbg4 <- projections_fbg4[,c(prefix, paste(varNames, suffix, sep="_"))]

#Order players by overall rank
projections_fbg4 <- projections_fbg4[order(projections_fbg4$overallRank_fbg4),]
row.names(projections_fbg4) <- 1:dim(projections_fbg4)[1]

#Density Plot
ggplot(projections_fbg4, aes(x=pts_fbg4)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Footballguys4 Projected Points")
ggsave(paste(getwd(),"/Figures/Footballguys4 projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fbg4, file = paste(getwd(),"/Data/Footballguys4-Projections.RData", sep=""))
write.csv(projections_fbg4, file=paste(getwd(),"/Data/Footballguys4-Projections.csv", sep=""), row.names=FALSE)

save(projections_fbg4, file = paste(getwd(),"/Data/Historical Projections/Footballguys4-Projections-2015.RData", sep=""))
write.csv(projections_fbg4, file=paste(getwd(),"/Data/Historical Projections/Footballguys4-Projections-2015.csv", sep=""), row.names=FALSE)
