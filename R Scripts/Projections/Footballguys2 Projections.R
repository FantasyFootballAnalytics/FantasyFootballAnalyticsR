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
suffix <- "fbg2"

#Download fantasy football projections from Footballguys.com (password-protected)
qbURL_fbg2 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projector=41"
rbURL_fbg2 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=rb&projector=41&profile=0"
wrURL_fbg2 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=wr&projector=41&profile=0"
teURL_fbg2 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=te&projector=41&profile=0"

handle_fbg <- handle("http://subscribers.footballguys.com") 
path_fbg   <- "amember/login.php"

qbLogin_fbg2 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = qbURL_fbg2
)

rbLogin_fbg2 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = rbURL_fbg2
)

wrLogin_fbg2 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = wrURL_fbg2
)

teLogin_fbg2 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = teURL_fbg2
)

qbContent_fbg2 <- POST(handle = handle_fbg, path = path_fbg, body = qbLogin_fbg2)
rbContent_fbg2 <- POST(handle = handle_fbg, path = path_fbg, body = rbLogin_fbg2)
wrContent_fbg2 <- POST(handle = handle_fbg, path = path_fbg, body = wrLogin_fbg2)
teContent_fbg2 <- POST(handle = handle_fbg, path = path_fbg, body = teLogin_fbg2)

qb_fbg2 <- readHTMLTable(content(qbContent_fbg2), stringsAsFactors = FALSE)$'NULL'
rb_fbg2 <- readHTMLTable(content(rbContent_fbg2), stringsAsFactors = FALSE)$'NULL'
wr_fbg2 <- readHTMLTable(content(wrContent_fbg2), stringsAsFactors = FALSE)$'NULL'
te_fbg2 <- readHTMLTable(content(teContent_fbg2), stringsAsFactors = FALSE)$'NULL'

#Add variable names for each object
names(qb_fbg2) <- c("rank_fbg2","name_fbg2","teamBye_fbg2","age_fbg2","exp_fbg2","passComp_fbg2","passAtt_fbg2","passCompPct_fbg2","passYds_fbg2","passYdsPerAtt_fbg2","passTds_fbg2","passInt_fbg2","rushAtt_fbg2","rushYds_fbg2","rushTds_fbg2","pts_fbg2")
names(rb_fbg2) <- c("rank_fbg2","name_fbg2","teamBye_fbg2","age_fbg2","exp_fbg2","rushAtt_fbg2","rushYds_fbg2","rushYdsPerAtt_fbg2","rushTds_fbg2","rec_fbg2","recYds_fbg2","recTds_fbg2","pts_fbg2")
names(wr_fbg2) <- c("rank_fbg2","name_fbg2","teamBye_fbg2","age_fbg2","exp_fbg2","rushAtt_fbg2","rushYds_fbg2","rushTds_fbg2","rec_fbg2","recYds_fbg2","recYdsPerRec_fbg2","recTds_fbg2","pts_fbg2")
names(te_fbg2) <- c("rank_fbg2","name_fbg2","teamBye_fbg2","age_fbg2","exp_fbg2","rec_fbg2","recYds_fbg2","recYdsPerRec_fbg2","recTds_fbg2","pts_fbg2")

#Add variable for player position
qb_fbg2$pos <- as.factor("QB")
rb_fbg2$pos <- as.factor("RB")
wr_fbg2$pos <- as.factor("WR")
te_fbg2$pos <- as.factor("TE")

#Merge players across positions
projections_fbg2 <- rbind.fill(qb_fbg2, rb_fbg2, wr_fbg2, te_fbg2)

#Add variables from other projection sources
projections_fbg2$returnTds_fbg2 <- NA
projections_fbg2$twoPts_fbg2 <- NA
projections_fbg2$fumbles_fbg2 <- NA

#Convert variables from character strings to numeric
projections_fbg2[,c("rank_fbg2","age_fbg2","exp_fbg2","passComp_fbg2","passAtt_fbg2","passCompPct_fbg2","passYds_fbg2","passYdsPerAtt_fbg2","passTds_fbg2","passInt_fbg2","rushAtt_fbg2","rushYds_fbg2","rushYdsPerAtt_fbg2","rushTds_fbg2","rec_fbg2","recYds_fbg2","recTds_fbg2","recYdsPerRec_fbg2","returnTds_fbg2","twoPts_fbg2","fumbles_fbg2","pts_fbg2")] <-
  convert.magic(projections_fbg2[,c("rank_fbg2","age_fbg2","exp_fbg2","passComp_fbg2","passAtt_fbg2","passCompPct_fbg2","passYds_fbg2","passYdsPerAtt_fbg2","passTds_fbg2","passInt_fbg2","rushAtt_fbg2","rushYds_fbg2","rushYdsPerAtt_fbg2","rushTds_fbg2","rec_fbg2","recYds_fbg2","recTds_fbg2","recYdsPerRec_fbg2","returnTds_fbg2","twoPts_fbg2","fumbles_fbg2","pts_fbg2")], "numeric")

#Team
projections_fbg2$team_fbg2 <- str_trim(sapply(str_split(projections_fbg2$teamBye_fbg2, "\\/"), "[", 1))

#Name for merging
projections_fbg2$name <- nameMerge(projections_fbg2$name_fbg2)

#Remove duplicate cases
projections_fbg2[projections_fbg2$name %in% projections_fbg2[duplicated(projections_fbg2$name),"name"],]

#Same name, different player
projections_fbg2 <- projections_fbg2[-which(projections_fbg2$name=="ALEXSMITH" & projections_fbg2$team_fbg2=="CIN"),]
projections_fbg2 <- projections_fbg2[-which(projections_fbg2$name=="RYANGRIFFIN" & projections_fbg2$team_fbg2=="NO"),]
projections_fbg2 <- projections_fbg2[-which(projections_fbg2$name=="ZACHMILLER" & projections_fbg2$team_fbg2=="CHI"),]

#Same player, different position

#Rename players
projections_fbg2[projections_fbg2$name=="TIMOTHYWRIGHT", "name"] <- "TIMWRIGHT"
projections_fbg2[projections_fbg2$name=="BENWATSON", "name"] <- "BENJAMINWATSON"

#Calculate overall rank
projections_fbg2$overallRank_fbg2 <- rank(-projections_fbg2$pts_fbg2, ties.method="min")

#Calculate Position Rank
projections_fbg2$positionRank_fbg2 <- NA
projections_fbg2[which(projections_fbg2$pos == "QB"), "positionRank_fbg2"] <- rank(-projections_fbg2[which(projections_fbg2$pos == "QB"), "pts_fbg2"], ties.method="min")
projections_fbg2[which(projections_fbg2$pos == "RB"), "positionRank_fbg2"] <- rank(-projections_fbg2[which(projections_fbg2$pos == "RB"), "pts_fbg2"], ties.method="min")
projections_fbg2[which(projections_fbg2$pos == "WR"), "positionRank_fbg2"] <- rank(-projections_fbg2[which(projections_fbg2$pos == "WR"), "pts_fbg2"], ties.method="min")
projections_fbg2[which(projections_fbg2$pos == "TE"), "positionRank_fbg2"] <- rank(-projections_fbg2[which(projections_fbg2$pos == "TE"), "pts_fbg2"], ties.method="min")

#Order variables in data set
projections_fbg2 <- projections_fbg2[,c(prefix, paste(varNames, suffix, sep="_"))]

#Order players by overall rank
projections_fbg2 <- projections_fbg2[order(projections_fbg2$overallRank_fbg2),]
row.names(projections_fbg2) <- 1:dim(projections_fbg2)[1]

#Density Plot
ggplot(projections_fbg2, aes(x=pts_fbg2)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Footballguys2 Projected Points")
ggsave(paste(getwd(),"/Figures/Footballguys2 projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fbg2, file = paste(getwd(),"/Data/Footballguys2-Projections.RData", sep=""))
write.csv(projections_fbg2, file=paste(getwd(),"/Data/Footballguys2-Projections.csv", sep=""), row.names=FALSE)

save(projections_fbg2, file = paste(getwd(),"/Data/Historical Projections/Footballguys2-Projections-2014.RData", sep=""))
write.csv(projections_fbg2, file=paste(getwd(),"/Data/Historical Projections/Footballguys2-Projections-2014.csv", sep=""), row.names=FALSE)
