###########################
# File: Footballguys Projections.R
# Description: Downloads Fantasy Football Projections from Footballguys1.com
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

#Download fantasy football projections from Footballguys.com (password-protected)
qbURL_fbg1 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projector=2"
rbURL_fbg1 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=rb&projector=2&profile=0"
wrURL_fbg1 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=wr&projector=2&profile=0"
teURL_fbg1 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=te&projector=2&profile=0"

handle_fbg <- handle("http://subscribers.footballguys.com") 
path_fbg   <- "amember/login.php"

qbLogin_fbg1 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = qbURL_fbg1
)

rbLogin_fbg1 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = rbURL_fbg1
)

wrLogin_fbg1 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = wrURL_fbg1
)

teLogin_fbg1 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = teURL_fbg1
)

qbContent_fbg1 <- POST(handle = handle_fbg, path = path_fbg, body = qbLogin_fbg1)
rbContent_fbg1 <- POST(handle = handle_fbg, path = path_fbg, body = rbLogin_fbg1)
wrContent_fbg1 <- POST(handle = handle_fbg, path = path_fbg, body = wrLogin_fbg1)
teContent_fbg1 <- POST(handle = handle_fbg, path = path_fbg, body = teLogin_fbg1)

qb_fbg1 <- readHTMLTable(content(qbContent_fbg1), stringsAsFactors = FALSE)$'NULL'
rb_fbg1 <- readHTMLTable(content(rbContent_fbg1), stringsAsFactors = FALSE)$'NULL'
wr_fbg1 <- readHTMLTable(content(wrContent_fbg1), stringsAsFactors = FALSE)$'NULL'
te_fbg1 <- readHTMLTable(content(teContent_fbg1), stringsAsFactors = FALSE)$'NULL'

#Add variable names for each object
names(qb_fbg1) <- c("rank_fbg1","name_fbg1","teamBye_fbg1","age_fbg1","exp_fbg1","passComp_fbg1","passAtt_fbg1","passCompPct_fbg1","passYds_fbg1","passYdsPerAtt_fbg1","passTds_fbg1","passInt_fbg1","rushAtt_fbg1","rushYds_fbg1","rushTds_fbg1","pts_fbg1")
names(rb_fbg1) <- c("rank_fbg1","name_fbg1","teamBye_fbg1","age_fbg1","exp_fbg1","rushAtt_fbg1","rushYds_fbg1","rushYdsPerAtt_fbg1","rushTds_fbg1","rec_fbg1","recYds_fbg1","recTds_fbg1","pts_fbg1")
names(wr_fbg1) <- c("rank_fbg1","name_fbg1","teamBye_fbg1","age_fbg1","exp_fbg1","rushAtt_fbg1","rushYds_fbg1","rushTds_fbg1","rec_fbg1","recYds_fbg1","recYdsPerRec_fbg1","recTds_fbg1","pts_fbg1")
names(te_fbg1) <- c("rank_fbg1","name_fbg1","teamBye_fbg1","age_fbg1","exp_fbg1","rec_fbg1","recYds_fbg1","recYdsPerRec_fbg1","recTds_fbg1","pts_fbg1")

#Add variable for player position
qb_fbg1$pos <- as.factor("QB")
rb_fbg1$pos <- as.factor("RB")
wr_fbg1$pos <- as.factor("WR")
te_fbg1$pos <- as.factor("TE")

#Merge players across positions
projections_fbg1 <- rbind.fill(qb_fbg1, rb_fbg1, wr_fbg1, te_fbg1)

#Add variables from other projection sources
projections_fbg1$twoPts_fbg1 <- NA
projections_fbg1$fumbles_fbg1 <- NA

#Convert variables from character strings to numeric
projections_fbg1[,c("rank_fbg1","age_fbg1","exp_fbg1","passComp_fbg1","passAtt_fbg1","passCompPct_fbg1","passYds_fbg1","passYdsPerAtt_fbg1","passTds_fbg1","passInt_fbg1","rushAtt_fbg1","rushYds_fbg1","rushYdsPerAtt_fbg1","rushTds_fbg1","rec_fbg1","recYds_fbg1","recTds_fbg1","recYdsPerRec_fbg1","twoPts_fbg1","fumbles_fbg1","pts_fbg1")] <-
  convert.magic(projections_fbg1[,c("rank_fbg1","age_fbg1","exp_fbg1","passComp_fbg1","passAtt_fbg1","passCompPct_fbg1","passYds_fbg1","passYdsPerAtt_fbg1","passTds_fbg1","passInt_fbg1","rushAtt_fbg1","rushYds_fbg1","rushYdsPerAtt_fbg1","rushTds_fbg1","rec_fbg1","recYds_fbg1","recTds_fbg1","recYdsPerRec_fbg1","twoPts_fbg1","fumbles_fbg1","pts_fbg1")], "numeric")

#Team
projections_fbg1$team_fbg1 <- str_trim(sapply(str_split(projections_fbg1$teamBye_fbg1, "\\/"), "[", 1))

#Name for merging
projections_fbg1$name <- nameMerge(projections_fbg1$name_fbg1)

#Remove duplicate cases
projections_fbg1[projections_fbg1$name %in% projections_fbg1[duplicated(projections_fbg1$name),"name"],]

#Same name, different player
projections_fbg1 <- projections_fbg1[-which(projections_fbg1$name=="STEVESMITH" & projections_fbg1$team_fbg1==""),]

#Same player, different position

#Rename players
projections_fbg1[projections_fbg1$name=="TIMOTHYWRIGHT", "name"] <- "TIMWRIGHT"
projections_fbg1[projections_fbg1$name=="BENWATSON", "name"] <- "BENJAMINWATSON"

#Calculate overall rank
projections_fbg1$overallRank_fbg1 <- rank(-projections_fbg1$pts_fbg1, ties.method="min")

#Order variables in data set
projections_fbg1 <- projections_fbg1[,c("name","name_fbg1","pos","team_fbg1","overallRank_fbg1",
                                        "passAtt_fbg1","passComp_fbg1","passYds_fbg1","passTds_fbg1","passInt_fbg1",
                                        "rushYds_fbg1","rushTds_fbg1","rec_fbg1","recYds_fbg1","recTds_fbg1","twoPts_fbg1","fumbles_fbg1","pts_fbg1")]

#Order players by overall rank
projections_fbg1 <- projections_fbg1[order(projections_fbg1$overallRank_fbg1),]
row.names(projections_fbg1) <- 1:dim(projections_fbg1)[1]

#Density Plot
ggplot(projections_fbg1, aes(x=pts_fbg1)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Footballguys1 Projected Points")
ggsave(paste(getwd(),"/Figures/Footballguys1 projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fbg1, file = paste(getwd(),"/Data/Footballguys1-Projections.RData", sep=""))
write.csv(projections_fbg1, file=paste(getwd(),"/Data/Footballguys1-Projections.csv", sep=""), row.names=FALSE)

save(projections_fbg1, file = paste(getwd(),"/Data/Historical Projections/Footballguys1-Projections-2014.RData", sep=""))
write.csv(projections_fbg1, file=paste(getwd(),"/Data/Historical Projections/Footballguys1-Projections-2014.csv", sep=""), row.names=FALSE)
