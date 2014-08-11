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

#Download fantasy football projections from Footballguys.com (password-protected)
qbURL_fbg3 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projector=53"
rbURL_fbg3 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=rb&projector=53&profile=0"
wrURL_fbg3 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=wr&projector=53&profile=0"
teURL_fbg3 <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=te&projector=53&profile=0"

handle_fbg <- handle("http://subscribers.footballguys.com") 
path_fbg   <- "amember/login.php"

qbLogin_fbg3 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = qbURL_fbg3
)

rbLogin_fbg3 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = rbURL_fbg3
)

wrLogin_fbg3 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = wrURL_fbg3
)

teLogin_fbg3 <- list(
  amember_login = footballguysUsername,
  amember_pass  = footballguysPassword,
  amember_redirect_url = teURL_fbg3
)

qbContent_fbg3 <- POST(handle = handle_fbg, path = path_fbg, body = qbLogin_fbg3)
rbContent_fbg3 <- POST(handle = handle_fbg, path = path_fbg, body = rbLogin_fbg3)
wrContent_fbg3 <- POST(handle = handle_fbg, path = path_fbg, body = wrLogin_fbg3)
teContent_fbg3 <- POST(handle = handle_fbg, path = path_fbg, body = teLogin_fbg3)

qb_fbg3 <- readHTMLTable(content(qbContent_fbg3), stringsAsFactors = FALSE)$'NULL'
rb_fbg3 <- readHTMLTable(content(rbContent_fbg3), stringsAsFactors = FALSE)$'NULL'
wr_fbg3 <- readHTMLTable(content(wrContent_fbg3), stringsAsFactors = FALSE)$'NULL'
te_fbg3 <- readHTMLTable(content(teContent_fbg3), stringsAsFactors = FALSE)$'NULL'

#Add variable names for each object
names(qb_fbg3) <- c("rank_fbg3","name_fbg3","teamBye_fbg3","age_fbg3","exp_fbg3","passComp_fbg3","passAtt_fbg3","passCompPct_fbg3","passYds_fbg3","passYdsPerAtt_fbg3","passTds_fbg3","passInt_fbg3","rushAtt_fbg3","rushYds_fbg3","rushTds_fbg3","pts_fbg3")
names(rb_fbg3) <- c("rank_fbg3","name_fbg3","teamBye_fbg3","age_fbg3","exp_fbg3","rushAtt_fbg3","rushYds_fbg3","rushYdsPerAtt_fbg3","rushTds_fbg3","rec_fbg3","recYds_fbg3","recTds_fbg3","pts_fbg3")
names(wr_fbg3) <- c("rank_fbg3","name_fbg3","teamBye_fbg3","age_fbg3","exp_fbg3","rushAtt_fbg3","rushYds_fbg3","rushTds_fbg3","rec_fbg3","recYds_fbg3","recYdsPerRec_fbg3","recTds_fbg3","pts_fbg3")
names(te_fbg3) <- c("rank_fbg3","name_fbg3","teamBye_fbg3","age_fbg3","exp_fbg3","rec_fbg3","recYds_fbg3","recYdsPerRec_fbg3","recTds_fbg3","pts_fbg3")

#Add variable for player position
qb_fbg3$pos <- as.factor("QB")
rb_fbg3$pos <- as.factor("RB")
wr_fbg3$pos <- as.factor("WR")
te_fbg3$pos <- as.factor("TE")

#Merge players across positions
projections_fbg3 <- rbind.fill(qb_fbg3, rb_fbg3, wr_fbg3, te_fbg3)

#Add variables from other projection sources
projections_fbg3$twoPts_fbg3 <- NA
projections_fbg3$fumbles_fbg3 <- NA

#Convert variables from character strings to numeric
projections_fbg3[,c("rank_fbg3","age_fbg3","exp_fbg3","passComp_fbg3","passAtt_fbg3","passCompPct_fbg3","passYds_fbg3","passYdsPerAtt_fbg3","passTds_fbg3","passInt_fbg3","rushAtt_fbg3","rushYds_fbg3","rushYdsPerAtt_fbg3","rushTds_fbg3","rec_fbg3","recYds_fbg3","recTds_fbg3","recYdsPerRec_fbg3","twoPts_fbg3","fumbles_fbg3","pts_fbg3")] <-
  convert.magic(projections_fbg3[,c("rank_fbg3","age_fbg3","exp_fbg3","passComp_fbg3","passAtt_fbg3","passCompPct_fbg3","passYds_fbg3","passYdsPerAtt_fbg3","passTds_fbg3","passInt_fbg3","rushAtt_fbg3","rushYds_fbg3","rushYdsPerAtt_fbg3","rushTds_fbg3","rec_fbg3","recYds_fbg3","recTds_fbg3","recYdsPerRec_fbg3","twoPts_fbg3","fumbles_fbg3","pts_fbg3")], "numeric")

#Team
projections_fbg3$team_fbg3 <- str_trim(sapply(str_split(projections_fbg3$teamBye_fbg3, "\\/"), "[", 1))

#Name for merging
projections_fbg3$name <- nameMerge(projections_fbg3$name_fbg3)

#Remove duplicate cases
projections_fbg3[projections_fbg3$name %in% projections_fbg3[duplicated(projections_fbg3$name),"name"],]

#Same name, different player

#Same player, different position

#Rename players
projections_fbg3[projections_fbg3$name=="TIMOTHYWRIGHT", "name"] <- "TIMWRIGHT"
projections_fbg3[projections_fbg3$name=="BENWATSON", "name"] <- "BENJAMINWATSON"

#Calculate overall rank
projections_fbg3$overallRank_fbg3 <- rank(-projections_fbg3$pts_fbg3, ties.method="min")

#Order variables in data set
projections_fbg3 <- projections_fbg3[,c("name","name_fbg3","pos","team_fbg3","overallRank_fbg3",
                                        "passAtt_fbg3","passComp_fbg3","passYds_fbg3","passTds_fbg3","passInt_fbg3",
                                        "rushYds_fbg3","rushTds_fbg3","rec_fbg3","recYds_fbg3","recTds_fbg3","twoPts_fbg3","fumbles_fbg3","pts_fbg3")]

#Order players by overall rank
projections_fbg3 <- projections_fbg3[order(projections_fbg3$overallRank_fbg3),]
row.names(projections_fbg3) <- 1:dim(projections_fbg3)[1]

#Density Plot
ggplot(projections_fbg3, aes(x=pts_fbg3)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Footballguys3 Projected Points")
ggsave(paste(getwd(),"/Figures/Footballguys3 projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fbg3, file = paste(getwd(),"/Data/Footballguys3-Projections.RData", sep=""))
write.csv(projections_fbg3, file=paste(getwd(),"/Data/Footballguys3-Projections.csv", sep=""), row.names=FALSE)

save(projections_fbg3, file = paste(getwd(),"/Data/Historical Projections/Footballguys3-Projections-2014.RData", sep=""))
write.csv(projections_fbg3, file=paste(getwd(),"/Data/Historical Projections/Footballguys3-Projections-2014.csv", sep=""), row.names=FALSE)
