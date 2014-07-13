###########################
# File: Footballguys Projections.R
# Description: Downloads Fantasy Football Projections from Footballguys.com
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#First, install phantomjs:
#http://phantomjs.org/download.html

#Second, add directory of phantomjs to your path enviroment variable:
#https://stackoverflow.com/questions/9546324/adding-directory-to-path-environment-variable-in-windows

#Third, install RSelenium
#library(devtools)
#devtools::install_github("ropensci/RSelenium")

#Load libraries
library(XML)
library(stringr)
library(ggplot2)
library(plyr)
library(RSelenium)

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Download fantasy football projections from Footballguys.com (password-protected)
loginURL <- "http://subscribers.footballguys.com/amember/login.php"

qbURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projector=50"
rbURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=rb&projector=50&profile=0"
wrURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=wr&projector=50&profile=0"
teURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=te&projector=50&profile=0"

pJS <- phantom() # start phantomjs
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
remDr$navigate(loginURL)
remDr$findElement("id", "login")$sendKeysToElement(list(footballguysUsername))  #replace "footballguysUsername" with your username
remDr$findElement("id", "pass")$sendKeysToElement(list(footballguysPassword))   #replace "footballguysPassword" with your password
remDr$findElement("css", ".am-login-form input[type='submit']")$clickElement()

remDr$navigate(qbURL)
tableElem <- remDr$findElement("css", "table.datamedium")
qb_fbg4 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

remDr$navigate(rbURL)
tableElem <- remDr$findElement("css", "table.datamedium")
rb_fbg4 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

remDr$navigate(wrURL)
tableElem <- remDr$findElement("css", "table.datamedium")
wr_fbg4 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

remDr$navigate(teURL)
tableElem <- remDr$findElement("css", "table.datamedium")
te_fbg4 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

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
projections_fbg4$twoPts_fbg4 <- NA
projections_fbg4$fumbles_fbg4 <- NA

#Convert variables from character strings to numeric
projections_fbg4[,c("rank_fbg4","age_fbg4","exp_fbg4","passComp_fbg4","passAtt_fbg4","passCompPct_fbg4","passYds_fbg4","passYdsPerAtt_fbg4","passTds_fbg4","passInt_fbg4","rushAtt_fbg4","rushYds_fbg4","rushYdsPerAtt_fbg4","rushTds_fbg4","rec_fbg4","recYds_fbg4","recTds_fbg4","recYdsPerRec_fbg4","twoPts_fbg4","fumbles_fbg4","pts_fbg4")] <-
  convert.magic(projections_fbg4[,c("rank_fbg4","age_fbg4","exp_fbg4","passComp_fbg4","passAtt_fbg4","passCompPct_fbg4","passYds_fbg4","passYdsPerAtt_fbg4","passTds_fbg4","passInt_fbg4","rushAtt_fbg4","rushYds_fbg4","rushYdsPerAtt_fbg4","rushTds_fbg4","rec_fbg4","recYds_fbg4","recTds_fbg4","recYdsPerRec_fbg4","twoPts_fbg4","fumbles_fbg4","pts_fbg4")], "numeric")

#Team
projections_fbg4$team_fbg4 <- str_trim(sapply(str_split(projections_fbg4$teamBye_fbg4, "\\/"), "[", 1))

#Name for merging
projections_fbg4$name <- nameMerge(projections_fbg4$name_fbg4)

#Remove duplicate cases
projections_fbg4[projections_fbg4$name %in% projections_fbg4[duplicated(projections_fbg4$name),"name"],]

#Same name, different player
projections_fbg4 <- projections_fbg4[-which(projections_fbg4$name=="STEVESMITH" & projections_fbg4$team_fbg4==""),]

#Same player, different position

#Calculate overall rank
projections_fbg4$overallRank_fbg4 <- rank(-projections_fbg4$pts_fbg4, ties.method="min")

#Order variables in data set
projections_fbg4 <- projections_fbg4[,c("name","name_fbg4","pos","team_fbg4","overallRank_fbg4",
                                        "passAtt_fbg4","passComp_fbg4","passYds_fbg4","passTds_fbg4","passInt_fbg4",
                                        "rushYds_fbg4","rushTds_fbg4","rec_fbg4","recYds_fbg4","recTds_fbg4","twoPts_fbg4","fumbles_fbg4","pts_fbg4")]

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

save(projections_fbg4, file = paste(getwd(),"/Data/Historical Projections/Footballguys4-Projections-2014.RData", sep=""))
write.csv(projections_fbg4, file=paste(getwd(),"/Data/Historical Projections/Footballguys4-Projections-2014.csv", sep=""), row.names=FALSE)
