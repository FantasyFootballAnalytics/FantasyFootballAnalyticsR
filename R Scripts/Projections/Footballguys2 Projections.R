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

qbURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projector=41"
rbURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=rb&projector=41&profile=0"
wrURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=wr&projector=41&profile=0"
teURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=te&projector=41&profile=0"

pJS <- phantom() # start phantomjs
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
remDr$navigate(loginURL)
remDr$findElement("id", "login")$sendKeysToElement(list(footballguysUsername))  #replace "footballguysUsername" with your username
remDr$findElement("id", "pass")$sendKeysToElement(list(footballguysPassword))   #replace "footballguysPassword" with your password
remDr$findElement("css", ".am-login-form input[type='submit']")$clickElement()

remDr$navigate(qbURL)
tableElem <- remDr$findElement("css", "table.datamedium")
qb_fbg2 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

remDr$navigate(rbURL)
tableElem <- remDr$findElement("css", "table.datamedium")
rb_fbg2 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

remDr$navigate(wrURL)
tableElem <- remDr$findElement("css", "table.datamedium")
wr_fbg2 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

remDr$navigate(teURL)
tableElem <- remDr$findElement("css", "table.datamedium")
te_fbg2 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

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
projections_fbg2$twoPts_fbg2 <- NA
projections_fbg2$fumbles_fbg2 <- NA

#Convert variables from character strings to numeric
projections_fbg2[,c("rank_fbg2","age_fbg2","exp_fbg2","passComp_fbg2","passAtt_fbg2","passCompPct_fbg2","passYds_fbg2","passYdsPerAtt_fbg2","passTds_fbg2","passInt_fbg2","rushAtt_fbg2","rushYds_fbg2","rushYdsPerAtt_fbg2","rushTds_fbg2","rec_fbg2","recYds_fbg2","recTds_fbg2","recYdsPerRec_fbg2","twoPts_fbg2","fumbles_fbg2","pts_fbg2")] <-
  convert.magic(projections_fbg2[,c("rank_fbg2","age_fbg2","exp_fbg2","passComp_fbg2","passAtt_fbg2","passCompPct_fbg2","passYds_fbg2","passYdsPerAtt_fbg2","passTds_fbg2","passInt_fbg2","rushAtt_fbg2","rushYds_fbg2","rushYdsPerAtt_fbg2","rushTds_fbg2","rec_fbg2","recYds_fbg2","recTds_fbg2","recYdsPerRec_fbg2","twoPts_fbg2","fumbles_fbg2","pts_fbg2")], "numeric")

#Team
projections_fbg2$team_fbg2 <- str_trim(sapply(str_split(projections_fbg2$teamBye_fbg2, "\\/"), "[", 1))

#Name for merging
projections_fbg2$name <- nameMerge(projections_fbg2$name_fbg2)

#Remove duplicate cases
projections_fbg2[projections_fbg2$name %in% projections_fbg2[duplicated(projections_fbg2$name),"name"],]

#Same name, different player
projections_fbg2 <- projections_fbg2[-which(projections_fbg2$name=="STEVESMITH" & projections_fbg2$team_fbg2==""),]

#Same player, different position

#Calculate overall rank
projections_fbg2$overallRank_fbg2 <- rank(-projections_fbg2$pts_fbg2, ties.method="min")

#Order variables in data set
projections_fbg2 <- projections_fbg2[,c("name","name_fbg2","pos","team_fbg2","overallRank_fbg2",
                                        "passAtt_fbg2","passComp_fbg2","passYds_fbg2","passTds_fbg2","passInt_fbg2",
                                        "rushYds_fbg2","rushTds_fbg2","rec_fbg2","recYds_fbg2","recTds_fbg2","twoPts_fbg2","fumbles_fbg2","pts_fbg2")]

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
