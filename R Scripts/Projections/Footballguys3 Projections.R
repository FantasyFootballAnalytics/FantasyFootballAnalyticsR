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

qbURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projector=53"
rbURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=rb&projector=53&profile=0"
wrURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=wr&projector=53&profile=0"
teURL <- "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat=te&projector=53&profile=0"

pJS <- phantom() # start phantomjs
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
remDr$navigate(loginURL)
remDr$findElement("id", "login")$sendKeysToElement(list(footballguysUsername))  #replace "footballguysUsername" with your username
remDr$findElement("id", "pass")$sendKeysToElement(list(footballguysPassword))   #replace "footballguysPassword" with your password
remDr$findElement("css", ".am-login-form input[type='submit']")$clickElement()

remDr$navigate(qbURL)
tableElem <- remDr$findElement("css", "table.datamedium")
qb_fbg3 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

remDr$navigate(rbURL)
tableElem <- remDr$findElement("css", "table.datamedium")
rb_fbg3 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

remDr$navigate(wrURL)
tableElem <- remDr$findElement("css", "table.datamedium")
wr_fbg3 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

remDr$navigate(teURL)
tableElem <- remDr$findElement("css", "table.datamedium")
te_fbg3 <- readHTMLTable(header = TRUE, tableElem$getElementAttribute("outerHTML")[[1]], stringsAsFactors = FALSE)$'NULL'

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
projections_fbg3 <- projections_fbg3[-which(projections_fbg3$name=="STEVESMITH" & projections_fbg3$team_fbg3==""),]

#Same player, different position

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
