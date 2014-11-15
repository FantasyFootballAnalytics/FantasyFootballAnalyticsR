###########################
# File: Yahoo Projections.R
# Description: Downloads Fantasy Football Projections from yahoo.com
# Date: 5/31/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")
library("data.table")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Download fantasy football projections from Yahoo.com
yahoo1 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1", stringsAsFactors = FALSE)[2]$'NULL'
yahoo2 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=25", stringsAsFactors = FALSE)[2]$'NULL'
yahoo3 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=50", stringsAsFactors = FALSE)[2]$'NULL'
yahoo4 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=75", stringsAsFactors = FALSE)[2]$'NULL'
yahoo5 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=100", stringsAsFactors = FALSE)[2]$'NULL'
yahoo6 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=125", stringsAsFactors = FALSE)[2]$'NULL'
yahoo7 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=150", stringsAsFactors = FALSE)[2]$'NULL'
yahoo8 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=175", stringsAsFactors = FALSE)[2]$'NULL'
yahoo9 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=200", stringsAsFactors = FALSE)[2]$'NULL'
yahoo10 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=225", stringsAsFactors = FALSE)[2]$'NULL'
yahoo11 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=250", stringsAsFactors = FALSE)[2]$'NULL'
yahoo12 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=275", stringsAsFactors = FALSE)[2]$'NULL'
yahoo13 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=300", stringsAsFactors = FALSE)[2]$'NULL'
yahoo14 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=325", stringsAsFactors = FALSE)[2]$'NULL'
yahoo15 <- readHTMLTable("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_PS_2014&myteam=0&sort=PTS&sdir=1&count=350", stringsAsFactors = FALSE)[2]$'NULL'

#Merge
projections_yahoo <- rbind(yahoo1,yahoo2,yahoo3,yahoo4,yahoo5,yahoo6,yahoo7,yahoo8,yahoo9,yahoo10,yahoo11,yahoo12,yahoo13,yahoo14,yahoo15)

#Variable Names
names(projections_yahoo) <- c("star","player","add","owner","pts_yahoo","ownedPct","proj","actual",
                              "passYds_yahoo","passTds_yahoo","passInt_yahoo","passAtt_yahoo",
                              "rushYds_yahoo","rushTds_yahoo",
                              "recTgt_yahoo","rec_yahoo","recYds_yahoo","recTds_yahoo",
                              "returnTds_yahoo","twoPts_yahoo","fumbles_yahoo","missing")

#Add missing variables
projections_yahoo$passComp_yahoo <- NA
projections_yahoo$rushAtt_yahoo <- NA

#Convert to data.table for faster processing
setDT(projections_yahoo)

#Remove special characters (%)
projections_yahoo <- projections_yahoo[,lapply(.SD, function(x) gsub("\\%", "", x))]

#Convert variables from character strings to numeric
numericVars <- c("passAtt_yahoo","passComp_yahoo","passYds_yahoo","passTds_yahoo","passInt_yahoo","rushAtt_yahoo","rushYds_yahoo","rushTds_yahoo","rec_yahoo","recYds_yahoo","recTds_yahoo","returnTds_yahoo","twoPts_yahoo","fumbles_yahoo","pts_yahoo")

projections_yahoo[, (numericVars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = numericVars]

#Player name, position, and team
projections_yahoo[,player := str_trim(sapply(str_split(player, "\n"), "[[", 2))]
projections_yahoo[,pos := str_trim(str_sub(player, start= -2))]
projections_yahoo[,name_yahoo := str_trim(str_sub(player, start=0, end=nchar(player)-8))]
projections_yahoo[,name := nameMerge(name_yahoo)]
projections_yahoo[,team_yahoo := toupper(str_trim(str_sub(player, start=str_locate(player, "-")[,1]-4, end=str_locate(player, "-")[,1]-2)))]

#Remove duplicate cases
projections_yahoo[projections_yahoo$name %in% projections_yahoo[duplicated(projections_yahoo$name),"name"],]

#Rename players
projections_yahoo[name=="STEVIEJOHNSON", name:="STEVEJOHNSON"]

#Calculate overall rank
projections_yahoo[,overallRank_yahoo := rank(-pts_yahoo, ties.method="min")]

#Calculate Position Rank
projections_yahoo[which(pos == "QB"), positionRank_yahoo := rank(-projections_yahoo[which(projections_yahoo$pos == "QB"), "pts_yahoo", with=FALSE], ties.method="min")]
projections_yahoo[which(pos == "RB"), positionRank_yahoo := rank(-projections_yahoo[which(projections_yahoo$pos == "RB"), "pts_yahoo", with=FALSE], ties.method="min")]
projections_yahoo[which(pos == "WR"), positionRank_yahoo := rank(-projections_yahoo[which(projections_yahoo$pos == "WR"), "pts_yahoo", with=FALSE], ties.method="min")]
projections_yahoo[which(pos == "TE"), positionRank_yahoo := rank(-projections_yahoo[which(projections_yahoo$pos == "TE"), "pts_yahoo", with=FALSE], ties.method="min")]

#Add source name

#Order variables in data set
keepVars <- c(prefix, paste(varNames, suffix, sep="_"))
projections_yahoo <- projections_yahoo[,keepVars, with=FALSE]

#Order players by overall rank
projections_yahoo <- projections_yahoo[order(projections_yahoo$overallRank_yahoo),]
row.names(projections_yahoo) <- 1:nrow(projections_yahoo)

#Density Plot
ggplot(projections_yahoo, aes(x=pts_yahoo)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Yahoo Projected Points")
ggsave(paste(getwd(),"/Figures/Yahoo projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_yahoo, file = paste(getwd(),"/Data/Yahoo-Projections.RData", sep=""))
write.csv(projections_yahoo, file=paste(getwd(),"/Data/Yahoo-Projections.csv", sep=""), row.names=FALSE)

save(projections_yahoo, file = paste(getwd(),"/Data/Historical Projections/Yahoo-Projections-2015.RData", sep=""))
write.csv(projections_yahoo, file=paste(getwd(),"/Data/Historical Projections/Yahoo-Projections-2015.csv", sep=""), row.names=FALSE)
