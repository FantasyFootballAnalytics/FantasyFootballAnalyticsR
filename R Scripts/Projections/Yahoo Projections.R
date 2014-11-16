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

#Projection Info
year <- 2015
suffix <- "yahoo"

#Download fantasy football projections from Yahoo.com
yahoo_baseurl <- "http://football.fantasysports.yahoo.com/f1/39345/players?status=ALL&cut_type=9&myteam=0&sort=PTS&sdir=1"
yahoo_pages <- paste0("&count=", seq(0, 150, by=25))
yahoo_pos <- list(QB="QB", RB="RB", WR="WR", TE="TE", K="K", DEF="DEF") #, FLEX="W%2FR%2FT"
yahoo_urls <- paste0(yahoo_baseurl, yahoo_pages, "&pos=", rep(yahoo_pos, each=length(yahoo_pages)), "&stat1=S_PS_", year)

#Scrape
yahoo <- lapply(yahoo_urls, function(x) {data.table(readHTMLTable(x, stringsAsFactors = FALSE)[2]$'NULL')})

#Merge
projections_yahoo <- rbindlist(yahoo, fill=TRUE)

#Variable Names
setnames(projections_yahoo,
         c("star","player","add","owner","points","ownedPct","proj","actual",
           "passYds","passTds","passInt","passAtt",
           "rushYds","rushTds",
           "recTgt","rec","recYds","recTds",
           "returnTds","twoPts","fumbles","missing1",
           "kicker","fg019","fg2029","fg3039","fg4049","fg50","fg","missing2",
           "dst","dstPtsAllowed","dstSack","dstSafety","dstFumlRec","dstBlk","missing3"))

#Add source
projections_yahoo$sourceName <- suffix

#Remove special characters (%, comma)
projections_yahoo <- projections_yahoo[,lapply(.SD, function(x) gsub("\\%", "", x))]
projections_yahoo <- projections_yahoo[,lapply(.SD, function(x) gsub("\\,", "", x))]

#Convert variables from character strings to numeric
numericVars <- names(projections_yahoo)[names(projections_yahoo) %in% scoreCategories]

projections_yahoo[, (numericVars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = numericVars]

#Player name, position, and team
projections_yahoo[which(!is.na(kicker)), player := kicker]
projections_yahoo[which(!is.na(dst)), player := dst]
projections_yahoo[,player := str_trim(sapply(str_split(player, "\n"), "[", 2))]

projections_yahoo[,pos := str_trim(gsub("(.*)\\-", "\\2", player))]
projections_yahoo[which(nchar(pos) == 4), pos := str_sub(pos, start=0, end=2)] #remove 2nd position
projections_yahoo[which(pos == "DEF"), pos := "DST"]

projections_yahoo[,name_yahoo := str_trim(str_sub(gsub("(.*)\\-", "\\1", projections_yahoo$player), start=0, end=nchar(projections_yahoo$player)-4))]
projections_yahoo[,name_yahoo := str_trim(str_sub(gsub("  R", "", name_yahoo)))]
projections_yahoo[,name_yahoo := str_trim(str_sub(gsub("  W", "", name_yahoo)))]
projections_yahoo[,name_yahoo := str_trim(str_sub(gsub("  Q", "", name_yahoo)))]
projections_yahoo[,team_yahoo := toupper(str_trim(str_sub(name_yahoo, start=nchar(name_yahoo)-2, end=nchar(name_yahoo))))]
projections_yahoo[,name_yahoo := str_trim(str_sub(name_yahoo, start=0, end=nchar(name_yahoo)-3))]
projections_yahoo[,name := nameMerge(name_yahoo)]

#Remove NA rows
projections_yahoo <- projections_yahoo[!is.na(name),]

#Remove duplicate cases
duplicateCases <- projections_yahoo[duplicated(name)]$name
projections_yahoo[which(name %in% duplicateCases),]

#Rename players
projections_yahoo[name=="STEVIEJOHNSON", name:="STEVEJOHNSON"]

#Calculate overall rank
projections_yahoo[,overallRank := rank(-points, ties.method="min")]

#Calculate Position Rank
projections_yahoo[which(pos == "QB"), positionRank := rank(-projections_yahoo[which(pos == "QB"), "points", with=FALSE], ties.method="min")]
projections_yahoo[which(pos == "RB"), positionRank := rank(-projections_yahoo[which(pos == "RB"), "points", with=FALSE], ties.method="min")]
projections_yahoo[which(pos == "WR"), positionRank := rank(-projections_yahoo[which(pos == "WR"), "points", with=FALSE], ties.method="min")]
projections_yahoo[which(pos == "TE"), positionRank := rank(-projections_yahoo[which(pos == "TE"), "points", with=FALSE], ties.method="min")]
projections_yahoo[which(pos == "K"), positionRank := rank(-projections_yahoo[which(pos == "K"), "points", with=FALSE], ties.method="min")]
projections_yahoo[which(pos == "DST"), positionRank := rank(-projections_yahoo[which(pos == "DST"), "points", with=FALSE], ties.method="min")]

#Order variables in data set
allVars <- c(prefix, paste(sourceSpecific, suffix, sep="_"), varNames)
keepVars <- allVars[allVars %in% names(projections_yahoo)]
projections_yahoo <- projections_yahoo[,keepVars, with=FALSE]

#Order players by overall rank
projections_yahoo <- projections_yahoo[order(projections_yahoo$overallRank),]

#Density Plot
ggplot(projections_yahoo, aes(x=points)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Yahoo Projected Points")
ggsave(paste(getwd(),"/Figures/Yahoo projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_yahoo, file = paste0(getwd(),"/Data/Yahoo-Projections.RData"))
write.csv(projections_yahoo, file = paste(getwd(),"/Data/Yahoo-Projections.csv"), row.names=FALSE)

save(projections_yahoo, file = paste0(getwd(), "/Data/Historical Projections/Yahoo-Projections-", year, ".RData"))
write.csv(projections_yahoo, file = paste0(getwd(), "/Data/Historical Projections/Yahoo-Projections-", year, ".csv"), row.names=FALSE)
