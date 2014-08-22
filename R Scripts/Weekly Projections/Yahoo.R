###########################
# File: Yahoo Weekly Projections.R
# Description: Downloads Fantasy Football Projections from yahoo.com
# Date: 8/21/2014
# Edited by: Michael Griebe (michaelgriebe@gmail.com)
# Notes: Adapted from Isaac Petersen (isaac@fantasyfootballanalytics.net)
# To do:
###########################
#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("data.table")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))
#Suffix
suffix <- "yahoo"

#Download fantasy football projections from Yahoo.com
baseurl <-"http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&myteam=0&sort=PTS&sdir=1"
playerCount <- paste("&count=",seq(0,350,by=25),sep="")
PWeekCount<-paste("&stat1=S_PW_",1:17,sep="")
YahooURLs <- unlist(lapply(PWeekCount,function(x)paste(baseurl,playerCount,x,sep="")))

#Pull from yahoo, but do it slowly, with random sys.sleep intervals ~7 secs long.  (Don't want to anger website admin.) Takes 30 mins.
#yahoo<-lapply(YahooURLs,function(x) {Sys.sleep(abs(rnorm(1)*4+7));readHTMLTable(x, stringsAsFactors = FALSE)[2]$'NULL'})
#save(yahoo,file="YahooRawWeekProj.RData")i
load("YahooRawWeekProj.RData")
yahoo<-yahoo_backup
#Add Weeks.
for(i in 1:length(yahoo)) { yahoo[[i]]$week <- trunc((i-1)/15+1)}

#rbind, makes a data.table.
yahoo_proj<-rbindlist(yahoo)

#Variable Names
setnames(yahoo_proj,c("star","player","add","owner","pts_yahoo","ownedPct","proj","actual",
                              "passYds_yahoo","passTds_yahoo","passInt_yahoo","rushYds_yahoo",
                         "rushTds_yahoo","recYds_yahoo","recTds_yahoo","returnTds_yahoo",
                         "twoPts_yahoo","fumbles_yahoo","missing","week"))
yahoo_proj
#Add missing variables
yahoo_proj[,c("passAtt_yahoo","passComp_yahoo","rushAtt_yahoo","rec_yahoo"):=NA]
#Remove special characters(commas)
yahoo_proj<-yahoo_proj[,lapply(.SD,function(x) gsub("\\,", "", x))]

#Convert variables from character strings to numeric
yahoo_proj<-yahoo_proj[,lapply(.SD,as.numeric),by=eval(names(yahoo_proj)[c(1:4,6,8,19)]),.SDcols=c(5,7,9:18,20:24)]
yahoo_proj<-yahoo_proj[,ownedPct := as.numeric(sub("%", "", ownedPct))]

#Player name, position, and team
yahoo_proj[,player := str_trim(sapply(str_split(player, "\n"), "[[", 2))]
yahoo_proj[,pos := str_trim(str_sub(player, start= -2))]
yahoo_proj[,name_yahoo := str_trim(str_sub(player, start=0, end=nchar(player)-8))]
yahoo_proj[,name := nameMerge(name_yahoo)]
yahoo_proj[,team_yahoo := toupper(str_trim(str_sub(player, start=str_locate(player, "-")[,1]-4, end=str_locate(player, "-")[,1]-2)))]

#Check for duplicates (duplicate means same name, position, week, and team).
setkey(yahoo_proj,name,pos,week,team_yahoo)
yahoo_proj[duplicated(yahoo_proj)]

#Rename players
yahoo_proj[name=="STEVIEJOHNSON", name:= "STEVEJOHNSON"]

#Calculate Week Rank
yahoo_proj[,weekRank_yahoo:= rank(-pts_yahoo, ties.method="min")]

#Calculate Season Rank
yahoo_proj[,SeasonPts_yahoo:=sum(pts_yahoo,rm.na=TRUE),by=list(name,pos,team_yahoo)]

#Overall Rank
yahoo_proj[,overallRank_yahoo:=as.numeric(as.factor(rank(-SeasonPts_yahoo,ties.method = "min")))]

#Calculate Position Rank
yahoo_proj[,positionRank_yahoo:=as.numeric(as.factor(rank(-SeasonPts_yahoo,ties.method = "min"))),by=list(pos)]
#Delete Nuisiance Columns
yahoo_proj[,c("star","player","add","owner","actual","missing","proj"):=NULL]
#Order variables in data set
setcolorder(yahoo_proj,c("week",prefix,"SeasonPts_yahoo","weekRank_yahoo", paste(varNames, suffix, sep="_"),"ownedPct"))

#Order players by overall rank
yahoo_proj[order(overallRank_yahoo,week)][1:100]
#Density Plot
ggplot(yahoo_proj, aes(x=pts_yahoo)) + geom_density(fill="blue", alpha=.3) + xlab("Players' Weekly Projected Points") + ggtitle("Density Plot of Yahoo Projected Points")
ggsave(paste(getwd(),"/Figures/Yahoo projections.jpg", sep=""), width=10, height=10)
dev.off()
#Save file
save(yahoo_proj, file = paste(getwd(),"/Data/Yahoo-Weekly-Projections.RData", sep=""))
write.csv(yahoo_proj, file=paste(getwd(),"/Data/Yahoo-Weekly-Projections.csv", sep=""), row.names=FALSE)
save(yahoo_proj, file = paste(getwd(),"/Data/Historical Projections/Yahoo-Weekly-Projections-2014.RData", sep=""))
write.csv(yahoo_proj, file=paste(getwd(),"/Data/Historical Projections/Yahoo-Weekly-Projections-2014.csv", sep=""), row.names=FALSE)
