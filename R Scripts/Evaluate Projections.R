###########################
# File: Evaluate Projections.R
# Description: Compares ESPN and CBS projections to actual values
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# -ESPN projections do not include fumbles!
# To do:
###########################

#####################
# UPDATE
#####################
year <- 2013 #can only scrape Yahoo data from past 2 years (2012 or 2013), but can load data from other years if already scraped & saved
yahooLeagueID <- 39345
pagesToGrab <- 15

#Library
library("psy")
library("psych")
library("ggplot2")
library("forecast")
library("XML")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

###IMPORTANT###
# If already data are already scraped and download, skip to section 2

#####################
# 1. Scrape and Process Historical Actual Points
#####################

pb <- txtProgressBar(min = 0, max = pagesToGrab, style = 3)
for(i in 1:pagesToGrab){
  setTxtProgressBar(pb, i)
  if(i > 1){
    count <- 25*(i-1)
    assign(paste("yahoo", i, sep=""), readHTMLTable(paste("http://football.fantasysports.yahoo.com/f1/", yahooLeagueID, "/players?status=A&pos=O&cut_type=9&stat1=S_S_", year, "&myteam=0&sort=PTS&sdir=1&count=", count, sep=""), stringsAsFactors = FALSE)[2]$'NULL')
  } else if(i == 1){
    assign(paste("yahoo", i, sep=""), readHTMLTable(paste("http://football.fantasysports.yahoo.com/f1/", yahooLeagueID, "/players?&sort=PTS&sdir=1&status=A&pos=O&stat1=S_S_", year, "&jsenabled=1&jsenabled=1", sep=""), stringsAsFactors = FALSE)[2]$'NULL')
  }
}

#Merge
actualPoints <- rbind(yahoo1,yahoo2,yahoo3,yahoo4,yahoo5,yahoo6,yahoo7,yahoo8,yahoo9,yahoo10,yahoo11,yahoo12,yahoo13,yahoo14,yahoo15)

#Variable Names
names(actualPoints) <- c("star","player","add","owner","pts","ownedPct","proj","actual","passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","missing")

#Remove special characters(commas)
actualPoints[,c("passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts")] <-
  apply(actualPoints[,c("passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts")], 2, function(x) gsub("\\,", "", x))

#Convert variables from character strings to numeric
actualPoints[,c("passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts")] <- 
  convert.magic(actualPoints[,c("passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts")], "numeric")

#Player name, position, and team
actualPoints$player <- str_trim(sapply(str_split(actualPoints$player, "\n"), "[[", 2))
actualPoints$pos <- str_trim(str_sub(actualPoints$player, start= -2))
actualPoints$name_yahoo <- str_trim(str_sub(actualPoints$player, start=0, end=str_locate(actualPoints$player, "-")[,1]-5))
actualPoints$name <- nameMerge(actualPoints$name_yahoo)
actualPoints$team_yahoo <- toupper(str_trim(str_sub(actualPoints$player, start=str_locate(actualPoints$player, "-")[,1]-4, end=str_locate(actualPoints$player, "-")[,1]-2)))

#Select variables to keep
actualPoints <- actualPoints[,c("name","name_yahoo","pos","team_yahoo","passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts")]

#Save historical actual data
write.csv(actualPoints, file=paste(getwd(),"/Data/Historical Actual Points/Yahoo-actualpoints-", year, ".csv", sep=""), row.names=FALSE)

#####################
# 2. Import Historical Actual Stats and Calculate Actual Fantasy Points
#####################

actualPoints <- read.csv(paste(getwd(),"/Data/Historical Actual Points/Yahoo-actualpoints-", year, ".csv", sep=""))

#Calculate actual fantasy points for your league based on actual stats
actualPoints$passYdsPts <- actualPoints$passYds * passYdsMultiplier
actualPoints$passTdsPts <- actualPoints$passTds * passTdsMultiplier
actualPoints$passIntPts <- actualPoints$passInt * passIntMultiplier
actualPoints$rushYdsPts <- actualPoints$rushYds * rushYdsMultiplier
actualPoints$rushTdsPts <- actualPoints$rushTds * rushTdsMultiplier
actualPoints$recYdsPts <- actualPoints$recYds * recYdsMultiplier
actualPoints$recTdsPts <- actualPoints$recTds * recTdsMultiplier
actualPoints$fumblesPts <- actualPoints$fumbles * fumlMultiplier

actualPoints$actualPts <- rowSums(actualPoints[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recYdsPts","recTdsPts","twoPts","fumblesPts")], na.rm=T)

actualPoints <- actualPoints[,c("name","name_yahoo","pos","team_yahoo","actualPts")]
row.names(actualPoints) <- 1:dim(actualPoints)[1]

write.csv(actualPoints, file=paste(getwd(),"/Data/Historical Actual Points/Yahoo-actualpoints-", year, "-formatted.csv", sep=""), row.names=FALSE)

#####################
# 3. Import and Process Historical Projections
#####################
load(paste(getwd(), "/Data/Historical Projections/LeagueProjections-", year, ".RData", sep=""))
projections$name_fp <- projections$name
projections$name <- nameMerge(projections$name)

#####################
# 4. Merge/Process Projected & Actual Points
#####################

#Merge projections with Yahoo actual points
projectedWithActualPts <- merge(projections, actualPoints, by=c("name","pos"), all.x=TRUE)

#Remove duplicate cases
projectedWithActualPts[projectedWithActualPts$name %in% projectedWithActualPts[duplicated(projectedWithActualPts$name),"name"],]

#projectedWithActualPts[duplicated(projectedWithActualPts$name),]
#projectedWithActualPts[projectedWithActualPts$name=="Alex Smith",]
#projectedWithActualPts[projectedWithActualPts$name=="Steve Smith",]

#projectedWithActualPts[projectedWithActualPts$name=="Alex Smith",][2,] <- NA
#projectedWithActualPts <- projectedWithActualPts[!is.na(projectedWithActualPts$name),]
#projectedWithActualPts[projectedWithActualPts$name=="Steve Smith",][2,] <- NA
#projectedWithActualPts <- projectedWithActualPts[!is.na(projectedWithActualPts$name),]

#####################
# 5. Compare Projections
#####################

#Correlation between projections and actual points
cor(projectedWithActualPts[,c("projectedPts_espn","projectedPts_cbs","projectedPts_nfl","projectedPts_fp","projectedPtsAvg","projectedPtsLatent","actualPts")], use="pairwise.complete.obs")

#R-squared
summary(lm(actualPts ~ projectedPts_espn, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPts_cbs, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPts_nfl, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPts_fp, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPtsAvg, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPtsLatent, data=projectedWithActualPts))$r.squared

#Harrell's c-index & Somers Dxy
rcorrcens(actualPts ~ projectedPts_espn, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPts_cbs, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPts_nfl, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPts_fp, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPtsAvg, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPtsLatent, data=projectedWithActualPts)

#Absolute agreement (ICC)
icc(projectedWithActualPts[,c("projectedPts_espn","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPts_cbs","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPts_nfl","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPts_fp","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPtsAvg","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPtsLatent","actualPts")])$icc.agreement

#Mean Error (ME), Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), Mean Percentage Error (MPE), Mean Absolute Percentage Error (MAPE)
accuracy(na.omit(projectedWithActualPts[,c("actualPts","projectedPts_espn")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPts_espn")])[[2]])
accuracy(na.omit(projectedWithActualPts[,c("actualPts","projectedPts_cbs")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPts_cbs")])[[2]])
accuracy(na.omit(projectedWithActualPts[,c("actualPts","projectedPts_nfl")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPts_nfl")])[[2]])
accuracy(na.omit(projectedWithActualPts[,c("actualPts","projectedPts_fp")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPts_fp")])[[2]])
accuracy(na.omit(projectedWithActualPts[,c("actualPts","projectedPtsAvg")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPtsAvg")])[[2]])
accuracy(na.omit(projectedWithActualPts[,c("actualPts","projectedPtsLatent")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPtsLatent")])[[2]])

#Mean Absolute Scaled Error (MASE)
calculateMASE(na.omit(projectedWithActualPts[,c("actualPts","projectedPts_espn")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPts_espn")])[[2]])
calculateMASE(na.omit(projectedWithActualPts[,c("actualPts","projectedPts_cbs")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPts_cbs")])[[2]])
calculateMASE(na.omit(projectedWithActualPts[,c("actualPts","projectedPts_nfl")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPts_nfl")])[[2]])
calculateMASE(na.omit(projectedWithActualPts[,c("actualPts","projectedPts_fp")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPts_fp")])[[2]])
calculateMASE(na.omit(projectedWithActualPts[,c("actualPts","projectedPtsAvg")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPtsAvg")])[[2]])
calculateMASE(na.omit(projectedWithActualPts[,c("actualPts","projectedPtsLatent")])[[1]], na.omit(projectedWithActualPts[,c("actualPts","projectedPtsLatent")])[[2]])

#After removing cases with projected points of 0
projectedWithActualPtsNoZeros <- projectedWithActualPts[which(projectedWithActualPts$projectedPtsAvg!=0),]

#Re-evaluate correlation between projections and actual points when cases with 0 projected points were excluded
cor(projectedWithActualPtsNoZeros[,c("projectedPts_espn","projectedPts_cbs","projectedPts_nfl","projectedPts_fp","projectedPtsAvg","projectedPtsLatent","actualPts")], use="pairwise.complete.obs")

#R-squared
summary(lm(actualPts ~ projectedPts_espn, data=projectedWithActualPtsNoZeros))$r.squared
summary(lm(actualPts ~ projectedPts_cbs, data=projectedWithActualPtsNoZeros))$r.squared
summary(lm(actualPts ~ projectedPts_nfl, data=projectedWithActualPtsNoZeros))$r.squared
summary(lm(actualPts ~ projectedPts_fp, data=projectedWithActualPtsNoZeros))$r.squared
summary(lm(actualPts ~ projectedPtsAvg, data=projectedWithActualPtsNoZeros))$r.squared
summary(lm(actualPts ~ projectedPtsLatent, data=projectedWithActualPtsNoZeros))$r.squared

#Re-evaluate Absolute agreement
icc(projectedWithActualPtsNoZeros[,c("projectedPts_espn","actualPts")])$icc.agreement
icc(projectedWithActualPtsNoZeros[,c("projectedPts_cbs","actualPts")])$icc.agreement
icc(projectedWithActualPtsNoZeros[,c("projectedPts_nfl","actualPts")])$icc.agreement
icc(projectedWithActualPtsNoZeros[,c("projectedPts_fp","actualPts")])$icc.agreement
icc(projectedWithActualPtsNoZeros[,c("projectedPtsAvg","actualPts")])$icc.agreement
icc(projectedWithActualPtsNoZeros[,c("projectedPtsLatent","actualPts")])$icc.agreement

#Harrell's c-index & Somers Dxy
rcorrcens(actualPts ~ projectedPts_espn, data=projectedWithActualPtsNoZeros)
rcorrcens(actualPts ~ projectedPts_cbs, data=projectedWithActualPtsNoZeros)
rcorrcens(actualPts ~ projectedPts_nfl, data=projectedWithActualPtsNoZeros)
rcorrcens(actualPts ~ projectedPts_fp, data=projectedWithActualPtsNoZeros)
rcorrcens(actualPts ~ projectedPtsAvg, data=projectedWithActualPtsNoZeros)
rcorrcens(actualPts ~ projectedPtsLatent, data=projectedWithActualPtsNoZeros)

#Mean Error (ME), Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), Mean Percentage Error (MPE), Mean Absolute Percentage Error (MAPE)
accuracy(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_espn")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_espn")])[[2]])
accuracy(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_cbs")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_cbs")])[[2]])
accuracy(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_nfl")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_nfl")])[[2]])
accuracy(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_fp")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_fp")])[[2]])
accuracy(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPtsAvg")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPtsAvg")])[[2]])
accuracy(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPtsLatent")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPtsLatent")])[[2]])

#Mean Absolute Scaled Error (MASE)
calculateMASE(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_espn")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_espn")])[[2]])
calculateMASE(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_cbs")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_cbs")])[[2]])
calculateMASE(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_nfl")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_nfl")])[[2]])
calculateMASE(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_fp")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPts_fp")])[[2]])
calculateMASE(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPtsAvg")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPtsAvg")])[[2]])
calculateMASE(na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPtsLatent")])[[1]], na.omit(projectedWithActualPtsNoZeros[,c("actualPts","projectedPtsLatent")])[[2]])

#Compare accuracy of projections while taking into account risk vs when not taking risk into account
summary(lm(actualPts ~ projections, data=na.omit(projections[,c("actualPts","projections","risk")])))$r.squared #not considering risk #projectedPtsLatent
summary(lm(actualPts ~ projections + risk, data=na.omit(projections[,c("actualPts","projections","risk")])))$r.squared #considering risk #projectedPtsLatent
summary(lm(actualPts ~ projections + risk, data=na.omit(projections[,c("actualPts","projections","risk")])))

#Plot
ggplot(data=projectedWithActualPts, aes(x=projectedPts_fp, y=actualPts)) + geom_point() + geom_smooth() + xlab("Projected Fantasy Football Points") + ylab("Actual Fantasy Football Points") + ggtitle("Association Between Projected Fantasy Points and Actual Points") +
  annotate("text", x = 80, y = max(projectedWithActualPts$projections, na.rm=TRUE), label = paste("R-Squared = ",round(summary(lm(actualPts ~ projectedPts_fp, data=projectedWithActualPts))$r.squared,2),sep=""))
ggsave(paste(getwd(),"/Figures/Evaluate Projections.jpg", sep=""))
dev.off()

#Save data
save(projectedWithActualPts, file = paste(getwd(),"/Data/projectedWithActualPoints.RData", sep=""))
write.csv(projectedWithActualPts, file=paste(getwd(),"/Data/projectedWithActualPoints.csv", sep=""), row.names=FALSE)

save(projectedWithActualPts, file = paste(getwd(),"/Data/Historical Files/projectedWithActualPoints-", year, ".RData", sep=""))
write.csv(projectedWithActualPts, file=paste(getwd(),"/Data/Historical Files/projectedWithActualPoints-", year, ".csv", sep=""), row.names=FALSE)
