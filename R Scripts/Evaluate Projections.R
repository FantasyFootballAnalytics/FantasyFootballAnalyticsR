###########################
# File: Evaluate Projections.R
# Description: Compares ESPN and CBS projections to actual values
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# -ESPN projections do not include fumbles!
# To do:
###########################

#Library
library("psy")
library("psych")
library("ggplot2")
library("forecast")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/LeagueProjections-2014.RData", sep=""))
actualPoints <- read.csv(paste(getwd(),"/Data/Yahoo-actualpoints-2012.csv", sep=""))

#Cleanup Yahoo actual points data
actualPoints <- actualPoints[which(actualPoints$Fan.Pts!=""),]
actualPoints$name <- as.character(actualPoints$Player)
actualPoints$actualPts <- actualPoints$Fan.Pts
actualPoints <- actualPoints[,c("name","actualPts")]
row.names(actualPoints) <- 1:dim(actualPoints)[1]

#Change player names
actualPoints[which(actualPoints$name=="Stevie Johnson"),"name"] <- "Steve Johnson"

#Merge projections with Yahoo actual points
projectedWithActualPts <- merge(projections, actualPoints, by="name", all.x=TRUE)

#Remove duplicate cases
projectedWithActualPts[projectedWithActualPts$name %in% projectedWithActualPts[duplicated(projectedWithActualPts$name),"name"],]

#projectedWithActualPts[duplicated(projectedWithActualPts$name),]
#projectedWithActualPts[projectedWithActualPts$name=="Alex Smith",]
#projectedWithActualPts[projectedWithActualPts$name=="Steve Smith",]

projectedWithActualPts[projectedWithActualPts$name=="Alex Smith",][2,] <- NA
projectedWithActualPts <- projectedWithActualPts[!is.na(projectedWithActualPts$name),]
projectedWithActualPts[projectedWithActualPts$name=="Steve Smith",][2,] <- NA
projectedWithActualPts <- projectedWithActualPts[!is.na(projectedWithActualPts$name),]

#Correlation between projections and actual points
cor(projectedWithActualPts[,c("projectedPts_espn","projectedPts_cbs","projectedPts_nfl","projectedPts_fp","projectedPtsAvg","projectedPtsLatent","actualPts")], use="pairwise.complete.obs")

#R-squared
summary(lm(actualPts ~ projectedPts_espn, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPts_cbs, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPts_nfl, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPts_fp, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPtsAvg, data=projectedWithActualPts))$r.squared
summary(lm(actualPts ~ projectedPtsLatent, data=projectedWithActualPts))$r.squared

#Absolute agreement (ICC)
icc(projectedWithActualPts[,c("projectedPts_espn","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPts_cbs","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPts_nfl","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPts_fp","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPtsAvg","actualPts")])$icc.agreement
icc(projectedWithActualPts[,c("projectedPtsLatent","actualPts")])$icc.agreement

#Harrell's c-index & Somers Dxy
rcorrcens(actualPts ~ projectedPts_espn, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPts_cbs, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPts_nfl, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPts_fp, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPtsAvg, data=projectedWithActualPts)
rcorrcens(actualPts ~ projectedPtsLatent, data=projectedWithActualPts)

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

#Plot
ggplot(data=projectedWithActualPts, aes(x=projectedPts_fp, y=actualPts)) + geom_point() + geom_smooth() + xlab("Projected Fantasy Football Points") + ylab("Actual Fantasy Football Points") + ggtitle("Association Between Projected Fantasy Points and Actual Points") +
  annotate("text", x = 80, y = max(projectedWithActualPts$projections), label = paste("R-Squared = ",round(summary(lm(actualPts ~ projections, data=projectedWithActualPts))$r.squared,2),sep=""))
ggsave(paste(getwd(),"/Figures/Evaluate Projections 2014.jpg", sep=""))
dev.off()

#Save data
save(projectedWithActualPts, file = paste(getwd(),"/Data/projectedWithActualPoints-2014.RData", sep=""))
write.csv(projectedWithActualPts, file=paste(getwd(),"/Data/CSV/projectedWithActualPoints-2014.csv", sep=""), row.names=FALSE)
