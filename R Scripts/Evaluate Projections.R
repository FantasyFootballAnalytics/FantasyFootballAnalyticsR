###########################
# File: Evaluate Projections.R
# Description: Compares ESPN projections to actual values
# Date: 3/2/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These projections are from last year (ESPN has not yet updated them for the upcoming season)
# -ESPN projections do not include fumbles!
###########################

#Load data
load(paste(getwd(),"/Data/ESPN-LeagueProjections-2012.RData", sep=""))
actualPoints <- read.csv(paste(getwd(),"/Data/Yahoo-actualpoints-2012.csv", sep="")) 

#Calculate projected points for your league
projections$projectedPts <- (projections$passYds*passYdsMultiplier) + (projections$passTds*passTdsMultiplier) + (projections$passInt*passIntMultiplier) + (projections$rushYds*rushYdsMultiplier) + (projections$rushTds*rushTdsMultiplier) + (projections$recYds*recYdsMultiplier) + (projections$recTds*recTdsMultiplier)

#Cleanup Yahoo actual points data
actualPoints <- actualPoints[which(actualPoints$Fan.Pts!=""),]
actualPoints$name <- as.character(actualPoints$Player)
actualPoints$actualPts <- actualPoints$Fan.Pts
actualPoints <- actualPoints[,c("name","actualPts")]
row.names(actualPoints) <- 1:dim(actualPoints)[1]

#Merge ESPN projections with Yahoo actual points
projectedWithActualPts <- merge(projections, actualPoints, by="name", all.x=TRUE)

#Remove duplicate cases
projectedWithActualPts[!duplicated(projectedWithActualPts$name),]

#Correlation between projections and actual points
cor.test(projectedWithActualPts$projectedPts, projectedWithActualPts$actualPts) #r=.707, p<.001
cor(projectedWithActualPts$projectedPts, projectedWithActualPts$actualPts, use="pairwise.complete.obs")^2 #r-squared = .50

#After removing cases with projected points of 0
projectedWithActualPtsNoZeros <- projectedWithActualPts[which(projectedWithActualPts$projectedPts!=0),]

#Re-evaluate correlation between projections and actual points when cases with 0 projected points were excluded
cor.test(projectedWithActualPtsNoZeros$projectedPts, projectedWithActualPtsNoZeros$actualPts) #r=.683, p<.001
cor(projectedWithActualPtsNoZeros$projectedPts, projectedWithActualPtsNoZeros$actualPts, use="pairwise.complete.obs")^2 #r-squared = .47

#Save data
save(projectedWithActualPts, file = paste(getwd(),"/Data/projectedWithActualPoints-2012.RData", sep=""))