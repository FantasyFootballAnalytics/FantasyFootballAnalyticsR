###########################
# File: Evaluate Projections.R
# Description: Compares ESPN and CBS projections to actual values
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -ESPN projections do not include fumbles!
###########################

#Website
#http://fifthdown.blogs.nytimes.com/2010/11/12/how-accurate-are-yahoo-espn-and-cbs-fantasy-projections/

#Load data
load(paste(getwd(),"/Data/LeagueProjections-2012.RData", sep=""))
actualPoints <- read.csv(paste(getwd(),"/Data/Yahoo-actualpoints-2012.csv", sep="")) 

#Cleanup Yahoo actual points data
actualPoints <- actualPoints[which(actualPoints$Fan.Pts!=""),]
actualPoints$name <- as.character(actualPoints$Player)
actualPoints$actualPts <- actualPoints$Fan.Pts
actualPoints <- actualPoints[,c("name","actualPts")]
row.names(actualPoints) <- 1:dim(actualPoints)[1]

#Merge projections with Yahoo actual points
projectedWithActualPts <- merge(projections, actualPoints, by="name", all.x=TRUE)

#Remove duplicate cases
projectedWithActualPts[duplicated(projectedWithActualPts$name),]
projectedWithActualPts[projectedWithActualPts$name=="Alex Smith",][1,] <- NA
projectedWithActualPts <- projectedWithActualPts[!is.na(projectedWithActualPts$name),]
projectedWithActualPts[projectedWithActualPts$name=="Steve Smith",][c(3,4),] <- NA
projectedWithActualPts <- projectedWithActualPts[!is.na(projectedWithActualPts$name),]

#Correlation between projections and actual points
#ESPN
cor.test(projectedWithActualPts$projectedPts_espn, projectedWithActualPts$actualPts) #r=.734, p<.001
cor(projectedWithActualPts$projectedPts_espn, projectedWithActualPts$actualPts, use="pairwise.complete.obs")^2 #r-squared = .539

#CBS
cor.test(projectedWithActualPts$projectedPts_cbs, projectedWithActualPts$actualPts) #r=.762, p<.001
cor(projectedWithActualPts$projectedPts_cbs, projectedWithActualPts$actualPts, use="pairwise.complete.obs")^2 #r-squared = .580

#Average
cor.test(projectedWithActualPts$projectedPts, projectedWithActualPts$actualPts) #r=.765, p<.001
cor(projectedWithActualPts$projectedPts, projectedWithActualPts$actualPts, use="pairwise.complete.obs")^2 #r-squared = .585

#After removing cases with projected points of 0
projectedWithActualPtsNoZeros <- projectedWithActualPts[which(projectedWithActualPts$projectedPts!=0),]

#Re-evaluate correlation between projections and actual points when cases with 0 projected points were excluded
#ESPN
cor.test(projectedWithActualPtsNoZeros$projectedPts_espn, projectedWithActualPtsNoZeros$actualPts) #r=.721, p<.001
cor(projectedWithActualPtsNoZeros$projectedPts_espn, projectedWithActualPtsNoZeros$actualPts, use="pairwise.complete.obs")^2 #r-squared = .519

#CBS
cor.test(projectedWithActualPtsNoZeros$projectedPts_cbs, projectedWithActualPtsNoZeros$actualPts) #r=.754, p<.001
cor(projectedWithActualPtsNoZeros$projectedPts_cbs, projectedWithActualPtsNoZeros$actualPts, use="pairwise.complete.obs")^2 #r-squared = .569

#Average
cor.test(projectedWithActualPtsNoZeros$projectedPts, projectedWithActualPtsNoZeros$actualPts) #r=.759, p<.001
cor(projectedWithActualPtsNoZeros$projectedPts, projectedWithActualPtsNoZeros$actualPts, use="pairwise.complete.obs")^2 #r-squared = .576

#Save data
save(projectedWithActualPts, file = paste(getwd(),"/Data/projectedWithActualPoints-2012.RData", sep=""))