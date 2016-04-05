###########################
# File: AccuracyOfRankingsVsProjections.R
# Notes:
# To do: 
###########################

#Load libraries
library(data.table)

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))

#Data
projections2015 <- read.csv(paste(getwd(),"/R Scripts/Posts/Accuracy of Rankings vs Projections/FFA-CustomRankings-2015.csv", sep=""), header = TRUE, na.strings = "null", stringsAsFactors = FALSE)

#Convert to data.table 
projections2015 <- data.table(projections2015)

#Determine within-position preseason rankings
projections2015[is.finite(overallECR), preseasonRankings := rank(overallECR, ties.method = "min"), by = position]

#Subset accuracy data (compare only the same players by removing missing data)
projections2015[, preseasonProjections := points]

accuracy2015 <- projections2015[, c("playername","position","preseasonProjections","preseasonRankings","overallECR","actualPoints"), with=FALSE]
accuracy2015 <- accuracy2015[complete.cases(accuracy2015),]

setkey(accuracy2015, playername)
accuracy2015 <- unique(accuracy2015)
accuracy2015 <- accuracy2015[order(-rank(actualPoints))]

#View data
accuracy2015

#Calculate Accuracy of Projections vs. Rankings
summary(lm(actualPoints ~ preseasonProjections, data=accuracy2015))$r.squared
summary(lm(actualPoints ~ overallECR, data=accuracy2015))$r.squared

#Calculate Accuracy of Projections vs. Rankings by Position
summary(lm(actualPoints ~ preseasonProjections, data=accuracy2015[position == "QB",]))$r.squared
summary(lm(actualPoints ~ preseasonProjections, data=accuracy2015[position == "RB",]))$r.squared
summary(lm(actualPoints ~ preseasonProjections, data=accuracy2015[position == "WR",]))$r.squared
summary(lm(actualPoints ~ preseasonProjections, data=accuracy2015[position == "TE",]))$r.squared
summary(lm(actualPoints ~ preseasonProjections, data=accuracy2015[position == "K",]))$r.squared
summary(lm(actualPoints ~ preseasonProjections, data=accuracy2015[position == "DST",]))$r.squared

summary(lm(actualPoints ~ preseasonRankings, data=accuracy2015[position == "QB",]))$r.squared
summary(lm(actualPoints ~ preseasonRankings, data=accuracy2015[position == "RB",]))$r.squared
summary(lm(actualPoints ~ preseasonRankings, data=accuracy2015[position == "WR",]))$r.squared
summary(lm(actualPoints ~ preseasonRankings, data=accuracy2015[position == "TE",]))$r.squared
summary(lm(actualPoints ~ preseasonRankings, data=accuracy2015[position == "K",]))$r.squared
summary(lm(actualPoints ~ preseasonRankings, data=accuracy2015[position == "DST",]))$r.squared
