###########################
# File: Optimum Roster.R
# Description: Determines Optimum Roster to Maximize Projected Points and Minimize Risk
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# -Cost is based on Yahoo Avg Cost
# To do:
###########################

#Specify Maximum Risk
maxRisk <- 4.6

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/AvgCost.RData", sep=""))

#Roster Optimization
optimizeData <- na.omit(projections[,c("name","player","pos","projections","risk","inflatedCost")]) #name,projectedPtsLatent,projectedPtsMedian

#Calculate Optimum Roster
optimizeTeam(maxRisk=maxRisk)
optimizeTeam(maxRisk=100)
