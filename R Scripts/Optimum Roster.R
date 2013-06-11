###########################
# File: Optimum Roster.R
# Description: Determines Optimum Roster to Maximize Projected Points and Minimize Risk
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These calculations are from last year (they have not yet been updated for the upcoming season)
###########################

#Specify Maximum Risk
maxRisk <- 3.8

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/AvgCost-2013.RData", sep=""))

#Roster Optimization
optimizeData <- na.omit(projections[,c("name","pos","projections","risk","inflatedCost")]) #projectedPtsLatent

#Calculate Optimum Roster
optimizeTeam(maxRisk=maxRisk)
optimizeTeam(maxRisk=100)