###########################
# File: Risk.R
# Description: Calculates players' risk level
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These projections are from last year (ESPN and CBS have not yet updated them for the upcoming season)
# -ESPN projections do not include fumbles!
###########################

projections$sdPts <- apply(projections[,c("projectedPts_espn","projectedPts_cbs","projectedPts_nfl")],1,sd)
projections[,c("name","pos","projectedPts_espn","projectedPts_cbs","projectedPts_nfl","projectedPts","projectedPtsLatent","sdPts")]