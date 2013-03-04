###########################
# File: Calculate League Projections.R
# Description: Calculates league projections based on league settings
# Date: 3/2/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These projections are from last year (ESPN and CBS have not yet updated them for the upcoming season)
# -ESPN projections do not include fumbles!
###########################

#Customize your league settings
passYdsMultiplier <- (1/25) #1 pt per 25 pass yds
passTdsMultiplier <- 4      #4 pts per pass td
passIntMultiplier <- -3     #-3 pts per INT
rushYdsMultiplier <- (1/10) #1 pt per 10 rush yds
rushTdsMultiplier <- 6      #6 pts per rush td
recYdsMultiplier <- (1/8)   #1 pt per 8 rec yds
recTdsMultiplier <- 6       #6 pts per rec td
fumlMultiplier <- -3        #-3 pts per fumble (not included in ESPN projections, however)

#Library

#Load data
load(paste(getwd(),"/Data/ESPN-Projections-2012.RData", sep=""))
load(paste(getwd(),"/Data/CBS-Projections-2012.RData", sep=""))

#Merge projections from ESPN and CBS
projections <- merge(projections_espn, projections_cbs, by=c("name","pos"), all=TRUE)

#Remove duplicate cases
projections[duplicated(projections$name),]
projections[projections$name=="Steve Smith",][c(1,4),] <- NA
projections <- projections[!is.na(projections$name),]

#Calculate ESPN and CBS projections
projections$passYdsPts_espn <- projections$passYds_espn*passYdsMultiplier
projections$passTdsPts_espn <- projections$passTds_espn*passTdsMultiplier
projections$passIntPts_espn <- projections$passInt_espn*passIntMultiplier
projections$rushYdsPts_espn <- projections$rushYds_espn*rushYdsMultiplier
projections$rushTdsPts_espn <- projections$rushTds_espn*rushTdsMultiplier
projections$recYdsPts_espn <- projections$recYds_espn*recYdsMultiplier
projections$recTdsPts_espn <- projections$recTds_espn*recTdsMultiplier
projections$fumblesPts_espn <- projections$fumbles_espn*fumlMultiplier

projections$passYdsPts_cbs <- projections$passYds_cbs*passYdsMultiplier
projections$passTdsPts_cbs <- projections$passTds_cbs*passTdsMultiplier
projections$passIntPts_cbs <- projections$passInt_cbs*passIntMultiplier
projections$rushYdsPts_cbs <- projections$rushYds_cbs*rushYdsMultiplier
projections$rushTdsPts_cbs <- projections$rushTds_cbs*rushTdsMultiplier
projections$recYdsPts_cbs <- projections$recYds_cbs*recYdsMultiplier
projections$recTdsPts_cbs <- projections$recTds_cbs*recTdsMultiplier
projections$fumblesPts_cbs <- projections$fumbles_cbs*fumlMultiplier

projections$projectedPts_espn <- rowSums(projections[,c("passYdsPts_espn","passTdsPts_espn","passIntPts_espn","rushYdsPts_espn","rushTdsPts_espn","recYdsPts_espn","recTdsPts_espn","fumblesPts_espn")], na.rm=T)
projections$projectedPts_cbs <- rowSums(projections[,c("passYdsPts_cbs","passTdsPts_cbs","passIntPts_cbs","rushYdsPts_cbs","rushTdsPts_cbs","recYdsPts_cbs","recTdsPts_cbs","fumblesPts_cbs")], na.rm=T)

#Calculate average of categories
projections$passYds <- rowMeans(projections[,c("passYds_espn","passYds_cbs")], na.rm=TRUE)
projections$passTds <- rowMeans(projections[,c("passTds_espn","passTds_cbs")], na.rm=TRUE)
projections$passInt <- rowMeans(projections[,c("passInt_espn","passInt_cbs")], na.rm=TRUE)
projections$rushYds <- rowMeans(projections[,c("rushYds_espn","rushYds_cbs")], na.rm=TRUE)
projections$rushTds <- rowMeans(projections[,c("rushTds_espn","rushTds_cbs")], na.rm=TRUE)
projections$recYds <- rowMeans(projections[,c("recYds_espn","recYds_cbs")], na.rm=TRUE)
projections$recTds <- rowMeans(projections[,c("recTds_espn","recTds_cbs")], na.rm=TRUE)
projections$fumbles <- rowMeans(projections[,c("fumbles_espn","fumbles_cbs")], na.rm=TRUE)

#If one site's projection is 0, take max of sites' projections
for (i in 1:dim(projections)[1]){
  ifelse(projections$passYds_espn[i]==0 | projections$passYds_cbs[i]==0, projections$passYds[i] <- max(projections$passYds_espn[i], projections$passYds_cbs[i], na.rm=TRUE), projections$passYds[i] <- projections$passYds[i])
  ifelse(projections$passTds_espn[i]==0 | projections$passTds_cbs[i]==0, projections$passTds[i] <- max(projections$passTds_espn[i], projections$passTds_cbs[i], na.rm=TRUE), projections$passTds[i] <- projections$passTds[i])
  ifelse(projections$passInt_espn[i]==0 | projections$passInt_cbs[i]==0, projections$passInt[i] <- max(projections$passInt_espn[i], projections$passInt_cbs[i], na.rm=TRUE), projections$passInt[i] <- projections$passInt[i])
  ifelse(projections$rushYds_espn[i]==0 | projections$rushYds_cbs[i]==0, projections$rushYds[i] <- max(projections$rushYds_espn[i], projections$rushYds_cbs[i], na.rm=TRUE), projections$rushYds[i] <- projections$rushYds[i])
  ifelse(projections$rushTds_espn[i]==0 | projections$rushTds_cbs[i]==0, projections$rushTds[i] <- max(projections$rushTds_espn[i], projections$rushTds_cbs[i], na.rm=TRUE), projections$rushTds[i] <- projections$rushTds[i])
  ifelse(projections$recYds_espn[i]==0 | projections$recYds_cbs[i]==0, projections$recYds[i] <- max(projections$recYds_espn[i], projections$recYds_cbs[i], na.rm=TRUE), projections$recYds[i] <- projections$recYds[i])
  ifelse(projections$recTds_espn[i]==0 | projections$recTds_cbs[i]==0, projections$recTds[i] <- max(projections$recTds_espn[i], projections$recTds_cbs[i], na.rm=TRUE), projections$recTds[i] <- projections$recTds[i])
  ifelse(projections$fumbles_espn[i]==0 | projections$fumbles_cbs[i]==0, projections$fumbles[i] <- max(projections$fumbles_espn[i], projections$fumbles_cbs[i], na.rm=TRUE), projections$fumbles[i] <- projections$fumbles[i])
}

#Check projections
projections[,c("name","passYds_espn","passYds_cbs","passYds")]
projections[,c("name","passTds_espn","passTds_cbs","passTds")]
projections[,c("name","passInt_espn","passInt_cbs","passInt")]
projections[,c("name","rushYds_espn","rushYds_cbs","rushYds")]
projections[,c("name","rushTds_espn","rushTds_cbs","rushTds")]
projections[,c("name","recYds_espn","recYds_cbs","recYds")]
projections[,c("name","recTds_espn","recTds_cbs","recTds")]
projections[,c("name","fumbles_espn","fumbles_cbs","fumbles")]

#Calculate projected points for your league (avg of ESPN and CBS projections)
projections$passYdsPts <- projections$passYds*passYdsMultiplier
projections$passTdsPts <- projections$passTds*passTdsMultiplier
projections$passIntPts <- projections$passInt*passIntMultiplier
projections$rushYdsPts <- projections$rushYds*rushYdsMultiplier
projections$rushTdsPts <- projections$rushTds*rushTdsMultiplier
projections$recYdsPts <- projections$recYds*recYdsMultiplier
projections$recTdsPts <- projections$recTds*recTdsMultiplier
projections$fumblesPts <- projections$fumbles*fumlMultiplier

projections$projectedPts <- rowSums(projections[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recYdsPts","recTdsPts","fumblesPts")], na.rm=T)

#Calculate overall rank
projections$overallRank <- rank(-projections$projectedPts, ties.method="min")

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]

#Keep important variables
projections <- projections[,c("name","pos","team_espn","team_cbs","overallRank","projectedPts_espn","projectedPts_cbs","projectedPts")]

#View projections
projections

#Save file
save(projections, file = paste(getwd(),"/Data/LeagueProjections-2012.RData", sep=""))