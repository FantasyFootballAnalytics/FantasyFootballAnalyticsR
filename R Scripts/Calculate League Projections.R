###########################
# File: Calculate League Projections.R
# Description: Calculates league projections based on league settings
# Date: 3/2/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These projections are from last year (ESPN and CBS have not yet updated them for the upcoming season)
# -ESPN projections do not include fumbles!
#To do:
#-Add FantasyPros Projections
###########################

#Customize your league settings
passYdsMultiplier <- (1/25) #1 pt per 25 pass yds
passTdsMultiplier <- 4      #4 pts per pass td
passIntMultiplier <- -3     #-3 pts per INT
rushYdsMultiplier <- (1/10) #1 pt per 10 rush yds
rushTdsMultiplier <- 6      #6 pts per rush td
recYdsMultiplier <- (1/8)   #1 pt per 8 rec yds
recTdsMultiplier <- 6       #6 pts per rec td
twoPtsMultiplier <- 2       #2 pts per 2-point conversion (not included in ESPN or CBS projections)
fumlMultiplier <- -3        #-3 pts per fumble lost (not included in ESPN projections)

#Library
library("reshape")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))

#Load data
load(paste(getwd(),"/Data/ESPN-Projections-2012.RData", sep=""))
load(paste(getwd(),"/Data/CBS-Projections-2012.RData", sep=""))
load(paste(getwd(),"/Data/NFL-Projections-2012.RData", sep=""))

#Merge projections from ESPN and CBS
projections <- merge(projections_espn, projections_cbs, by=c("name","pos"), all=TRUE)

#Remove duplicate cases
projections[duplicated(projections$name),]
projections[projections$name=="Steve Smith",]
projections[projections$name=="Steve Smith",][c(1,4),] <- NA
projections <- projections[!is.na(projections$name),]

#Merge projections with NFL.com
projections <- merge(projections, projections_nfl, by=c("name","pos"), all=TRUE)

#Remove duplicate cases
projections[duplicated(projections$name),]
projections[projections$name=="Steve Smith",]

projections[projections$name=="Steve Smith" & projections$team_espn=="STL",c("team_nfl","positionRank_nfl","overallRank_nfl","passYds_nfl","passTds_nfl","passInt_nfl","rushYds_nfl","rushTds_nfl","recYds_nfl","recTds_nfl","twoPts_nfl","fumbles_nfl","pts_nfl")] <- NA

#Determine Team
projections$team <- NA
for (i in 1:dim(projections)[1]){
  if(is.na(projections[i,"team_espn"])==TRUE & is.na(projections[i,"team_cbs"])==TRUE & is.na(projections[i,"team_nfl"])==TRUE){
    projections[i,"team"] <- NA
  } else if (is.na(projections[i,"team_espn"])==TRUE & is.na(projections[i,"team_cbs"])==TRUE){
    projections[i,"team"] <- projections[i,"team_nfl"]
  } else if (is.na(projections[i,"team_espn"])==TRUE & is.na(projections[i,"team_nfl"])==TRUE){
    projections[i,"team"] <- projections[i,"team_cbs"]
  } else if (is.na(projections[i,"team_cbs"])==TRUE & is.na(projections[i,"team_nfl"])==TRUE){
    projections[i,"team"] <- projections[i,"team_espn"]
  } else if (is.na(projections[i,"team_espn"])==TRUE){
    ifelse(length(unique(c(projections[i,"team_cbs"],projections[i,"team_nfl"])))==1, projections[i,"team"] <- unique(c(projections[i,"team_cbs"],projections[i,"team_nfl"])), projections[i,"team"] <- paste(projections[i,"team_cbs"], projections[i,"team_nfl"], sep="/"))
  } else if (is.na(projections[i,"team_cbs"])==TRUE){
    ifelse(length(unique(c(projections[i,"team_espn"],projections[i,"team_nfl"])))==1, projections[i,"team"] <- unique(c(projections[i,"team_espn"],projections[i,"team_nfl"])), projections[i,"team"] <- paste(projections[i,"team_espn"], projections[i,"team_nfl"], sep="/"))
  } else if (is.na(projections[i,"team_nfl"])==TRUE){
    ifelse(length(unique(c(projections[i,"team_espn"],projections[i,"team_cbs"])))==1, projections[i,"team"] <- unique(c(projections[i,"team_espn"],projections[i,"team_cbs"])), projections[i,"team"] <- paste(projections[i,"team_espn"], projections[i,"team_cbs"], sep="/"))
  } else if (projections[i,"team_espn"] == projections[i,"team_cbs"] && projections[i,"team_espn"] == projections[i,"team_nfl"]){
    projections[i,"team"] <- projections[i,"team_espn"]
  } else if (projections[i,"team_espn"] == projections[i,"team_cbs"]){
    projections[i,"team"] <- projections[i,"team_espn"]
  } else if (projections[i,"team_espn"] == projections[i,"team_nfl"]){
    projections[i,"team"] <- projections[i,"team_nfl"]
  } else if (projections[i,"team_cbs"] == projections[i,"team_nfl"]){
    projections[i,"team"] <- projections[i,"team_cbs"]
  }
}

#Calculate ESPN and CBS projections
projections$passYdsPts_espn <- projections$passYds_espn*passYdsMultiplier
projections$passTdsPts_espn <- projections$passTds_espn*passTdsMultiplier
projections$passIntPts_espn <- projections$passInt_espn*passIntMultiplier
projections$rushYdsPts_espn <- projections$rushYds_espn*rushYdsMultiplier
projections$rushTdsPts_espn <- projections$rushTds_espn*rushTdsMultiplier
projections$recYdsPts_espn <- projections$recYds_espn*recYdsMultiplier
projections$recTdsPts_espn <- projections$recTds_espn*recTdsMultiplier
projections$twoPts_espn <- projections$fumbles_espn*twoPtsMultiplier
projections$fumblesPts_espn <- projections$fumbles_espn*fumlMultiplier

projections$passYdsPts_cbs <- projections$passYds_cbs*passYdsMultiplier
projections$passTdsPts_cbs <- projections$passTds_cbs*passTdsMultiplier
projections$passIntPts_cbs <- projections$passInt_cbs*passIntMultiplier
projections$rushYdsPts_cbs <- projections$rushYds_cbs*rushYdsMultiplier
projections$rushTdsPts_cbs <- projections$rushTds_cbs*rushTdsMultiplier
projections$recYdsPts_cbs <- projections$recYds_cbs*recYdsMultiplier
projections$recTdsPts_cbs <- projections$recTds_cbs*recTdsMultiplier
projections$twoPts_cbs <- projections$fumbles_cbs*twoPtsMultiplier
projections$fumblesPts_cbs <- projections$fumbles_cbs*fumlMultiplier

projections$passYdsPts_nfl <- projections$passYds_nfl*passYdsMultiplier
projections$passTdsPts_nfl <- projections$passTds_nfl*passTdsMultiplier
projections$passIntPts_nfl <- projections$passInt_nfl*passIntMultiplier
projections$rushYdsPts_nfl <- projections$rushYds_nfl*rushYdsMultiplier
projections$rushTdsPts_nfl <- projections$rushTds_nfl*rushTdsMultiplier
projections$recYdsPts_nfl <- projections$recYds_nfl*recYdsMultiplier
projections$recTdsPts_nfl <- projections$recTds_nfl*recTdsMultiplier
projections$twoPts_nfl <- projections$fumbles_nfl*twoPtsMultiplier
projections$fumblesPts_nfl <- projections$fumbles_nfl*fumlMultiplier

projections$projectedPts_espn <- rowSums(projections[,c("passYdsPts_espn","passTdsPts_espn","passIntPts_espn","rushYdsPts_espn","rushTdsPts_espn","recYdsPts_espn","recTdsPts_espn","twoPts_espn","fumblesPts_espn")], na.rm=T)
projections$projectedPts_cbs <- rowSums(projections[,c("passYdsPts_cbs","passTdsPts_cbs","passIntPts_cbs","rushYdsPts_cbs","rushTdsPts_cbs","recYdsPts_cbs","recTdsPts_cbs","twoPts_cbs","fumblesPts_cbs")], na.rm=T)
projections$projectedPts_nfl <- rowSums(projections[,c("passYdsPts_nfl","passTdsPts_nfl","passIntPts_nfl","rushYdsPts_nfl","rushTdsPts_nfl","recYdsPts_nfl","recTdsPts_nfl","twoPts_nfl","fumblesPts_nfl")], na.rm=T)

#Calculate average of categories
projections$passYds <- rowMeans(projections[,c("passYds_espn","passYds_cbs","passYds_nfl")], na.rm=TRUE)
projections$passTds <- rowMeans(projections[,c("passTds_espn","passTds_cbs","passTds_nfl")], na.rm=TRUE)
projections$passInt <- rowMeans(projections[,c("passInt_espn","passInt_cbs","passInt_nfl")], na.rm=TRUE)
projections$rushYds <- rowMeans(projections[,c("rushYds_espn","rushYds_cbs","rushYds_nfl")], na.rm=TRUE)
projections$rushTds <- rowMeans(projections[,c("rushTds_espn","rushTds_cbs","rushTds_nfl")], na.rm=TRUE)
projections$recYds <- rowMeans(projections[,c("recYds_espn","recYds_cbs","recYds_nfl")], na.rm=TRUE)
projections$recTds <- rowMeans(projections[,c("recTds_espn","recTds_cbs","recTds_nfl")], na.rm=TRUE)
projections$twoPts <- rowMeans(projections[,c("twoPts_espn","twoPts_cbs","twoPts_nfl")], na.rm=TRUE)
projections$fumbles <- rowMeans(projections[,c("fumbles_espn","fumbles_cbs","fumbles_nfl")], na.rm=TRUE)

#Convert NA to 0
#projections[is.na(projections$passYds)==TRUE,"passYds"] <- 0
#projections[is.na(projections$passTds)==TRUE,"passTds"] <- 0
#projections[is.na(projections$passInt)==TRUE,"passInt"] <- 0
#projections[is.na(projections$rushYds)==TRUE,"rushYds"] <- 0
#projections[is.na(projections$rushTds)==TRUE,"rushTds"] <- 0
#projections[is.na(projections$recYds)==TRUE,"recYds"] <- 0
#projections[is.na(projections$recTds)==TRUE,"recTds"] <- 0
#projections[is.na(projections$twoPts)==TRUE,"twoPts"] <- 0
#projections[is.na(projections$fumbles)==TRUE,"fumbles"] <- 0

#If one site's projection is 0, take max of sites' projections
#for (i in 1:dim(projections)[1]){
#  ifelse(projections$passYds_espn[i]==0 | projections$passYds_cbs[i]==0, projections$passYds[i] <- max(projections$passYds_espn[i], projections$passYds_cbs[i], na.rm=TRUE), projections$passYds[i] <- projections$passYds[i])
#  ifelse(projections$passTds_espn[i]==0 | projections$passTds_cbs[i]==0, projections$passTds[i] <- max(projections$passTds_espn[i], projections$passTds_cbs[i], na.rm=TRUE), projections$passTds[i] <- projections$passTds[i])
#  ifelse(projections$passInt_espn[i]==0 | projections$passInt_cbs[i]==0, projections$passInt[i] <- max(projections$passInt_espn[i], projections$passInt_cbs[i], na.rm=TRUE), projections$passInt[i] <- projections$passInt[i])
#  ifelse(projections$rushYds_espn[i]==0 | projections$rushYds_cbs[i]==0, projections$rushYds[i] <- max(projections$rushYds_espn[i], projections$rushYds_cbs[i], na.rm=TRUE), projections$rushYds[i] <- projections$rushYds[i])
#  ifelse(projections$rushTds_espn[i]==0 | projections$rushTds_cbs[i]==0, projections$rushTds[i] <- max(projections$rushTds_espn[i], projections$rushTds_cbs[i], na.rm=TRUE), projections$rushTds[i] <- projections$rushTds[i])
#  ifelse(projections$recYds_espn[i]==0 | projections$recYds_cbs[i]==0, projections$recYds[i] <- max(projections$recYds_espn[i], projections$recYds_cbs[i], na.rm=TRUE), projections$recYds[i] <- projections$recYds[i])
#  ifelse(projections$recTds_espn[i]==0 | projections$recTds_cbs[i]==0, projections$recTds[i] <- max(projections$recTds_espn[i], projections$recTds_cbs[i], na.rm=TRUE), projections$recTds[i] <- projections$recTds[i])
#  ifelse(projections$fumbles_espn[i]==0 | projections$fumbles_cbs[i]==0, projections$fumbles[i] <- max(projections$fumbles_espn[i], projections$fumbles_cbs[i], na.rm=TRUE), projections$fumbles[i] <- projections$fumbles[i])
#}

#Check projections
projections[,c("name","passYds_espn","passYds_cbs","passYds_nfl","passYds")]
projections[,c("name","passTds_espn","passTds_cbs","passTds_nfl","passTds")]
projections[,c("name","passInt_espn","passInt_cbs","passInt_nfl","passInt")]
projections[,c("name","rushYds_espn","rushYds_cbs","rushYds_nfl","rushYds")]
projections[,c("name","rushTds_espn","rushTds_cbs","rushTds_nfl","rushTds")]
projections[,c("name","recYds_espn","recYds_cbs","recYds_nfl","recYds")]
projections[,c("name","recTds_espn","recTds_cbs","recTds_nfl","recTds")]
projections[,c("name","twoPts_espn","twoPts_cbs","twoPts_nfl","twoPts")]
projections[,c("name","fumbles_espn","fumbles_cbs","fumbles_nfl","fumbles")]

#Calculate projected points for your league (avg of ESPN, CBS, and NFL projections)
projections$passYdsPts <- projections$passYds*passYdsMultiplier
projections$passTdsPts <- projections$passTds*passTdsMultiplier
projections$passIntPts <- projections$passInt*passIntMultiplier
projections$rushYdsPts <- projections$rushYds*rushYdsMultiplier
projections$rushTdsPts <- projections$rushTds*rushTdsMultiplier
projections$recYdsPts <- projections$recYds*recYdsMultiplier
projections$recTdsPts <- projections$recTds*recTdsMultiplier
projections$fumblesPts <- projections$fumbles*fumlMultiplier

projections$projectedPts <- rowSums(projections[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recYdsPts","recTdsPts","twoPts","fumblesPts")], na.rm=T)

#Calculate latent variable for projected points
cor(projections[,c("projectedPts_espn","projectedPts_cbs","projectedPts_nfl","projectedPts")], use="pairwise.complete.obs")

factor.analysis <- factanal(~projectedPts_espn + projectedPts_cbs + projectedPts_nfl, factors = 1, scores = "Bartlett", data=projections) #regression
factor.scores <- factor.analysis$scores
factor.loadings <- factor.analysis$loadings[,1]
factor.loadings
projectedPtsLatent <- factor.scores

#Rescale the factor scores to have the same range as the average projections data
projections$projectedPtsLatent <- as.vector(rescaleRange(variable=projectedPtsLatent, minOutput=0, maxOutput=max(projections$projectedPts)))

#Convert Zeros to NA
projections$projectedPts_espn[projections$projectedPts_espn == 0] <- NA
projections$projectedPts_cbs[projections$projectedPts_cbs == 0] <- NA
projections$projectedPts_nfl[projections$projectedPts_nfl == 0] <- NA

#Correlations among projections
cor(projections[,c("projectedPts_espn","projectedPts_cbs","projectedPts_nfl","projectedPts","projectedPtsLatent")], use="pairwise.complete.obs")

#Calculate overall rank
projections$overallRank <- rank(-projections$projectedPtsLatent, ties.method="min")

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]

#Keep important variables
projections <- projections[,c("name","pos","team","overallRank","projectedPts_espn","projectedPts_cbs","projectedPts_nfl","projectedPts","projectedPtsLatent")]

#View projections
projections

#Density Plot
pointDensity <- c(projections$projectedPts_espn,projections$projectedPts_cbs,projections$projectedPts_nfl) #,projections$projectedPtsLatent
sourceDensity <- c(rep("ESPN",dim(projections)[1]),rep("CBS",dim(projections)[1]),rep("NFL",dim(projections)[1])) #,rep("Latent",dim(projections)[1])
densityData <- data.frame(pointDensity,sourceDensity)

ggplot(densityData, aes(x=pointDensity, fill=sourceDensity)) + geom_density(alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Projected Points from 2012") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Calculate projections 2012.jpg", sep=""))

#Save file
save(projections, file = paste(getwd(),"/Data/LeagueProjections-2012.RData", sep=""))