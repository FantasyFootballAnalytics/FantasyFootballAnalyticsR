###########################
# File: Calculate League Projections.R
# Description: Calculates league projections based on league settings
# Date: 3/2/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# -ESPN projections do not include fumbles!
# To do:
###########################

#Library
library("reshape")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Projections
sourcesOfProjections <- c("Accuscore", "CBS", "ESPN", "FantasyPros", "FantasySharks", "NFL")
sourcesOfProjectionsAbbreviation <- c("accu", "cbs", "espn", "fp", "fs", "nfl")

#Import projections data
filenames <- paste(getwd(),"/Data/", sourcesOfProjections, "-Projections.RData", sep="")
lapply(filenames, load, envir=.GlobalEnv)

listProjections <- list(projections_accu, projections_cbs, projections_espn, projections_fp, projections_fs, projections_nfl)
  
#######OLD#######
#load(paste(getwd(),"/Data/Accuscore-Projections.RData", sep=""))
#load(paste(getwd(),"/Data/CBS-Projections.RData", sep=""))
#load(paste(getwd(),"/Data/ESPN-Projections.RData", sep=""))
#load(paste(getwd(),"/Data/FantasySharks-Projections.RData", sep=""))
#load(paste(getwd(),"/Data/NFL-Projections.RData", sep=""))
#load(paste(getwd(),"/Data/FantasyPros-Projections.RData", sep=""))
#################

#Merge projections data
projections <- merge_recurse(listProjections, by=c("name","pos")) #, all=TRUE

#######OLD#######
##Merge projections from ESPN and CBS
#projections <- merge(projections_espn, projections_cbs, by=c("name","pos"), all=TRUE)
#
##Remove duplicate cases
#projections[duplicated(projections$name),]
##projections[projections$name %in% projections[duplicated(projections$name),"name"],]
##projections[projections$name=="Steve Smith",]
##projections[projections$name=="Steve Smith",][c(2),] <- NA
##projections <- projections[!is.na(projections$name),]
#
##Merge projections with NFL.com
#projections <- merge(projections, projections_nfl, by=c("name","pos"), all=TRUE)
#
##Remove duplicate cases
#projections[projections$name %in% projections[duplicated(projections$name),"name"],]
##projections[duplicated(projections$name),]
#
##projections[projections$name=="Steve Smith" & projections$team_espn=="STL",c("team_nfl","positionRank_nfl","overallRank_nfl","passYds_nfl","passTds_nfl","passInt_nfl","rushYds_nfl","rushTds_nfl","recYds_nfl","recTds_nfl","twoPts_nfl","fumbles_nfl","pts_nfl")] <- NA
#
##Merge projections with FantasySharks
#projections <- merge(projections, projections_fs, by=c("name","pos"), all=TRUE)
#
##Remove duplicate cases
#projections[projections$name %in% projections[duplicated(projections$name),"name"],]
#
##Merge projections with Fantasy Pros
#projections <- merge(projections, projections_fp, by=c("name","pos"), all=TRUE)
#
##Remove duplicate cases
#projections[projections$name %in% projections[duplicated(projections$name),"name"],]
##projections[duplicated(projections$name),]
#
##projections[projections$name=="Steve Smith",][c(1),] <- NA
##projections <- projections[!is.na(projections$name),]
#################

#Add player name
projections$player <- projections$name_fp

#Determine Team
projections$team <- NA
for (i in 1:dim(projections)[1]){
  #If all are NA, set to NA
  if(is.na(projections[i,"team_espn"])==TRUE & is.na(projections[i,"team_cbs"])==TRUE & is.na(projections[i,"team_nfl"])==TRUE & is.na(projections[i,"team_fp"])==TRUE){
    projections[i,"team"] <- NA
  }
  
  #If all but one is NA, set to only one available
  else if (is.na(projections[i,"team_espn"])==TRUE & is.na(projections[i,"team_cbs"])==TRUE & is.na(projections[i,"team_nfl"])==TRUE){
    projections[i,"team"] <- projections[i,"team_fp"]
  } else if (is.na(projections[i,"team_espn"])==TRUE & is.na(projections[i,"team_cbs"])==TRUE & is.na(projections[i,"team_fp"])==TRUE){
    projections[i,"team"] <- projections[i,"team_nfl"]
  } else if (is.na(projections[i,"team_espn"])==TRUE & is.na(projections[i,"team_nfl"])==TRUE & is.na(projections[i,"team_fp"])==TRUE){
    projections[i,"team"] <- projections[i,"team_cbs"]
  } else if (is.na(projections[i,"team_cbs"])==TRUE & is.na(projections[i,"team_nfl"])==TRUE & is.na(projections[i,"team_fp"])==TRUE){
    projections[i,"team"] <- projections[i,"team_espn"]
  }
  
  #If FantasyPros available, set as FantasyPros
  else if (is.na(projections[i,"team_nfl"])==FALSE){
    projections[i,"team"] <- projections[i,"team_fp"]
  }
  
  #If FantasyPros is not available, and only one is available, set as only one available
  else if (is.na(projections[i,"team_espn"])==TRUE & is.na(projections[i,"team_cbs"])==TRUE){
    projections[i,"team"] <- projections[i,"team_nfl"]
  } else if (is.na(projections[i,"team_espn"])==TRUE & is.na(projections[i,"team_nfl"])==TRUE){
    projections[i,"team"] <- projections[i,"team_cbs"]
  } else if (is.na(projections[i,"team_cbs"])==TRUE & is.na(projections[i,"team_nfl"])==TRUE){
    projections[i,"team"] <- projections[i,"team_espn"]
  }
  
  #Settle conflicts
  else if (is.na(projections[i,"team_espn"])==TRUE){
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
  
  #Otherwise set to FantasyPros
  else{
    projections[i,"team"] <- projections[i,"team_fp"]
  }
}

#Check teams
projections[,c("name","pos","team_espn","team_cbs","team_nfl","team_fp","team_fs","team")]

#Calculate projections for each source
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste(c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recYdsPts","recTdsPts","twoPts","fumblesPts"), sourcesOfProjectionsAbbreviation[i], sep="_")] <- NA
  
  projections[,paste("passYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * passYdsMultiplier
  projections[,paste("passTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * passTdsMultiplier
  projections[,paste("passIntPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passIntPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * passIntMultiplier
  projections[,paste("rushYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rushYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * rushYdsMultiplier
  projections[,paste("rushTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rushTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * rushTdsMultiplier
  projections[,paste("recYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("recYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * recYdsMultiplier
  projections[,paste("recTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("recTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * recTdsMultiplier
  projections[,paste("twoPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("twoPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * twoPtsMultiplier
  projections[,paste("fumblesPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("fumblesPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * fumlMultiplier
  
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- NA
  
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- rowSums(projections[,paste(c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recYdsPts","recTdsPts","twoPts","fumblesPts"), sourcesOfProjectionsAbbreviation[i], sep="_")], na.rm=T)
}

#Calculate average of categories
projections$passYds <- rowMeans(projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passTds <- rowMeans(projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passInt <- rowMeans(projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushYds <- rowMeans(projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushTds <- rowMeans(projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$recYds <- rowMeans(projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$recTds <- rowMeans(projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$twoPts <- rowMeans(projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$fumbles <- rowMeans(projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)

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
projections[,c("name",paste("passYds", sourcesOfProjectionsAbbreviation, sep="_"),"passYds")]
projections[,c("name",paste("passTds", sourcesOfProjectionsAbbreviation, sep="_"),"passTds")]
projections[,c("name",paste("passInt", sourcesOfProjectionsAbbreviation, sep="_"),"passInt")]
projections[,c("name",paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_"),"rushYds")]
projections[,c("name",paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_"),"rushTds")]
projections[,c("name",paste("recYds", sourcesOfProjectionsAbbreviation, sep="_"),"recYds")]
projections[,c("name",paste("recTds", sourcesOfProjectionsAbbreviation, sep="_"),"recTds")]
projections[,c("name",paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_"),"twoPts")]
projections[,c("name",paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_"),"fumbles")]

#Calculate projected points for your league (avg of ESPN, CBS, NFL, and Fantasy Pros projections)
projections$passYdsPts <- projections$passYds * passYdsMultiplier
projections$passTdsPts <- projections$passTds * passTdsMultiplier
projections$passIntPts <- projections$passInt * passIntMultiplier
projections$rushYdsPts <- projections$rushYds * rushYdsMultiplier
projections$rushTdsPts <- projections$rushTds * rushTdsMultiplier
projections$recYdsPts <- projections$recYds * recYdsMultiplier
projections$recTdsPts <- projections$recTds * recTdsMultiplier
projections$fumblesPts <- projections$fumbles * fumlMultiplier

projections$projectedPtsAvg <- rowSums(projections[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recYdsPts","recTdsPts","twoPts","fumblesPts")], na.rm=T)

#Calculate latent variable for projected points
cor(projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"),"projectedPtsAvg")], use="pairwise.complete.obs")

factor.analysis <- factanal(projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_")], factors = 1, scores = "Bartlett") #factor.analysis <- factanal(~projectedPts_espn + projectedPts_cbs + projectedPts_nfl + projectedPts_fs + projectedPts_fp, factors = 1, scores = "Bartlett", data=projections) #regression

factor.scores <- factor.analysis$scores
factor.loadings <- factor.analysis$loadings[,1]
factor.loadings
#projectedPtsLatent <- factor.scores
projectedPtsLatent <- as.vector(factor.scores)

#Rescale the factor scores to have the same range as the average projections data
projectionVars <- projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_")]
projections$projectedPtsLatent <- rowMeans(projectionVars) + apply(projectionVars,1,sd)*projectedPtsLatent
projections$projectedPtsLatent <- rescaleRange(variable=projections$projectedPtsLatent, minOutput=0, maxOutput=max(projections$projectedPtsAvg))
projectionVars <- cbind(projectionVars,projections[,c("projectedPtsAvg","projectedPtsLatent")])

#Convert Zeros to NA
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")][which(projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] == 0)] <- NA
}

is.na(projectionVars) <- projectionVars==0

#Describe
describe(projectionVars)
plot(density(projections$projectedPtsAvg))
plot(density(projections$projectedPtsLatent))

#Correlations among projections
cor(projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"),"projectedPtsAvg","projectedPtsLatent")], use="pairwise.complete.obs")

#Set criterion for projections based on whose projections are most accurate
projections$projections <- projections$projectedPts_fp

#If projections are zero, set them to be the avg projections

#Calculate overall rank
projections$overallRank <- rank(-projections$projections, ties.method="min") #projectedPtsLatent

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]

#Keep important variables
projections <- projections[,c("name","player","pos","team","overallRank","projections",paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"),"projectedPtsAvg","projectedPtsLatent")]

#View projections
projections

#Density Plot
pointDensity <- c(projections$projectedPts_accu,projections$projectedPts_espn,projections$projectedPts_cbs,projections$projectedPts_nfl,projections$projectedPts_fs,projections$projectedPts_fp) #,projections$projectedPtsLatent
sourceDensity <- c(rep("Accuscore",dim(projections)[1]),rep("ESPN",dim(projections)[1]),rep("CBS",dim(projections)[1]),rep("NFL",dim(projections)[1]),rep("FS",dim(projections)[1]),rep("FP",dim(projections)[1])) #,rep("Latent",dim(projections)[1])
densityData <- data.frame(pointDensity,sourceDensity)

ggplot(densityData, aes(x=pointDensity, fill=sourceDensity)) + geom_density(alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Projected Points") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Calculate projections.jpg", sep=""))
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/LeagueProjections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/LeagueProjections.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Projections/LeagueProjections-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/LeagueProjections-2014.csv", sep=""), row.names=FALSE)
