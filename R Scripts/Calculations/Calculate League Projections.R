###########################
# File: Calculate League Projections.R
# Description: Calculates league projections based on league settings
# Date: 3/2/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Library
library("reshape")
library("MASS")
library("psych")
library("data.table")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Import projections data
filenames <- paste(getwd(),"/Data/", sourcesOfProjections, "-Projections.RData", sep="")
listProjections <- sapply(filenames, function(x) get(load(x)), simplify = FALSE)

projections <- rbindlist(listProjections, use.names=TRUE, fill=TRUE)
setkeyv(projections, cols=c("name","pos"))

#Set player name as most common instance across sources
playerNames <- na.omit(
  melt(projections[, c("name","pos", paste("name", sourcesOfProjectionsAbbreviation, sep="_")), with=FALSE], 
       id.var=c("name","pos"), value.name="player")
)[,player := names(which.max(table(player))),
  by=list(name, pos)][order(name), -3, with=FALSE]

setkeyv(playerNames, cols=c("name","pos"))
projections <- projections[unique(playerNames)]

#Set team name as most common instance across sources
teamNames <- na.omit(
  melt(projections[, c("name","pos", paste("team", sourcesOfProjectionsAbbreviation, sep="_")), with=FALSE], 
       id.var=c("name","pos"), value.name="team")
)[,team := names(which.max(table(team))),
  by=list(name, pos)][order(name), -3, with=FALSE]

setkeyv(teamNames, cols=c("name","pos"))
projections <- projections[unique(teamNames)]

#Set key
setkeyv(projections, cols=c("name","pos","team","sourceName"))

#Remove duplicate cases
cases <- projections[, c("name","pos","team"), with=FALSE]
uniqueCases <- unique(projections[, c("name","pos","team"), with=FALSE])
duplicateCases <- uniqueCases[duplicated(name) | duplicated(name, fromLast=TRUE)]

#Different player (Same name, different team)
setkeyv(duplicateCases, c("name", "team"))
duplicateCases[!duplicated(duplicateCases) & !duplicated(duplicateCases, fromLast=TRUE)]

#Same player (same name and team, different position)
setkeyv(duplicateCases, c("name", "team"))
duplicateCases[duplicated(duplicateCases) | duplicated(duplicateCases, fromLast=TRUE)]

#dropNames <- c("DENARDROBINSON","DEXTERMCCLUSTER","THEORIDDICK","ORSONCHARLES","JOEWEBB","EMILIGWENAGU","EVANRODRIGUEZ","BRADSMELLEY","RICHIEBROCKEL","BEARPASCOE","JEDCOLLINS","MARCUSTHIGPEN","JORDANLYNCH")
#dropVariables <- c("pos","pos","pos","pos","pos","pos","pos","pos","pos","pos","pos","pos","pos")
#dropLabels <- c("RB","WR","WR","TE","WR","RB","TE","TE","RB","RB","TE","WR","QB")

#projections2 <- ddply(projections, .(name), numcolwise(mean), na.rm=TRUE)

#for(i in 1:length(dropNames)){
#  if(dim(projections[-which(projections[,"name"] == dropNames[i] & projections[,dropVariables[i]] == dropLabels[i]),])[1] > 0){
#    projections <- projections[-which(projections[,"name"] == dropNames[i] & projections[,dropVariables[i]] == dropLabels[i]),]
#  }
#}

#projections <- merge(projections2, projections[,c("name","player","pos","team")], by="name", all.x=TRUE)

#Calculate stat categories for each source
projections[,passIncomp := passAtt - passComp]

#Calculate average of categories 
projectionsAvg <- projections[, lapply(.SD, mean, na.rm=TRUE), by=c("name","player","pos","team"), .SDcols=names(projections)[names(projections) %in% scoreCategories]]
projectionsAvg$sourceName <- "average"

#Calculate Hodges-Lehmann (pseudo-median) robust average of categories
projectionsRobustAvg <- projections[, lapply(.SD, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE))), by=c("name","player","pos","team"), .SDcols=names(projections)[names(projections) %in% scoreCategories]]
projectionsRobustAvg$sourceName <- "averageRobust"

#Merge
projectionCalculations <- rbind(projectionsAvg, projectionsRobustAvg, fill=TRUE)
setkeyv(projectionCalculations, cols=c("name","pos","team","sourceName"))

projections <- rbind(projections, projectionCalculations, fill=TRUE)

#Set key
setkeyv(projections, cols=c("name","pos","team"))
projections[, playerID := (.GRP), by=c("name","pos","team")]

#Calculate projected points for your league
projections[,passAttPts := passAtt * passAttMultiplier]
projections[,passCompPts := passComp * passCompMultiplier]
projections[,passIncompPts := passIncomp * passIncompMultiplier]
projections[,passYdsPts := passYds * passYdsMultiplier]
projections[,passTdsPts := passTds * passYdsMultiplier]
projections[,passIntPts := passInt * passIntMultiplier]
projections[,rushAttPts := rushAtt * rushAttMultiplier]
projections[,rushYdsPts := rushYds * rushYdsMultiplier]
projections[,rushTdsPts := rushTds * rushTdsMultiplier]
projections[,recPts := rec * recMultiplier]
projections[,recYdsPts := recYds * recYdsMultiplier]
projections[,recTdsPts := recTds * recTdsMultiplier]
projections[,returnTdsPts := returnTds * returnTdsMultiplier]
projections[,twoPtsPts := twoPts * twoPtsMultiplier]
projections[,fumblesPts := fumbles * fumlMultiplier]

scoreCategoriesPoints <- names(projections)[names(projections) %in% paste0(scoreCategories, "Pts")]

projections[,points := mySum(projections[,scoreCategoriesPoints, with=FALSE])]

#If variable is all missing for source, impute mean of other sources
pb <- txtProgressBar(min = 0, max = length(unique(projections$sourceName)), style = 3)
for(i in 1:length(unique(projections$sourceName))){
  setTxtProgressBar(pb, i)
  
  sourceIndex <- unique(projections$sourceName)[i]
  playerIDs <- projections$playerID[which(projections$sourceName == sourceIndex)]
  availableVars <- names(projections)[names(projections) %in% scoreCategories]
  
  if(sourceIndex != "average" & sourceIndex != "averageRobust"){
    missingVars <- availableVars[projections[which(projections$sourceName == sourceIndex), apply(.SD, 2, function(x) all(is.na(x))), .SDcols=availableVars]]
    projections[which(projections$sourceName == sourceIndex), (missingVars) := projections[which(projections$sourceName == "average" & projections$playerID %in% playerIDs), missingVars, with=FALSE]]
  }
}

#Calculate projections for each source
projections[,passAttPts := passAtt * passAttMultiplier]
projections[,passCompPts := passComp * passCompMultiplier]
projections[,passIncompPts := passIncomp * passIncompMultiplier]
projections[,passYdsPts := passYds * passYdsMultiplier]
projections[,passTdsPts := passTds * passYdsMultiplier]
projections[,passIntPts := passInt * passIntMultiplier]
projections[,rushAttPts := rushAtt * rushAttMultiplier]
projections[,rushYdsPts := rushYds * rushYdsMultiplier]
projections[,rushTdsPts := rushTds * rushTdsMultiplier]
projections[,recPts := rec * recMultiplier]
projections[,recYdsPts := recYds * recYdsMultiplier]
projections[,recTdsPts := recTds * recTdsMultiplier]
projections[,returnTdsPts := returnTds * returnTdsMultiplier]
projections[,twoPtsPts := twoPts * twoPtsMultiplier]
projections[,fumblesPts := fumbles * fumlMultiplier]

projections[,points := mySum(projections[,scoreCategoriesPoints, with=FALSE])]

#Describe
projections[,list(n = length(points),
                  min = min(points),
                  median = median(points),
                  mean = mean(points),
                  max = max(points))
            , by="sourceName"]

#Correlations among projections
projectionsWide <- dcast.data.table(projections, name + pos + team + playerID ~ sourceName, value.var="points", fun.aggregate = mean)

cor(projectionsWide[,c(unique(projections$sourceName)), with=FALSE], use="pairwise.complete.obs")

#Calculate ranks
projections <- projections[order(-points)][,overallRank := 1:.N, by=list(sourceName)]
projections <- projections[order(-points)][,positionRank := 1:.N, by=list(sourceName, pos)]

#projections$overallRank[projections$sourceName == "average"] <- rank(-projections$points[projections$sourceName == "average"], ties.method="min")
#projections$overallRank[projections$sourceName == "averageRobust"] <- rank(-projections$points[projections$sourceName == "averageRobust"], ties.method="min")

#Select and order variables
keepVars <- finalVarNames[finalVarNames %in% names(projections)]
projections <- projections[,keepVars, with=FALSE]

#Set Key
setkeyv(projections, cols=c("name","pos","team","sourceName"))

#Density Plot
pointDensity <- projections$points[order(projections$sourceName)]
sourceDensity <- projections$sourceName[order(projections$sourceName)]
densityData <- data.frame(pointDensity, sourceDensity)

ggplot(densityData, aes(x=pointDensity, fill=sourceDensity)) + geom_density(alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Projected Points") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Calculate projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/LeagueProjections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/LeagueProjections.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Projections/LeagueProjections-2015.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/LeagueProjections-2015.csv", sep=""), row.names=FALSE)
