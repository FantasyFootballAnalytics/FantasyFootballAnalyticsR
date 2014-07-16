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
library("MASS")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Import projections data
filenames <- paste(getwd(),"/Data/", sourcesOfProjections, "-Projections.RData", sep="")
listProjections <- sapply(filenames, function(x) get(load(x)), simplify = FALSE)

#Merge projections data
projections <- merge_recurse(listProjections, by=c("name","pos")) #, all=TRUE

#Set player name as most common instance across sources
nametable <- apply(projections[,paste("name", sourcesOfProjectionsAbbreviation, sep="_")], 1, table)  
projections$player <- names(sapply(nametable,`[`,1) )
projections$player[which(projections$player == "")] <- NA

#Set team name as most common instance across sources
teamtable <- apply(projections[,paste("team", sourcesOfProjectionsAbbreviation, sep="_")], 1, table)  
projections$team <- names(sapply(teamtable,`[`,1) )
projections$team[which(projections$team == "")] <- NA

#Remove duplicate cases
projections[projections$name %in% projections$name[duplicated(projections$name)],]

#Same name, different player

#Same player, different position
dropNames <- c("DENARDROBINSON","DEXTERMCCLUSTER","THEORIDDICK","ORSONCHARLES","JOEWEBB","EMILIGWENAGU")
dropVariables <- c("pos","pos","pos","pos","pos","pos")
dropLabels <- c("RB","WR","WR","TE","WR","RB")

projections2 <- ddply(projections, .(name), numcolwise(mean), na.rm=TRUE)

for(i in 1:length(dropNames)){
  if(dim(projections[-which(projections[,"name"] == dropNames[i] & projections[,dropVariables[i]] == dropLabels[i]),])[1] > 0){
    projections <- projections[-which(projections[,"name"] == dropNames[i] & projections[,dropVariables[i]] == dropLabels[i]),]
  }
}

projections <- merge(projections2, projections[,c("name","player","pos","team")], by="name")

#Calculate projections for each source
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste(c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","twoPtsPts","fumblesPts"), sourcesOfProjectionsAbbreviation[i], sep="_")] <- NA
  
  projections[,paste("passYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * passYdsMultiplier
  projections[,paste("passTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * passTdsMultiplier
  projections[,paste("passIntPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passInt", sourcesOfProjectionsAbbreviation[i], sep="_")] * passIntMultiplier
  projections[,paste("rushYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rushYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * rushYdsMultiplier
  projections[,paste("rushTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rushTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * rushTdsMultiplier
  projections[,paste("recPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rec", sourcesOfProjectionsAbbreviation[i], sep="_")] * recMultiplier
  projections[,paste("recYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("recYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * recYdsMultiplier
  projections[,paste("recTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("recTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * recTdsMultiplier
  projections[,paste("twoPtsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("twoPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * twoPtsMultiplier
  projections[,paste("fumblesPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("fumbles", sourcesOfProjectionsAbbreviation[i], sep="_")] * fumlMultiplier
  
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- NA
  
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- mySum(projections[,paste(c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","twoPtsPts","fumblesPts"), sourcesOfProjectionsAbbreviation[i], sep="_")])
}

#Calculate average of categories
projections$passYds <- rowMeans(projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passTds <- rowMeans(projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passInt <- rowMeans(projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushYds <- rowMeans(projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushTds <- rowMeans(projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rec <- rowMeans(projections[,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$recYds <- rowMeans(projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$recTds <- rowMeans(projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$twoPts <- rowMeans(projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$fumbles <- rowMeans(projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)

#Calculate Hodges-Lehmann (pseudo-median) average of categories
projections$passYdsMedian <- apply(projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passTdsMedian <- apply(projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passIntMedian <- apply(projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$rushYdsMedian <- apply(projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$rushTdsMedian <- apply(projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$recMedian <- apply(projections[,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$recYdsMedian <- apply(projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$recTdsMedian <- apply(projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$twoPtsMedian <- apply(projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$fumblesMedian <- apply(projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))

#Check projections
projections[,c("name",paste("passYds", sourcesOfProjectionsAbbreviation, sep="_"), c("passYds","passYdsMedian"))]
projections[,c("name",paste("passTds", sourcesOfProjectionsAbbreviation, sep="_"), c("passTds","passTdsMedian"))]
projections[,c("name",paste("passInt", sourcesOfProjectionsAbbreviation, sep="_"), c("passInt","passIntMedian"))]
projections[,c("name",paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_"), c("rushYds","rushYdsMedian"))]
projections[,c("name",paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_"), c("rushTds","rushTdsMedian"))]
projections[,c("name",paste("rec", sourcesOfProjectionsAbbreviation, sep="_"), c("rec","recMedian"))]
projections[,c("name",paste("recYds", sourcesOfProjectionsAbbreviation, sep="_"), c("recYds","recYdsMedian"))]
projections[,c("name",paste("recTds", sourcesOfProjectionsAbbreviation, sep="_"), c("recTds","recTdsMedian"))]
projections[,c("name",paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_"), c("twoPts","twoPtsMedian"))]
projections[,c("name",paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_"), c("fumbles","fumblesMedian"))]

#Calculate projected points for your league (average projections)
projections$passYdsPts <- projections$passYds * passYdsMultiplier
projections$passTdsPts <- projections$passTds * passTdsMultiplier
projections$passIntPts <- projections$passInt * passIntMultiplier
projections$rushYdsPts <- projections$rushYds * rushYdsMultiplier
projections$rushTdsPts <- projections$rushTds * rushTdsMultiplier
projections$recPts <- projections$rec * recMultiplier
projections$recYdsPts <- projections$recYds * recYdsMultiplier
projections$recTdsPts <- projections$recTds * recTdsMultiplier
projections$twoPtsPts <- projections$twoPts * twoPtsMultiplier
projections$fumblesPts <- projections$fumbles * fumlMultiplier

projections$passYdsMedianPts <- projections$passYdsMedian * passYdsMultiplier
projections$passTdsMedianPts <- projections$passTdsMedian * passTdsMultiplier
projections$passIntMedianPts <- projections$passIntMedian * passIntMultiplier
projections$rushYdsMedianPts <- projections$rushYdsMedian * rushYdsMultiplier
projections$rushTdsMedianPts <- projections$rushTdsMedian * rushTdsMultiplier
projections$recMedianPts <- projections$recMedian * recMultiplier
projections$recYdsMedianPts <- projections$recYdsMedian * recYdsMultiplier
projections$recTdsMedianPts <- projections$recTdsMedian * recTdsMultiplier
projections$twoPtsMedianPts <- projections$twoPtsMedian * twoPtsMultiplier
projections$fumblesMedianPts <- projections$fumblesMedian * fumlMultiplier

#Check projections
projections[,c("name",paste("passYdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("passYdsPts","passYdsMedianPts"))]
projections[,c("name",paste("passTdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("passTdsPts","passTdsMedianPts"))]
projections[,c("name",paste("passIntPts", sourcesOfProjectionsAbbreviation, sep="_"), c("passIntPts","passIntMedianPts"))]
projections[,c("name",paste("rushYdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("rushYdsPts","rushYdsMedianPts"))]
projections[,c("name",paste("rushTdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("rushTdsPts","rushYdsMedianPts"))]
projections[,c("name",paste("recPts", sourcesOfProjectionsAbbreviation, sep="_"), c("recPts","recMedianPts"))]
projections[,c("name",paste("recYdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("recYdsPts","recYdsMedianPts"))]
projections[,c("name",paste("recTdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("recTdsPts","recTdsMedianPts"))]
projections[,c("name",paste("twoPtsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("twoPtsPts","twoPtsMedianPts"))]
projections[,c("name",paste("fumblesPts", sourcesOfProjectionsAbbreviation, sep="_"), c("fumblesPts","fumblesMedianPts"))]

projections$projectedPtsMean <- mySum(projections[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","twoPtsPts","fumblesPts")])
projections$projectedPtsMedian <- mySum(projections[,c("passYdsMedianPts","passTdsMedianPts","passIntMedianPts","rushYdsMedianPts","rushTdsMedianPts","recMedianPts","recYdsMedianPts","recTdsMedianPts","twoPtsMedianPts","fumblesMedianPts")])

#Check projections
projections[,c("name",paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))]

projectionVars <- projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))]

#Convert Zeros to NA
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")][which(projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] == 0)] <- NA
}

is.na(projectionVars) <- projectionVars==0

#Describe
describe(projectionVars)
plot(density(na.omit(projections$projectedPtsMean)), col="black")
lines(density(na.omit(projections$projectedPtsMedian)), col="blue")

#Correlations among projections
cor(projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))], use="pairwise.complete.obs")

#Set criterion for projections based on whose projections are most accurate
projections$projections <- projections$projectedPtsMedian

#Calculate overall rank
projections$overallRank <- rank(-projections$projections, ties.method="min")

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]

#Keep important variables
projections <- projections[,c("name","player","pos","team","overallRank","projections",paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))]

#View projections
projections
projections[,c("name","pos","team","projectedPts_fp","projectedPtsMean","projectedPtsMedian")]

#Density Plot
pointDensity <- c(projections$projectedPts_cbs1, projections$projectedPts_cbs2, projections$projectedPts_espn, projections$projectedPts_nfl, projections$projectedPts_fs, projections$projectedPts_fp, projections$projectedPts_fftoday, projections$projectedPts_fbg1, projections$projectedPts_fbg2, projections$projectedPts_fbg3, projections$projectedPts_fbg4) #,projections$projectedPts_accu, projections$projectedPts_cbs, projections$projectedPts_yahoo, projections$projectedPtsLatent, projections$projectedPtsMean
sourceDensity <- c(rep("CBS1",dim(projections)[1]), rep("CBS2",dim(projections)[1]), rep("ESPN",dim(projections)[1]), rep("NFL.com",dim(projections)[1]), rep("FantasySharks",dim(projections)[1]), rep("FantasyPros",dim(projections)[1]), rep("FFtoday",dim(projections)[1]), rep("Footballguys1",dim(projections)[1]), rep("Footballguys2",dim(projections)[1]), rep("Footballguys3",dim(projections)[1]), rep("Footballguys4",dim(projections)[1])) #,rep("Accuscore",dim(projections)[1]), rep("CBS",dim(projections)[1]), rep("Yahoo",dim(projections)[1]), rep("Latent",dim(projections)[1]), rep("Average",dim(projections)[1])
densityData <- data.frame(pointDensity, sourceDensity)

ggplot(densityData, aes(x=pointDensity, fill=sourceDensity)) + geom_density(alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Projected Points") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Calculate projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/LeagueProjections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/LeagueProjections.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Projections/LeagueProjections-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/LeagueProjections-2014.csv", sep=""), row.names=FALSE)
