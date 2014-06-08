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

#Add player name
projections$player <- projections$name_fp

#Set team name as most common instance across sources
mytable <- apply(projections[,paste("team", sourcesOfProjectionsAbbreviation, sep="_")], 1, table)  
projections$team <- names(sapply(mytable,`[`,1) )
projections$team[which(projections$team == "")] <- NA

#Remove duplicate cases
projections[projections$name %in% projections$name[duplicated(projections$name)],]

#Calculate projections for each source
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste(c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","twoPts","fumblesPts"), sourcesOfProjectionsAbbreviation[i], sep="_")] <- NA
  
  projections[,paste("passYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * passYdsMultiplier
  projections[,paste("passTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * passTdsMultiplier
  projections[,paste("passIntPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passInt", sourcesOfProjectionsAbbreviation[i], sep="_")] * passIntMultiplier
  projections[,paste("rushYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rushYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * rushYdsMultiplier
  projections[,paste("rushTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rushTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * rushTdsMultiplier
  projections[,paste("recPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rec", sourcesOfProjectionsAbbreviation[i], sep="_")] * recMultiplier
  projections[,paste("recYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("recYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * recYdsMultiplier
  projections[,paste("recTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("recTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * recTdsMultiplier
  projections[,paste("twoPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("twoPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * twoPtsMultiplier
  projections[,paste("fumblesPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("fumblesPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * fumlMultiplier
  
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- NA
  
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- rowSums(projections[,paste(c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","twoPts","fumblesPts"), sourcesOfProjectionsAbbreviation[i], sep="_")], na.rm=T)
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
pb <- txtProgressBar(min = 0, max = dim(projections)[1], style = 3)
for(i in 1:dim(projections)[1]){
  setTxtProgressBar(pb, i)
  
  projections$passYdsMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
  projections$passTdsMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
  projections$passIntMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
  projections$rushYdsMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
  projections$rushTdsMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
  projections$recMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
  projections$recYdsMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
  projections$recTdsMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
  projections$twoPtsMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
  projections$fumblesMedian[i] <- tryCatch(wilcox.test(as.numeric(as.vector(projections[i,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")])), conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(as.numeric(as.vector(projections[i,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")])), na.rm=TRUE))
}

#Convert NA to 0
#projections[is.na(projections$passYds)==TRUE,"passYds"] <- 0
#projections[is.na(projections$passTds)==TRUE,"passTds"] <- 0
#projections[is.na(projections$passInt)==TRUE,"passInt"] <- 0
#projections[is.na(projections$rushYds)==TRUE,"rushYds"] <- 0
#projections[is.na(projections$rushTds)==TRUE,"rushTds"] <- 0
#projections[is.na(projections$rec)==TRUE,"rec"] <- 0
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

projections$projectedPtsMean <- rowSums(projections[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","twoPtsPts","fumblesPts")], na.rm=T)
projections$projectedPtsMedian <- rowSums(projections[,c("passYdsMedianPts","passTdsMedianPts","passIntMedianPts","rushYdsMedianPts","rushTdsMedianPts","recMedianPts","recYdsMedianPts","recTdsMedianPts","twoPtsMedianPts","fumblesMedianPts")], na.rm=T)

#Calculate latent variable for projected points
cor(projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))], use="pairwise.complete.obs")

factor.analysis <- factanal(projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_")], factors = 1, scores = "Bartlett") #factor.analysis <- factanal(~projectedPts_espn + projectedPts_cbs + projectedPts_nfl + projectedPts_fs + projectedPts_fp, factors = 1, scores = "Bartlett", data=projections) #regression

factor.scores <- factor.analysis$scores
factor.loadings <- factor.analysis$loadings[,1]
factor.loadings
projectedPtsLatent <- as.vector(factor.scores)

#Best fitting distribution of Average Fantasy Points
fitdistr(projections$projectedPtsMedian - floor(min(projections$projectedPtsMedian, na.rm=TRUE)), 't')$loglik
fitdistr(projections$projectedPtsMedian - floor(min(projections$projectedPtsMedian, na.rm=TRUE)), 'normal')$loglik
fitdistr(projections$projectedPtsMedian - floor(min(projections$projectedPtsMedian, na.rm=TRUE)), 'logistic')$loglik
fitdistr(projections$projectedPtsMedian - floor(min(projections$projectedPtsMedian, na.rm=TRUE)), 'weibull')$loglik #best
fitdistr(projections$projectedPtsMedian - floor(min(projections$projectedPtsMedian, na.rm=TRUE)), 'gamma')$loglik
fitdistr(projections$projectedPtsMedian - floor(min(projections$projectedPtsMedian, na.rm=TRUE)), 'lognormal')$loglik
fitdistr(projections$projectedPtsMedian - floor(min(projections$projectedPtsMedian, na.rm=TRUE)), 'exponential')$loglik

weibullShape <- fitdistr(projections$projectedPtsMedian - floor(min(projections$projectedPtsMedian, na.rm=TRUE)), 'weibull')$estimate[[1]]
weibullScale <- fitdistr(projections$projectedPtsMedian - floor(min(projections$projectedPtsMedian, na.rm=TRUE)), 'weibull')$estimate[[2]]

projectedPtsLatentWeibull <- qweibull(pnorm(projectedPtsLatent), shape=weibullShape, scale=weibullScale)
projectedPtsLatentWeibullRescaled <- rescaleRange(variable=projectedPtsLatentWeibull, minOutput=0, maxOutput=max(projections$projectedPtsMedian))

projections$projectedPtsLatent <- projectedPtsLatentWeibullRescaled

projectionVars <- projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian","projectedPtsLatent"))]

#Convert Zeros to NA
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")][which(projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] == 0)] <- NA
}

is.na(projectionVars) <- projectionVars==0

#Describe
describe(projectionVars)
plot(density(na.omit(projections$projectedPtsMean)), col="black")
lines(density(na.omit(projections$projectedPtsMedian)), col="blue")
lines(density(na.omit(projections$projectedPtsLatent)), col="red")

#Correlations among projections
cor(projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian","projectedPtsLatent"))], use="pairwise.complete.obs")

#Set criterion for projections based on whose projections are most accurate
projections$projections <- projections$projectedPts_fp

#If projections are zero, set them to be the avg projections

#Calculate overall rank
projections$overallRank <- rank(-projections$projections, ties.method="min") #projectedPtsLatent

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]

#Keep important variables
projections <- projections[,c("name","player","pos","team","overallRank","projections",paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian","projectedPtsLatent"))]

#View projections
projections
projections[,c("name","pos","team","projectedPts_fp","projectedPtsMean","projectedPtsMedian","projectedPtsLatent")]

#Density Plot
pointDensity <- c(projections$projectedPts_accu, projections$projectedPts_espn, projections$projectedPts_cbs, projections$projectedPts_nfl, projections$projectedPts_fs, projections$projectedPts_fp, projections$projectedPts_fftoday, projections$projectedPts_yahoo, projections$projectedPtsMean) #,projections$projectedPtsLatent
sourceDensity <- c(rep("Accuscore",dim(projections)[1]), rep("ESPN",dim(projections)[1]), rep("CBS",dim(projections)[1]), rep("NFL",dim(projections)[1]), rep("FS",dim(projections)[1]), rep("FP",dim(projections)[1]), rep("FFtoday",dim(projections)[1]), rep("Yahoo",dim(projections)[1]), rep("Average",dim(projections)[1])) #,rep("Latent",dim(projections)[1])
densityData <- data.frame(pointDensity, sourceDensity)

ggplot(densityData, aes(x=pointDensity, fill=sourceDensity)) + geom_density(alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Projected Points") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Calculate projections.jpg", sep=""))
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/LeagueProjections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/LeagueProjections.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Projections/LeagueProjections-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/LeagueProjections-2014.csv", sep=""), row.names=FALSE)
