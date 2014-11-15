###########################
# File: Value Over Replacement.R
# Description: Calculates a Player's Value Over a Typical Replacement Starter (for Snake Drafts)
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/Risk.RData", sep=""))

#Calculate Value over Replacement
qb <- projections[projections$pos=="QB",][order(projections[projections$pos=="QB",]$overallRank),]
rb <- projections[projections$pos=="RB",][order(projections[projections$pos=="RB",]$overallRank),]
wr <- projections[projections$pos=="WR",][order(projections[projections$pos=="WR",]$overallRank),]
te <- projections[projections$pos=="TE",][order(projections[projections$pos=="TE",]$overallRank),]

qb$positionRank <- rank(-qb$projections, ties.method="min") #projectedPtsLatent
rb$positionRank <- rank(-rb$projections, ties.method="min") #projectedPtsLatent
wr$positionRank <- rank(-wr$projections, ties.method="min") #projectedPtsLatent
te$positionRank <- rank(-te$projections, ties.method="min") #projectedPtsLatent

qbValueOfReplacement <- print(mean(c(qb$projections[qb$positionRank==qbReplacements],qb$projections[qb$positionRank==(qbReplacements-1)],qb$projections[qb$positionRank==(qbReplacements+1)]))) #projectedPtsLatent
rbValueOfReplacement <- print(mean(c(rb$projections[rb$positionRank==rbReplacements],rb$projections[rb$positionRank==(rbReplacements-1)],rb$projections[rb$positionRank==(rbReplacements+1)])))
wrValueOfReplacement <- print(mean(c(wr$projections[wr$positionRank==wrReplacements],wr$projections[wr$positionRank==(wrReplacements-1)],wr$projections[wr$positionRank==(wrReplacements+1)])))
teValueOfReplacement <- print(mean(c(te$projections[te$positionRank==teReplacements],te$projections[te$positionRank==(teReplacements-1)],te$projections[te$positionRank==(teReplacements+1)])))

qb$vor <- qb$projections - qbValueOfReplacement
rb$vor <- rb$projections - rbValueOfReplacement
wr$vor <- wr$projections - wrValueOfReplacement
te$vor <- te$projections - teValueOfReplacement

#Merge across positions
projections <- rbind(qb,rb,wr,te)

#Calculate overall rank by VOR
projections$overallRank <- rank(-projections$vor, ties.method="min")

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]

#Reorder variables
projections <- projections[,c("name","player","pos","team","overallRank","pick","positionRank","projections",paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"),"projectedPtsMean","projectedPtsMedian","vor","sdPick","sdPts","risk")] #,"projectedPtsLatent"

#Starters (low risk)
projections[which(projections$risk <= 5 & projections$vor >= 0),]

#Sleepers (high risk)
projections[which(projections$risk >=5 & projections$vor >= 0),]

#Density Plot
ggplot(projections[which(projections$vor >= 0),], aes(x=vor, fill=pos)) + geom_density(alpha=.3) + xlab("Player's Value Over Replacement") + ggtitle("Density Plot of Projected VOR") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/VOR-Density.jpg", sep=""), width=10, height=10)
dev.off()

#Boxplot
qplot(pos, vor, data=projections[which(projections$vor >= 0),], geom=c("boxplot", "jitter"), fill=pos, main="Value Over Replacement By Position", xlab="", ylab="Value Over Replacement")
ggsave(paste(getwd(),"/Figures/VOR-Boxplot.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/VOR.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/VOR.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Files/VOR-2015.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Files/VOR-2015.csv", sep=""), row.names=FALSE)

#Subset data
draftData <- projections[row.names(na.omit(projections[,c("projections","vor","risk")])),c("name","pos","team","projections","vor","sdPick","sdPts","risk")] #projectedPtsLatent
row.names(draftData) <- 1:dim(draftData)[1]

options(digits=2)
draftData

#Example: Update with drafted (i.e., unavailable) players
drafted <- c("Arian Foster","Ray Rice")

draftData[!(draftData$name %in% drafted),]

###Draft Dashboard
drafted <- c("")

#All players
draftData[!(draftData$name %in% drafted),]

#Starters (low risk)
draftData[!(draftData$name %in% drafted) & draftData$risk <=4,]

#Sleepers (high risk)
draftData[!(draftData$name %in% drafted) & draftData$risk >=6,]
