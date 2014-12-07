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
projectionsRobustAvg <- projections[which(sourceName == "averageRobust"),]

qbValueOfReplacement <- mean(c(projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "QB" & projectionsRobustAvg$positionRank == qbReplacements)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "QB" & projectionsRobustAvg$positionRank == qbReplacements-1)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "QB" & projectionsRobustAvg$positionRank == qbReplacements+1)]))
rbValueOfReplacement <- mean(c(projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "RB" & projectionsRobustAvg$positionRank == rbReplacements)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "RB" & projectionsRobustAvg$positionRank == rbReplacements-1)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "RB" & projectionsRobustAvg$positionRank == rbReplacements+1)]))
wrValueOfReplacement <- mean(c(projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "WR" & projectionsRobustAvg$positionRank == wrReplacements)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "WR" & projectionsRobustAvg$positionRank == wrReplacements-1)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "WR" & projectionsRobustAvg$positionRank == wrReplacements+1)]))
teValueOfReplacement <- mean(c(projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "TE" & projectionsRobustAvg$positionRank == teReplacements)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "TE" & projectionsRobustAvg$positionRank == teReplacements-1)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "TE" & projectionsRobustAvg$positionRank == teReplacements+1)]))

projections[which(pos == "QB"), vor := points - qbValueOfReplacement]
projections[which(pos == "RB"), vor := points - rbValueOfReplacement]
projections[which(pos == "WR"), vor := points - wrValueOfReplacement]
projections[which(pos == "TE"), vor := points - teValueOfReplacement]

#Calculate ranks by VOR
projections <- projections[order(-vor)][,overallRank := 1:.N, by=list(sourceName)]
projections <- projections[order(-vor)][,positionRank := 1:.N, by=list(sourceName, pos)]

#Select and order variables
keepVars <- finalVarNames[finalVarNames %in% names(projections)]
projections <- projections[,keepVars, with=FALSE]

#Starters (low risk)
projections[which(projections$risk <= 5 & projections$vor >= 0),]

#Sleepers (high risk)
projections[which(projections$risk >=5 & projections$vor >= 0),]

#Density Plot
ggplot(projections[which(projections$vor >= 0),], aes(x=vor, fill=pos)) + geom_density(alpha=.3) + xlab("Player's Value Over Replacement") + ggtitle("Density Plot of Projected VOR") + theme(legend.title=element_blank())
ggsave(paste0(getwd(), "/Figures/VOR-Density.jpg"), width=10, height=10)
dev.off()

#Boxplot
qplot(pos, vor, data=projections[which(projections$vor >= 0),], geom=c("boxplot", "jitter"), fill=pos, main="Value Over Replacement By Position", xlab="", ylab="Value Over Replacement")
ggsave(paste0(getwd(), "/Figures/VOR-Boxplot.jpg"), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste0(getwd(), "/Data/VOR.RData"))
write.csv(projections, file = paste0(getwd(), "/Data/VOR.csv"), row.names=FALSE)

save(projections, file = paste0(getwd(), "/Data/Historical Files/VOR-", season, ".RData"))
write.csv(projections, file = paste0(getwd(), "/Data/Historical Files/VOR-", season, ".csv"), row.names=FALSE)

#Subset data
draftData <- projections[as.numeric(row.names(na.omit(projections[,c("points","vor","risk"), with=FALSE]))), c("name","pos","team","points","vor","sdPick","sdPts","risk"), with=FALSE] #projectedPtsLatent
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
