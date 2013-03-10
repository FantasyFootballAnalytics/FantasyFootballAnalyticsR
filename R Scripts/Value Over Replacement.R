###########################
# File: Value Over Replacement.R
# Description: Calculates a Player's Value Over a Typical Replacement Starter (for Snake Drafts)
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These projections are from last year (they have not yet been updated for the upcoming season)
###########################

#League settings
numTeams <- 10  #number of teams in league
numQB <- 1      #number of avg QBs in starting lineup
numRB <- 2.5    #number of avg RBs in starting lineup
numWR <- 2.5    #number of avg WRs in starting lineup
numTE <- 1      #number of avg TEs in starting lineup

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))

#Load data
load(paste(getwd(),"/Data/Risk-2012.RData", sep=""))

#Calculate Value over Replacement
qb <- projections[projections$pos=="QB",][order(projections[projections$pos=="QB",]$overallRank),]
rb <- projections[projections$pos=="RB",][order(projections[projections$pos=="RB",]$overallRank),]
wr <- projections[projections$pos=="WR",][order(projections[projections$pos=="WR",]$overallRank),]
te <- projections[projections$pos=="TE",][order(projections[projections$pos=="TE",]$overallRank),]

qb$positionRank <- rank(-qb$projectedPtsLatent, ties.method="min")
rb$positionRank <- rank(-rb$projectedPtsLatent, ties.method="min")
wr$positionRank <- rank(-wr$projectedPtsLatent, ties.method="min")
te$positionRank <- rank(-te$projectedPtsLatent, ties.method="min")

qbReplacements <- print(ceiling(numQB*numTeams*1.5))
rbReplacements <- print(ceiling(numRB*numTeams*1.5))
wrReplacements <- print(ceiling(numWR*numTeams*1.5))
teReplacements <- print(ceiling(numTE*numTeams*1.5))

qbValueOfReplacement <- print(mean(c(qb$projectedPts[qb$positionRank==qbReplacements],qb$projectedPts[qb$positionRank==(qbReplacements-1)],qb$projectedPts[qb$positionRank==(qbReplacements+1)])))
rbValueOfReplacement <- print(mean(c(rb$projectedPts[rb$positionRank==rbReplacements],rb$projectedPts[rb$positionRank==(rbReplacements-1)],rb$projectedPts[rb$positionRank==(rbReplacements+1)])))
wrValueOfReplacement <- print(mean(c(wr$projectedPts[wr$positionRank==wrReplacements],wr$projectedPts[wr$positionRank==(wrReplacements-1)],wr$projectedPts[wr$positionRank==(wrReplacements+1)])))
teValueOfReplacement <- print(mean(c(te$projectedPts[te$positionRank==teReplacements],te$projectedPts[te$positionRank==(teReplacements-1)],te$projectedPts[te$positionRank==(teReplacements+1)])))

qb$vor <- qb$projectedPts - qbValueOfReplacement
rb$vor <- rb$projectedPts - rbValueOfReplacement
wr$vor <- wr$projectedPts - wrValueOfReplacement
te$vor <- te$projectedPts - teValueOfReplacement

#Merge across positions
projections <- rbind(qb,rb,wr,te)

#Calculate overall rank by VOR
projections$overallRank <- rank(-projections$vor, ties.method="min")

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]

#Reorder variables
projections <- projections[,c("name","pos","team","overallRank","pick","positionRank","projectedPts_espn","projectedPts_cbs","projectedPts_nfl","projectedPts","projectedPtsLatent","vor","sdPick","sdPts","risk")]

#Starters (low risk)
projections[which(projections$risk <= 5 & projections$vor >= 0),]

#Sleepers (high risk)
projections[which(projections$risk >=5 & projections$vor >= 0),]

#Density Plot
ggplot(projections, aes(x=vor, fill=pos)) + geom_density(alpha=.3) + xlab("Player's Value Over Replacement") + ggtitle("Density Plot of Projected VOR from 2012") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/VOR-Density 2012.jpg", sep=""))

# Boxplot
qplot(pos, vor, data=projections, geom=c("boxplot", "jitter"), fill=pos, main="Value Over Replacement By Position", xlab="", ylab="Value Over Replacement")
ggsave(paste(getwd(),"/Figures/VOR-Boxplot 2012.jpg", sep=""))

#Save file
save(projections, file = paste(getwd(),"/Data/VOR-2012.RData", sep=""))