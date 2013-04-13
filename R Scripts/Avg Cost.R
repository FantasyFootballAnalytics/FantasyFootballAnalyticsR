###########################
# File: Avg Cost.R
# Description: Downloads a Player's Avg Cost in Yahoo Auction Drafts
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These costs are from last year (they have not yet been updated for the upcoming season)
###########################

#League settings
leagueCap <- 225   #
defaultCap <- 200

#Library
library("stringr")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))

#Load data
load(paste(getwd(),"/Data/VOR-2012.RData", sep=""))

#Avg & Projected Cost
#readHTMLTable("http://football.fantasysports.yahoo.com/f1/24155/draftanalysis?tab=AD&pos=ALL&sort=DA_AP", stringsAsFactors = FALSE)
avgcost <- read.csv(paste(path,"/Fantasy Football/Research/R/avgcost.csv",sep=""))

avgcost2 <- avgcost[which(avgcost$Avg.Cost!=""),]
avgcost2$name <- as.character(avgcost2$Player)
avgcost2$avgCost <- as.numeric((str_replace_all(avgcost2$Avg.Cost, "\\$", "")))
avgcost3 <- avgcost2[,c("name","avgCost")]
avgcost3$projectedCost <- ceiling(avgcost3$avgCost * (leagueCap/defaultCap))

#Change names
avgcost3$name[avgcost3$name=="Stevie Johnson"] <- "Steve Johnson"

projections <- merge(projections, avgcost3, by="name", all.x=TRUE)
projections$projectedCost[is.na(projections$projectedCost)==TRUE] <- 1
projections <- projections[order(projections$overallRank),]

#Remove duplicate cases
projections[duplicated(projections$name),]

#Apply 10% price premium to 33 players with highest projected points, apply 10% price premium for players lower than rank 66
projections$inflatedCost <- ceiling(projections$avgCost * (leagueCap/defaultCap) * 1.0)
projections$inflatedCost[projections$overallRank <= 33] <- ceiling(projections$avgCost[projections$overallRank <= 33] * (leagueCap/defaultCap) * 1.1)
projections$inflatedCost[projections$overallRank >= 67] <- ceiling(projections$avgCost[projections$overallRank >= 67] * (leagueCap/defaultCap) * 0.9)
projections$inflatedCost[is.na(projections$inflatedCost)==TRUE] <- 1

#Density Plot
ggplot(projections, aes(x=inflatedCost)) + geom_density(fill="green", alpha=.3) + xlab("Player's Intrinsic Value (Cost)") + ggtitle("Density Plot of Players' Values from 2012") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Inflated Cost 2012.jpg", sep=""))

#Save file
save(projections, file = paste(getwd(),"/Data/AvgCost-2012.RData", sep=""))