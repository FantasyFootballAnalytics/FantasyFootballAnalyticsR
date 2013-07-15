###########################
# File: Avg Cost.R
# Description: Downloads a Player's Avg Cost in Yahoo Auction Drafts
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These costs are from last year (they have not yet been updated for the upcoming season)
###########################

#Library
library("stringr")
library("XML")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/VOR-2013.RData", sep=""))

#Avg & Projected Cost
avgcost_yahoo <- read.csv(paste(getwd(),"/Data/Yahoo-avgcost-2013.csv",sep=""))
  
###Yahoo
#readHTMLTable("http://football.fantasysports.yahoo.com/f1/35024/draftanalysis?tab=AD&pos=ALL&sort=DA_AP", stringsAsFactors = FALSE)
#avgcost <- read.csv(paste(path,"/Fantasy Football/Research/R/avgcost.csv",sep=""))
avgcost_yahoo2 <- avgcost_yahoo[which(avgcost_yahoo$Avg.Cost!=""),]
avgcost_yahoo2$name <- as.character(avgcost_yahoo2$Player)
avgcost_yahoo2$avgCost <- as.numeric((str_replace_all(avgcost_yahoo2$Avg.Cost, "\\$", "")))
avgcost_yahoo3 <- avgcost_yahoo2[,c("name","avgCost")]
avgcost_yahoo3$projectedCost <- ceiling(avgcost_yahoo3$avgCost * (leagueCap/defaultCap))

#Change names
avgcost_yahoo3$name[avgcost_yahoo3$name=="Stevie Johnson"] <- "Steve Johnson"

#Merge
projections <- merge(projections, avgcost_yahoo3, by="name", all.x=TRUE)

#Calculate Overall Rank
projections$overallRank <- rank(-projections$projections, ties.method="min")

#Remove duplicate cases
projections[duplicated(projections$name),]

#Apply 10% price premium to 33 players with highest projected points, apply 10% price premium for players lower than rank 66
projections$inflatedCost <- ceiling(projections$avgCost * (leagueCap/defaultCap) * 1.0)
projections$inflatedCost[projections$overallRank <= 33] <- ceiling(projections$avgCost[projections$overallRank <= 33] * (leagueCap/defaultCap) * 1.1)
projections$inflatedCost[projections$overallRank >= 34 & projections$overallRank <= 66] <- ceiling(projections$avgCost[projections$overallRank >= 34 & projections$overallRank <= 66] * (leagueCap/defaultCap) * 1.0)
projections$inflatedCost[projections$overallRank >= 67] <- ceiling(projections$avgCost[projections$overallRank >= 67] * (leagueCap/defaultCap) * 0.9)
projections$inflatedCost[is.na(projections$inflatedCost)==TRUE] <- 1
projections$inflatedCost[projections$inflatedCost==0] <- 1

projections$avgCost[is.na(projections$avgCost)==TRUE] <- 1
projections$projectedCost[is.na(projections$projectedCost)==TRUE] <- 1
projections$inflatedCost[is.na(projections$inflatedCost)==TRUE] <- 1

#Order data
projections <- projections[order(projections$overallRank),]

#Density Plot
ggplot(projections, aes(x=inflatedCost)) + geom_density(fill="green", alpha=.3) + xlab("Player's Intrinsic Value (Cost)") + ggtitle("Density Plot of Players' Values from 2013") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Inflated Cost 2013.jpg", sep=""))
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/AvgCost-2013.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/CSV/AvgCost-2013.csv", sep=""), row.names=FALSE)
