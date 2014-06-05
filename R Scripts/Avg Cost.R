###########################
# File: Avg Cost.R
# Description: Downloads a Player's Avg Cost in Yahoo Auction Drafts
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Library
library("stringr")
library("XML")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/VOR.RData", sep=""))

#Avg & Projected Cost
avgcost_yahoo <- read.csv(paste(getwd(),"/Data/Yahoo-avgcost.csv",sep=""))
  
###Yahoo

#Variable Names
names(avgcost_yahoo) <- c("star","player","add","projected","avg","draftedPct")

#Remove special characters($, spaces, and dashes)
avgcost_yahoo[,c("projected","avg")] <- apply(avgcost_yahoo[,c("projected","avg")], 2, function(x) gsub("\\$", "", gsub(" ", "", gsub("-", "", x))))

#Convert variables from character strings to numeric
avgcost_yahoo[,c("projected","avg")] <- convert.magic(avgcost_yahoo[,c("projected","avg")], "numeric")

#Player name, position, and team
avgcost_yahoo$player[which(!is.na(avgcost_yahoo$avg))] <- avgcost_yahoo$player[which(!is.na(avgcost_yahoo$avg)) + 1]
avgcost_yahoo <- avgcost_yahoo[which(!is.na(avgcost_yahoo$avg)),]
avgcost_yahoo$name_yahoo <- str_trim(str_sub(avgcost_yahoo$player, start=0, end=str_locate(avgcost_yahoo$player, "-")[,1]-5))
avgcost_yahoo$name <- toupper(gsub("[[:punct:]]", "", gsub(" ", "", avgcost_yahoo$name_yahoo)))
avgcost_yahoo$team_yahoo <- toupper(str_trim(str_sub(avgcost_yahoo$player, start=str_locate(avgcost_yahoo$player, "-")[,1]-4, end=str_locate(avgcost_yahoo$player, "-")[,1]-2)))

#avgcost_yahoo$avgCost <- apply(avgcost_yahoo[,c("avg","projected")], 1, max, na.rm=TRUE) #Take larger of projected value and average cost
avgcost_yahoo$avgCost <- rowMeans(avgcost_yahoo[,c("avg","projected")], na.rm=TRUE) #Take mean of projected value and average cost
avgcost_yahoo <- avgcost_yahoo[,c("name","avgCost")]
avgcost_yahoo$projectedCost <- ceiling(avgcost_yahoo$avgCost * (leagueCap/defaultCap))

#Change names
#avgcost_yahoo$name[avgcost_yahoo$name=="Stevie Johnson"] <- "Steve Johnson"

#Merge
projections <- merge(projections, avgcost_yahoo, by="name", all.x=TRUE)

#Calculate Overall Rank
projections$overallRank <- rank(-projections$projections, ties.method="min")

#Remove duplicate cases
projections[projections$name %in% projections$name[duplicated(projections$name)],]

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
row.names(projections) <- 1:dim(projections)[1]

#Density Plot
ggplot(projections, aes(x=inflatedCost)) + geom_density(fill="green", alpha=.3) + xlab("Player's Intrinsic Value (Cost)") + ggtitle("Density Plot of Players' Intrinsic Value") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Inflated Cost.jpg", sep=""))
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/AvgCost.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/AvgCost.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Cost/AvgCost-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Cost/AvgCost-2014.csv", sep=""), row.names=FALSE)

