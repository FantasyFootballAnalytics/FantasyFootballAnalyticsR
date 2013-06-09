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
avgcost_fp <- readHTMLTable("http://www.fantasypros.com/nfl/auction-values/overall.php", stringsAsFactors = FALSE)$data
  
###Yahoo
#readHTMLTable("http://football.fantasysports.yahoo.com/f1/35024/draftanalysis?tab=AD&pos=ALL&sort=DA_AP", stringsAsFactors = FALSE)
#avgcost <- read.csv(paste(path,"/Fantasy Football/Research/R/avgcost.csv",sep=""))
avgcost_yahoo2 <- avgcost_yahoo[which(avgcost$Avg.Cost!=""),]
avgcost_yahoo2$name <- as.character(avgcost_yahoo2$Player)
avgcost_yahoo2$avgCost <- as.numeric((str_replace_all(avgcost_yahoo2$Avg.Cost, "\\$", "")))
avgcost_yahoo3 <- avgcost_yahoo2[,c("name","avgCost")]
avgcost_yahoo3$projectedCost <- ceiling(avgcost_yahoo3$avgCost * (leagueCap/defaultCap))

#Change names
avgcost_yahoo3$name[avgcost_yahoo3$name=="Stevie Johnson"] <- "Steve Johnson"

###Fantasy Pros
avgcost_fp$name <- str_sub(avgcost_fp[,c("Player (pos, team, bye)")], end=str_locate(avgcost_fp[,c("Player (pos, team, bye)")], ',')[,1]-1)
avgcost_fp$avgCost <- as.numeric(sub("\\$","", avgcost_fp$Ave))
avgcost_fp <- avgcost_fp[,c("name","avgCost")]
avgcost_fp$projectedCost <- ceiling(avgcost_fp$avgCost * (leagueCap/defaultCap))

#Rename Players
avgcost_fp[grep("Beanie", avgcost_fp[,c("name")]),"name"] <- "Beanie Wells"
avgcost_fp[grep("Ty Hilton", avgcost_fp[,c("name")]),"name"] <- "T.Y. Hilton"
avgcost_fp[grep("Robert Housler", avgcost_fp[,c("name")]),"name"] <- "Rob Housler"
avgcost_fp[grep("Reuben Randle", avgcost_fp[,c("name")]),"name"] <- "Rueben Randle"
avgcost_fp[grep("Joseph Morgan", avgcost_fp[,c("name")]),"name"] <- "Joe Morgan"
avgcost_fp[grep("Christopher Ivory", avgcost_fp[,c("name")]),"name"] <- "Chris Ivory"

#Merge
projections <- merge(projections, avgcost_yahoo3, by="name", all.x=TRUE)
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
ggplot(projections, aes(x=inflatedCost)) + geom_density(fill="green", alpha=.3) + xlab("Player's Intrinsic Value (Cost)") + ggtitle("Density Plot of Players' Values from 2013") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Inflated Cost 2013.jpg", sep=""))
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/AvgCost-2013.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/CSV/AvgCost-2013.csv", sep=""), row.names=FALSE)
