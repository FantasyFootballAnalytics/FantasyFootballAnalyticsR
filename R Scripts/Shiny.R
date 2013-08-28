###########################
# File: Shiny.R
# Description: Prepare Shiny Data
# Date: 6/10/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
###########################

#Libraries
library("stringr")
library("XML")

#Source Files
source(paste(getwd(),"/R Scripts/ESPN Projections.R", sep=""), echo=TRUE) #need for calculating risk (sd)
source(paste(getwd(),"/R Scripts/CBS Projections.R", sep=""), echo=TRUE) #need for calculating risk (sd)
source(paste(getwd(),"/R Scripts/NFL Projections.R", sep=""), echo=TRUE) #need for calculating risk (sd)
source(paste(getwd(),"/R Scripts/FantasyPros Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculate League Projections.R", sep=""), echo=TRUE) #for Risk.R
source(paste(getwd(),"/R Scripts/Evaluate Projections.R", sep=""), echo=TRUE) #for Risk.R
source(paste(getwd(),"/R Scripts/Risk.R", sep=""), echo=TRUE)

#Load data
load(paste(getwd(),"/Data/FantasyPros-Projections-2013.RData", sep=""))

#Risk Data
riskData <- projections[,c("name","risk")]

#Avg & Projected Cost
avgcost <- readHTMLTable("http://www.fantasypros.com/nfl/auction-values/overall.php", stringsAsFactors = FALSE)$data

###Fantasy Pros
avgcost$name <- str_sub(avgcost[,c("Player (pos, team, bye)")], end=str_locate(avgcost[,c("Player (pos, team, bye)")], ',')[,1]-1)
avgcost$cost <- as.numeric(sub("\\$","", avgcost$Ave))
#avgcost$cost <- as.numeric(sub("\\$","", avgcost$Max)) ###IMPORTANT: Avg cost for FantasyPros is messed up bc they have a source with cost of zeros for all players -- use max instead
avgcost <- avgcost[,c("name","cost")]

#Rename Players
avgcost[grep("Beanie", avgcost[,c("name")]),"name"] <- "Beanie Wells"
avgcost[grep("Ty Hilton", avgcost[,c("name")]),"name"] <- "T.Y. Hilton"
avgcost[grep("Robert Housler", avgcost[,c("name")]),"name"] <- "Rob Housler"
#avgcost[grep("Reuben Randle", avgcost[,c("name")]),"name"] <- "Rueben Randle"
#avgcost[grep("Joseph Morgan", avgcost[,c("name")]),"name"] <- "Joe Morgan"
avgcost[grep("Christopher Ivory", avgcost[,c("name")]),"name"] <- "Chris Ivory"

#Merge
projections <- merge(projections_fp, riskData, by="name", all.x=TRUE)
projections <- merge(projections, avgcost, by="name", all.x=TRUE)
projections$cost[is.na(projections$cost)==TRUE] <- 0

#Remove duplicate cases
projections[duplicated(projections$name),"name"]
projections[projections$name %in% projections[duplicated(projections$name),"name"],]

#Modify variable names
projections$team <- projections$team_fp
projections$passYds <- projections$passYds_fp
projections$passTds <- projections$passTds_fp
projections$passInt <- projections$passInt_fp
projections$rushYds <- projections$rushYds_fp
projections$rushTds <- projections$rushTds_fp
projections$rec <- projections$rec_fp
projections$recYds <- projections$recYds_fp
projections$recTds <- projections$recTds_fp
projections$twoPts <- projections$twoPts_fp
projections$fumbles <- projections$fumbles_fp
projections$points <- projections$pts_fp

#Subset data to keep players with no missing values for name/pos/points/risk/cost
shinyData1 <- projections[,c("name","pos","team","passYds","passTds","passInt","rushYds","rushTds","rec","recYds","recTds","twoPts","fumbles","points","cost","risk")]

shinyData2 <- na.omit(projections[,c("name","pos","points","cost","risk")])

shinyData <- shinyData1[shinyData1$name %in% shinyData2$name,]

#Calculate Position Rank
shinyData$positionRank[shinyData$pos=="QB"] <- rank(-shinyData$points[shinyData$pos=="QB"], ties.method="min")
shinyData$positionRank[shinyData$pos=="RB"] <- rank(-shinyData$points[shinyData$pos=="RB"], ties.method="min")
shinyData$positionRank[shinyData$pos=="WR"] <- rank(-shinyData$points[shinyData$pos=="WR"], ties.method="min")
shinyData$positionRank[shinyData$pos=="TE"] <- rank(-shinyData$points[shinyData$pos=="TE"], ties.method="min")

#Calculate Overall Rank
shinyData$overallRank <- rank(-shinyData$points, ties.method="min")

#Order Data by Overall Rank
shinyData <- shinyData[order(-shinyData$points),]
row.names(shinyData) <- 1:dim(shinyData)[1]

#Select Variables to Keep
shinyData <- shinyData[,c("name","pos","team","overallRank","positionRank","passYds","passTds","passInt","rushYds","rushTds","rec","recYds","recTds","twoPts","fumbles","cost","risk")] #,"points"

#Save file
save(shinyData, file = paste(getwd(),"/Data/shinyData-2013.RData", sep=""))
write.csv(shinyData, file=paste(getwd(),"/Data/CSV/shinyData-2013.csv", sep=""), row.names=FALSE)
write.csv(shinyData, file=paste(getwd(),"/shinyapp/shinyData.csv", sep=""), row.names=FALSE)
