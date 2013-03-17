###########################
# File: Risk.R
# Description: Calculates players' risk level
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These projections are from last year (ESPN and CBS have not yet updated them for the upcoming season)
# -ESPN projections do not include fumbles!
# To do:
# -Evaluate accuracy of projections while taking into account risk
# -Add FantasyPros to sdPts calculation
###########################

#Load libraries
library("XML")
library("stringr")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))

#Load data
#load(paste(getwd(),"/Data/LeagueProjections-2012.RData", sep=""))
load(paste(getwd(),"/Data/projectedWithActualPoints-2012.RData", sep=""))

projections <- projectedWithActualPts

#Risk - "Experts"
experts <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php", stringsAsFactors = FALSE)$data
experts$sdPick_experts <- as.numeric(experts[,"Std Dev"])
experts$pick_experts <- as.numeric(experts$Ave)
experts$name <- str_sub(experts[,c("Player (pos, team)")], end=str_locate(experts[,c("Player (pos, team)")], '\\(')[,1]-2)
experts[grep("Beanie", experts[,c("Player (pos, team)")]),"name"] <- "Beanie Wells"
experts <- experts[c("name","pick_experts","sdPick_experts")]

#Risk - Wisdom of the Crowd
drafts <- readHTMLTable("http://fantasyfootballcalculator.com/adp.php?teams=10", stringsAsFactors = FALSE)$`NULL`
drafts$sdPick_crowd <- as.numeric(drafts$Std.Dev)
drafts$pick_crowd <- as.numeric(drafts$Overall)
drafts$name <- drafts$Name
drafts <- drafts[,c("name","pick_crowd","sdPick_crowd")]

#Change player names
drafts[which(drafts$name=="Robert Griffin"),"name"] <- "Robert Griffin III"
drafts[which(drafts$name=="Stevie Johnson"),"name"] <- "Steve Johnson"

#Merge files
risk <- merge(experts, drafts, by="name", all=TRUE)
projections <- merge(projections, risk, by="name", all.x=TRUE)
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:max(as.numeric(row.names(projections)))

#Calculate risk
projections$pick <- rowMeans(projections[,c("pick_experts","pick_crowd")], na.rm=TRUE)
projections$sdPts <- apply(projections[,c("projectedPts_espn","projectedPts_cbs","projectedPts_nfl")],1,sd) #add FantasyPros
projections$sdPick <- rowMeans(projections[,c("sdPick_experts","sdPick_crowd")], na.rm=TRUE)
projections$sdPickZ <- scale(projections$sdPick)
projections$sdPtsZ <- scale(projections$sdPts)
projections$risk <- rowMeans(projections[,c("sdPickZ","sdPtsZ")], na.rm=TRUE)

#Rescale risk with mean~5 and sd~2
projections$risk <- ((projections$risk * 2/(sd(projections$risk, na.rm=TRUE))) + (5-(mean(projections$risk, na.rm=TRUE))))

#Remove duplicate cases
projections[duplicated(projections$name),]

#Drop variables
projections <- projections[,!(names(projections) %in% c("pick_experts","sdPick_experts","pick_crowd","sdPick_crowd","sdPickZ","sdPtsZ"))]

#Evaluate accuracy of projections while taking into account risk
summary(lm(actualPts ~ projectedPtsLatent + risk, data=projections))$r.squared

#Density plot
ggplot(projections, aes(x=risk)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Density Plot of Players' Risk Levels")
ggsave(paste(getwd(),"/Figures/Risk.jpg", sep=""))

#Save file
save(projections, file = paste(getwd(),"/Data/Risk-2012.RData", sep=""))