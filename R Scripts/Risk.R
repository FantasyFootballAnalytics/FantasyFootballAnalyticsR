###########################
# File: Risk.R
# Description: Calculates players' risk level
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
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
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Load data
#load(paste(getwd(),"/Data/LeagueProjections-2014.RData", sep=""))
load(paste(getwd(),"/Data/projectedWithActualPoints-2014.RData", sep=""))

projections <- projectedWithActualPts

#Risk - "Experts"
experts <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php", stringsAsFactors = FALSE)$data
experts$sdPick_experts <- as.numeric(experts[,"Std Dev"])
experts$pick_experts <- as.numeric(experts$Ave)
#experts$name <- str_sub(experts[,c("Player, pos (team/bye)")], end=str_locate(experts[,c("Player, pos (team/bye)")], '\\(')[,1]-2)
#experts$name <- str_sub(experts[,c("Player, pos (team/bye)")], end=str_locate(experts[,c("Player, pos (team/bye)")], ',')[,1]-1)
experts$name <- str_sub(experts[,c("Player (team/bye)")], end=str_locate(experts[,c("Player (team/bye)")], '\\(')[,1]-2)

#Rename Players
experts[grep("Beanie", experts[,c("Player (team/bye)")]),"name"] <- "Beanie Wells"
experts[grep("Ty Hilton", experts[,c("Player (team/bye)")]),"name"] <- "T.Y. Hilton"
experts[grep("Robert Housler", experts[,c("Player (team/bye)")]),"name"] <- "Rob Housler"
experts[grep("Reuben Randle", experts[,c("Player (team/bye)")]),"name"] <- "Rueben Randle"
experts[grep("Joseph Morgan", experts[,c("Player (team/bye)")]),"name"] <- "Joe Morgan"
experts[grep("Christopher Ivory", experts[,c("Player (team/bye)")]),"name"] <- "Chris Ivory"

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

projections$sdPts <- NA
for (i in 1:dim(projections)[1]){
  projections$sdPts[i] <- sd(projections[i,c("projectedPts_espn","projectedPts_cbs","projectedPts_nfl","projectedPts_fp")], na.rm=TRUE)
}

projections$sdPick <- rowMeans(projections[,c("sdPick_experts","sdPick_crowd")], na.rm=TRUE)
projections$sdPts[projections$sdPts == 0] <- NA
projections$sdPickZ <- scale(projections$sdPick)
projections$sdPtsZ <- scale(projections$sdPts)
projections$risk <- rowMeans(projections[,c("sdPickZ","sdPtsZ")], na.rm=TRUE)

#Rescale risk with mean~5 and sd~2
projections$risk <- ((projections$risk * 2/(sd(projections$risk, na.rm=TRUE))) + (5-(mean(projections$risk, na.rm=TRUE))))

#Remove duplicate cases
projections[duplicated(projections$name),]

#Drop variables
projections <- projections[,!(names(projections) %in% c("pick_experts","sdPick_experts","pick_crowd","sdPick_crowd","sdPickZ","sdPtsZ"))]

#Compare accuracy of projections while taking into account risk vs when not taking risk into account
summary(lm(actualPts ~ projections, data=na.omit(projections[,c("actualPts","projections","risk")])))$r.squared #not considering risk #projectedPtsLatent
summary(lm(actualPts ~ projections + risk, data=na.omit(projections[,c("actualPts","projections","risk")])))$r.squared #considering risk #projectedPtsLatent
summary(lm(actualPts ~ projections + risk, data=na.omit(projections[,c("actualPts","projections","risk")])))

#Players with highest risk levels
projections[rank(projections$risk, na.last="keep") %in% (max(rank(projections$risk, na.last="keep"), na.rm=TRUE)-5):max(rank(projections$risk, na.last="keep"), na.rm=TRUE) ,]

#Density plot
ggplot(projections, aes(x=risk)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Density Plot of Players' Risk Levels")
ggsave(paste(getwd(),"/Figures/Risk 2014.jpg", sep=""))
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/Risk-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/CSV/Risk-2014.csv", sep=""), row.names=FALSE)
