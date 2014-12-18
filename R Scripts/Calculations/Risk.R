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
library("data.table")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Load data
load(paste(getwd(),"/Data/LeagueProjections.RData", sep=""))
load(paste(getwd(),"/Data/wisdomOfTheCrowd.RData", sep=""))

#Risk - "Experts"
experts <- data.table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php", stringsAsFactors = FALSE)$data)
experts[,sdPick_experts := as.numeric(get("Std Dev"))]
experts[,pick_experts := as.numeric(Avg)]
experts[,player := str_sub(get("Player (team/bye)"), end=str_locate(get("Player (team/bye)"), "\\(")[,1]-2)]
experts$name <- nameMerge(experts$player)

#Rename Players
#experts[experts$name=="DOMANIQUEDAVIS", "name"] <- "DOMINIQUEDAVIS"

experts <- experts[,c("name","pick_experts","sdPick_experts"), with=FALSE]

#Risk - Wisdom of the Crowd
wisdomOfTheCrowd[,pick_crowd := mean]
wisdomOfTheCrowd[,sdPick_crowd := mad]
wisdomOfTheCrowd <- wisdomOfTheCrowd[,c("name","pick_crowd","sdPick_crowd"), with=FALSE]

#drafts <- readHTMLTable("http://fantasyfootballcalculator.com/adp.php?teams=10", stringsAsFactors = FALSE)$`NULL`
#drafts$sdPick_crowd <- as.numeric(drafts$Std.Dev)
#drafts$pick_crowd <- as.numeric(drafts$Overall)
#drafts$name <- drafts$Name
#drafts <- drafts[,c("name","pick_crowd","sdPick_crowd")]

#Change player names
#drafts[which(drafts$name=="Robert Griffin"),"name"] <- "Robert Griffin III"
#drafts[which(drafts$name=="Stevie Johnson"),"name"] <- "Steve Johnson"

#Merge files
risk <- merge(experts, wisdomOfTheCrowd, by="name", all=TRUE)
projections <- merge(projections, risk, by="name", all.x=TRUE)

#Calculate risk
projections[,pick := rowMeans(projections[,c("pick_experts", "pick_crowd"), with=FALSE], na.rm=TRUE)]

projections[-which(sourceName %in% c("average","averageRobust","averageWeighted")), sdPts := sd(points), by=c("name","player","pos","team","playerID")]
projections[,sdPts := mean(sdPts, na.rm=TRUE), by=c("name","player","pos","team","playerID")]

projections[,sdPick := rowMeans(projections[,c("sdPick_experts","sdPick_crowd"), with=FALSE], na.rm=TRUE)]
projections$sdPts[which(projections$sdPts == 0)] <- NA

#Standardize risk by position
projections[,sdPickZ := scale(sdPick), by="pos"]
projections[,sdPtsZ := scale(sdPts), by="pos"]
projections[,risk := rowMeans(projections[,c("sdPickZ","sdPtsZ"), with=FALSE], na.rm=TRUE)]

#Rescale risk with mean~5 and sd~2
projections[,risk := ((risk * 2/(sd(risk, na.rm=TRUE))) + (5-(mean(risk, na.rm=TRUE))))]

#Select and order variables
newVars <- c("pick","risk","sdPts","sdPick")
allVars <- c(finalVarNames, newVars)
keepVars <- finalVarNames[finalVarNames %in% names(projections)]
projections <- projections[,keepVars, with=FALSE]

#Players with highest risk levels
projections[rank(projections$risk, na.last="keep") %in% (max(rank(projections$risk, na.last="keep"), na.rm=TRUE)-5):max(rank(projections$risk, na.last="keep"), na.rm=TRUE) ,]

#Density plot
ggplot(projections, aes(x=risk)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Density Plot of Players' Risk Levels")
ggsave(paste(getwd(),"/Figures/Risk.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste0(getwd(), "/Data/Risk.RData"))
write.csv(projections, file = paste0(getwd(), "/Data/Risk.csv"), row.names=FALSE)

save(projections, file = paste0(getwd(), "/Data/Historical Files/Risk-", season, ".RData"))
write.csv(projections, file = paste0(getwd(), "/Data/Historical Files/Risk-", season, ".csv"), row.names=FALSE)
