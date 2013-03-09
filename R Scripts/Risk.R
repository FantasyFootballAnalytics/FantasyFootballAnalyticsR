###########################
# File: Risk.R
# Description: Calculates players' risk level
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These projections are from last year (ESPN and CBS have not yet updated them for the upcoming season)
# -ESPN projections do not include fumbles!
###########################

#Load libraries
library("XML")

#Load data
load(paste(getwd(),"/Data/LeagueProjections-2012.RData", sep=""))

#Risk
drafts <- readHTMLTable("http://fantasyfootballcalculator.com/adp.php?teams=10", stringsAsFactors = FALSE)$`NULL`
drafts$sdPick <- as.numeric(drafts$Std.Dev)
drafts$pick <- as.numeric(drafts$Overall)
drafts$name <- drafts$Name
drafts <- drafts[,c("name","pick","sdPick")]

#Change player names
drafts[which(drafts$name=="Robert Griffin"),"name"] <- "Robert Griffin III"
drafts[which(drafts$name=="Stevie Johnson"),"name"] <- "Steve Johnson"

projections <- merge(projections, drafts, by="name", all.x=TRUE)
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:max(as.numeric(row.names(projections)))

#Calculate risk
projections$sdPts <- apply(projections[,c("projectedPts_espn","projectedPts_cbs","projectedPts_nfl")],1,sd)
projections$sdPickZ <- scale(projections$sdPick)
projections$sdPtsZ <- scale(projections$sdPts)
projections$risk <- rowMeans(projections[,c("sdPickZ","sdPtsZ")], na.rm=TRUE)

#Rescale risk with mean~5 and sd~2
projections$risk <- ((projections$risk * 2/(sd(projections$risk, na.rm=TRUE))) + (5-(mean(projections$risk, na.rm=TRUE))))

#Remove duplicate cases
projections[duplicated(projections$name),]

#Drop variables
projections <- projections[,!(names(projections) %in% c("sdPickZ","sdPtsZ"))]

#Density plot
ggplot(projections, aes(x=risk)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Density Plot of Players' Risk Levels")
ggsave(paste(getwd(),"/Figures/Risk.jpg", sep=""))

#Save file
save(projections, file = paste(getwd(),"/Data/Risk-2012.RData", sep=""))