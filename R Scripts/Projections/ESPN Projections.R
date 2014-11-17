###########################
# File: ESPN Projections.R
# Description: Downloads Fantasy Football Projections from ESPN.com
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")
library("data.table")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Projection Info
season <- 2015
suffix <- "espn"

#Download fantasy football projections from ESPN.com
espn_base_url <- paste0("http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=", season, "&scoringPeriodId=")
espn_pos <- list(QB=0, RB=2, WR=4, TE=6, K=17, DST=16)
espn_pages <- c("0","40","80")
espn_urls <- paste0(espn_base_url, "&slotCategoryId=", rep(espn_pos, each=length(espn_pages)), "&startIndex=", espn_pages)

#Scrape
espn <- lapply(espn_urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0)})
espnList <- espn

#Clean data
qbNames <- rbNames <- wrNames <- teNames <- kNames <- dstNames <- c("rank","player","passCompAtt","passYds","passTds","passInt","rushAtt","rushYds","rushTds","rec","recYds","recTds","points")

for(i in 1:length(espnList)) {
  if(nrow(espnList[[i]]) > 0){
    #Add position to projection
    espnList[[i]][,pos := rep(names(espn_pos), each=length(espn_pages))[i]]
    espnList[[i]][,pos := as.factor(pos)]
    
    #Trim dimensions  
    espnList[[i]] <- espnList[[i]][2:nrow(espnList[[i]])]
    
    #Add variable names
    if(unique(espnList[[i]][,pos]) == "QB"){
      setnames(espnList[[i]], c(qbNames, "pos"))
    } else if(unique(espnList[[i]][,pos]) == "RB"){
      setnames(espnList[[i]], c(rbNames, "pos"))
    } else if(unique(espnList[[i]][,pos]) == "WR"){
      setnames(espnList[[i]], c(wrNames, "pos"))
    } else if(unique(espnList[[i]][,pos]) == "TE"){
      setnames(espnList[[i]], c(teNames, "pos"))
    } else if(unique(espnList[[i]][,pos]) == "K"){
      setnames(espnList[[i]], c(kNames, "pos"))
    } else if(unique(espnList[[i]][,pos]) == "DST"){
      setnames(espnList[[i]], c(dstNames, "pos"))
    }
  }
}

#Merge
projections_espn <- rbindlist(espnList, use.names=TRUE, fill=TRUE)

#Replace symbols with value of zero
projections_espn[which(passCompAtt == "--/--"), passCompAtt := "0/0"]
projections_espn[which(passYds == "--"), passYds := "0"]
projections_espn[which(passTds == "--"), passTds := "0"]
projections_espn[which(passInt == "--"), passInt := "0"]
projections_espn[which(rushAtt == "--"), rushAtt := "0"]
projections_espn[which(rushYds == "--"), rushYds := "0"]
projections_espn[which(rushTds == "--"), rushTds := "0"]
projections_espn[which(rec == "--"), rec := "0"]
projections_espn[which(recYds == "--"), recYds := "0"]
projections_espn[which(recTds == "--"), recTds := "0"]
projections_espn[which(points == "--"), points := "0"]

#Separate pass completions from attempts
projections_espn[, passComp := str_sub(string=passCompAtt, end=str_locate(string=passCompAtt, '/')[,1]-1)]
projections_espn[, passAtt := str_sub(string=passCompAtt, start=str_locate(string=passCompAtt, '/')[,1]+1)]

#Convert variables from character strings to numeric
numericVars <- names(projections_espn)[names(projections_espn) %in% scoreCategories]
projections_espn[, (numericVars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = numericVars]

#Player teams
projections_espn[,team_espn := str_sub(player, start=str_locate(string=player, ',')[,1]+2, end = str_locate(string=player, ',')[,1]+4)]
projections_espn[,team_espn := str_trim(projections_espn$team_espn, side="right")]
projections_espn[which(pos == "DST"), team_espn := convertTeamAbbreviation(str_sub(projections_espn$player[which(projections_espn$pos == "DST")], end=str_locate(string=projections_espn$player[which(projections_espn$pos == "DST")], " ")[,1]-1))]
projections_espn[,team_espn := cleanTeamAbbreviations(toupper(projections_espn$team_espn))]

#Player names
projections_espn[,name_espn := str_sub(player, end=str_locate(string=player, ',')[,1]-1)]
projections_espn[,name_espn := str_replace_all(name_espn, "\\*", "")]
projections_espn[which(pos == "DST"), name_espn := convertTeamName(projections_espn$team_espn[which(projections_espn$pos == "DST")])]
projections_espn[,name := nameMerge(projections_espn$name_espn)]

#Remove duplicate cases
duplicateCases <- projections_espn[duplicated(name)]$name
projections_espn[which(name %in% duplicateCases),]

#Same name, different player
#projections_espn <- projections_espn[-which(name == "ALEXSMITH" & team_espn == "CIN"),]
#projections_espn <- projections_espn[-which(name == "RYANGRIFFIN" & team_espn == "NO"),]
#projections_espn <- projections_espn[-which(name == "ZACHMILLER" & team_espn == "CHI"),]

#Same player, different position
#dropNames <- c("DEXTERMCCLUSTER")
#dropVariables <- c("pos")
#dropLabels <- c("WR")

#projections_espn2 <- setDT(ddply(projections_espn, .(name), numcolwise(mean), na.rm=TRUE))

#for(i in 1:length(dropNames)){
#  if(dim(projections_espn[-which(name == dropNames[i] & projections_espn[,dropVariables[i], with=FALSE] == dropLabels[i]),])[1] > 0){
#    projections_espn <- projections_espn[-which(name == dropNames[i] & projections_espn[,dropVariables[i], with=FALSE] == dropLabels[i]),]
#  }
#}

#setkeyv(projections_espn2, cols="name")
#setkeyv(projections_espn, cols="name")

#projections_espn <- merge(projections_espn2, projections_espn[,c("name","name_espn","player","pos","team_espn"), with=FALSE], by="name")

#Rename players

#Calculate Overall Rank
projections_espn <- projections_espn[order(-points)][,overallRank := 1:.N]

#Calculate Position Rank
projections_espn <- projections_espn[order(-points)][,positionRank := 1:.N, by=list(pos)]

#Add source
projections_espn$sourceName <- suffix

#Order variables in data set
allVars <- c(prefix, paste(sourceSpecific, suffix, sep="_"), varNames)
keepVars <- allVars[allVars %in% names(projections_espn)]
projections_espn <- projections_espn[,keepVars, with=FALSE]

#Order players by overall rank
projections_espn <- projections_espn[order(projections_espn$overallRank),]

#Density Plot
ggplot(projections_espn, aes(x=points)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of ESPN Projected Points")
ggsave(paste(getwd(),"/Figures/ESPN projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_espn, file = paste(getwd(),"/Data/ESPN-Projections.RData", sep=""))
write.csv(projections_espn, file=paste(getwd(),"/Data/ESPN-Projections.csv", sep=""), row.names=FALSE)

save(projections_espn, file = paste(getwd(),"/Data/Historical Projections/ESPN-Projections-2015.RData", sep=""))
write.csv(projections_espn, file=paste(getwd(),"/Data/Historical Projections/ESPN-Projections-2015.csv", sep=""), row.names=FALSE)
