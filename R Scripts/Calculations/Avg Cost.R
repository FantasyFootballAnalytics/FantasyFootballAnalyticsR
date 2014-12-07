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
library("data.table")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))
is.odd <- function(x) x %% 2 != 0

#Load data
load(paste(getwd(),"/Data/VOR.RData", sep=""))

###############
# Yahoo
###############

#Scrape data
yahoo_baseurl <- "http://football.fantasysports.yahoo.com/f1/draftanalysis?"
yahoo_pos <- list(QB="QB", RB="RB", WR="WR", TE="TE", K="K", DST="DEF")
yahoo_pages <- list(Cost="AD", ADP="SD")
yahoo_urls <- paste0(yahoo_baseurl, "tab=", yahoo_pages, "&pos=", rep(yahoo_pos, each=length(yahoo_pages)))
yahoo <- lapply(yahoo_urls, function(x) {data.table(readHTMLTable(x, stringsAsFactors = FALSE)[2]$draftanalysistable)})
yahooList <- yahoo

yahooListCost <- list()
yahooListADP <- list()
for(i in 1:length(yahooList)){
  #Add position to projection
  yahooList[[i]][,pos := rep(names(yahoo_pos), each=length(yahoo_pages))[i]]
  yahooList[[i]][,pos := as.factor(pos)]
  
  #Add variable names
  if(is.odd(i)){
    setnames(yahooList[[i]], c("player","costProjected_yahoo","cost_yahoo","draftedPct_yahoo","pos"))
    yahooListCost[[(i+1)/2]] <- setDT(yahooList[[i]])
  } else{
    setnames(yahooList[[i]], c("player","adp_yahoo","avgRound_yahoo","draftedPct_yahoo","pos"))
    yahooListADP[[i/2]] <- setDT(yahooList[[i]])
  }
}

#Merge
avgCost_yahoo <- rbindlist(yahooListCost, use.names=TRUE, fill=TRUE)
adp_yahoo <- rbindlist(yahooListADP, use.names=TRUE, fill=TRUE)

#Player name, position, and team
avgCost_yahoo[,player := str_trim(sapply(str_split(player, "\n"), "[[", 2))]
avgCost_yahoo[,team_yahoo := toupper(str_trim(str_sub(player, start=str_locate(player, "-")[,1]-4, end=str_locate(player, "-")[,1]-2)))]
avgCost_yahoo[,name_yahoo := str_trim(str_sub(player, start=0, end=nchar(player)-8))]
avgCost_yahoo[which(pos == "DST"), name_yahoo := convertTeamName(team_yahoo)]
avgCost_yahoo[,name := nameMerge(name_yahoo)]

adp_yahoo[,player := str_trim(sapply(str_split(player, "\n"), "[[", 2))]
adp_yahoo[,team_yahoo := cleanTeamAbbreviations(toupper(str_trim(str_sub(player, start=str_locate(player, "-")[,1]-4, end=str_locate(player, "-")[,1]-2))))]
adp_yahoo[,name_yahoo := str_trim(str_sub(player, start=0, end=nchar(player)-8))]
adp_yahoo[which(pos == "DST"), name_yahoo := convertTeamName(team_yahoo)]
adp_yahoo[,name := nameMerge(name_yahoo)]

#Merge ADP & avgCost
costADP_yahoo <- merge(avgCost_yahoo[,c("name","name_yahoo","pos","team_yahoo","costProjected_yahoo","cost_yahoo","draftedPct_yahoo"), with=FALSE], adp_yahoo[,c("name","pos","adp_yahoo","avgRound_yahoo"), with=FALSE], by=c("name","pos"), all=TRUE)

#Remove special characters
numericVars <- c("costProjected_yahoo","cost_yahoo","draftedPct_yahoo","adp_yahoo","avgRound_yahoo")
costADP_yahoo[, (numericVars) := lapply(.SD, function(x) gsub("\\%", "", gsub("\\$", "", x))), .SDcols = numericVars]

#Convert to numeric
costADP_yahoo[, (numericVars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = numericVars]

#Calculations
costADP_yahoo[,costAvg_yahoo := rowMeans(costADP_yahoo[,c("costProjected_yahoo","cost_yahoo"), with=FALSE], na.rm=TRUE)]
costADP_yahoo[,costMax_yahoo := apply(costADP_yahoo[,c("costProjected_yahoo","cost_yahoo"), with=FALSE], 1, function(x) max(x, na.rm=TRUE))]

#Rename players
#projections_yahoo[projections_yahoo$name=="STEVIEJOHNSON", "name"] <- "STEVEJOHNSON"

#Subset
costADP_yahoo <- costADP_yahoo[,c("name","name_yahoo","pos","team_yahoo","cost_yahoo","costProjected_yahoo","costAvg_yahoo","costMax_yahoo","adp_yahoo"), with=FALSE]

###############
# ESPN
###############

#Scrape data
espn_baseurl <- "http://games.espn.go.com/ffl/livedraftresults?"
espn_pos <- list(QB="QB", RB="RB", WR="WR", TE="TE", K="K", DST="D/ST", DT="DT", DE="DE", LB="LB", CB="CB", S="S")
espn_urls <- paste0(espn_baseurl, "position=", espn_pos)
espn <- lapply(espn_urls, function(x) {data.table(readHTMLTable(x, stringsAsFactors = FALSE)$`NULL`)})
espnList <- espn

for(i in 1:length(espnList)){
  #Add variable names
  setnames(espnList[[i]], c("info","player","pos","adp_espn","adp7Day","cost_espn","costAvg7Day_espn","draftedPct_espn"))
  
  #Trim Dimensions
  espnList[[i]] <- espnList[[i]][-c(1:4, nrow(espnList[[i]])),]
  
  #Add position to projection
  espnList[[i]][,pos := names(espn_pos)[i]]
  espnList[[i]][,pos := as.factor(pos)]
}

#Merge players across positions
avgCost_espn <- rbindlist(espnList, use.names=TRUE, fill=TRUE)

#Player names
avgCost_espn[,name_espn := str_sub(player, end=str_locate(string=player, ",")[,1]-1)]
avgCost_espn[,name_espn := str_replace_all(name_espn, "\\*", "")]
avgCost_espn[pos == "DST", name_espn := convertTeamName(convertTeamAbbreviation(str_trim(str_sub(player, end=str_locate(string=player, "D/ST")[,1]-1))))]
avgCost_espn[,name := nameMerge(name_espn)]

#Player teams
avgCost_espn[,team_espn := str_sub(player, start=str_locate(string=player, ",")[,1]+2, end = str_locate(string=player, ",")[,1]+4)]
avgCost_espn[,team_espn := str_trim(avgCost_espn$team_espn, side="right")]
avgCost_espn[,team_espn := toupper(team_espn)]
avgCost_espn[pos == "DST", team_espn := convertTeamAbbreviation(name_espn)]
avgCost_espn[,team_espn := cleanTeamAbbreviations(team_espn)]

#Remove special characters
numericVars <- c("adp_espn","adp7Day","cost_espn","costAvg7Day_espn","draftedPct_espn")
avgCost_espn[, (numericVars) := lapply(.SD, function(x) gsub("\\+", "", x)), .SDcols = numericVars]

#Convert to numeric
avgCost_espn[, (numericVars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = numericVars]

#Subset
avgCost_espn <- avgCost_espn[,c("name","name_espn","pos","team_espn","cost_espn","adp_espn"), with=FALSE]

###############
# FantasyPros
###############

#Scrape data
avgCost_fp <- data.table(readHTMLTable("http://www.fantasypros.com/nfl/auction-values/overall.php", stringsAsFactors = FALSE)$data)

#Clean data
avgCost_fp[,name_fp := str_sub(avgCost_fp$"Player (pos, team, bye)", end=str_locate(avgCost_fp$"Player (pos, team, bye)", "\\,")[,1]-1)]
avgCost_fp[,name := nameMerge(name_fp)]
avgCost_fp[,cost_fp := as.numeric(sub("\\$", "", Ave))]
avgCost_fp[,team_fp := cleanTeamAbbreviations(nameMerge(str_sub(avgCost_fp$"Player (pos, team, bye)", start=str_locate(avgCost_fp$"Player (pos, team, bye)", "\\(")[,1]+1, end=str_locate(avgCost_fp$"Player (pos, team, bye)", "\\(")[,1]+3)))]
avgCost_fp[,pos := as.factor(nameMerge(str_sub(avgCost_fp$"Player (pos, team, bye)", start=str_locate(avgCost_fp$"Player (pos, team, bye)", "\\,")[,1]+2, end=str_locate(avgCost_fp$"Player (pos, team, bye)", "\\,")[,1]+3)))]
avgCost_fp[,adp_fp := as.numeric(ADP)]
avgCost_fp[,ecr_fp := as.numeric(ECR)]

#Rename Players
avgCost_fp[name == "CHRISTOPHERIVORY", name := "CHRISIVORY"]
#avgCost_fp[name == "DOMANIQUEDAVIS", name := "DOMINIQUEDAVIS"]

#Subset
avgCost_fp <- avgCost_fp[,c("name","name_fp","pos","team_fp","cost_fp","adp_fp","ecr_fp"), with=FALSE]

###############
# FantasyFootballCalculator
###############

#Scrape data
avgADP_ffc <- data.table(readHTMLTable("http://fantasyfootballcalculator.com/adp.php?teams=10", stringsAsFactors = FALSE)$`NULL`)

#Clean data
avgADP_ffc[,adp_ffc := as.numeric(Overall)]
avgADP_ffc[,adpSD_ffc := as.numeric(Std.Dev)]
avgADP_ffc[,name_ffc := Name]
avgADP_ffc[,name := nameMerge(Name)]
avgADP_ffc[,pos := as.factor(Pos)]
avgADP_ffc[,team_ffc := cleanTeamAbbreviations(Team)]

#Subset
avgADP_ffc <- avgADP_ffc[,c("name","name_ffc","pos","team_ffc","adp_ffc","adpSD_ffc"), with=FALSE]

###############
# Merge
###############

sourcesOfCost <- c("yahoo","espn","fp","ffc")
costList <- list(costADP_yahoo, avgCost_espn, avgCost_fp, avgADP_ffc)

lapply(costList, function(x){
  setkeyv(x, cols=c("name","pos"))
})

avgCost <- Reduce(function(x, y){
  merge(x, y, all=TRUE, allow.cartesian=TRUE)
}, costList)

#Set player name as most common instance across sources
#playerNames <- melt(avgCost,
#                    id.vars = c("name","pos"),
#                    measure.vars = paste("name", sourcesOfCost, sep="_"),
#                    na.rm=TRUE,
#                    value.name="player")[,player := names(which.max(table(player))),
#                                         by=list(name, pos)][order(name), -3, with=FALSE]

#setkeyv(playerNames, cols=c("name","pos"))
#avgCost <- avgCost[unique(playerNames)]

#Set team name as most common instance across sources
#teamNames <- melt(avgCost,
#                  id.vars = c("name","pos"),
#                  measure.vars = paste("team", sourcesOfCost, sep="_"),
#                  na.rm=TRUE,
#                  value.name="team")[,team := names(which.max(table(team))),
#                                     by=list(name, pos)][order(name), -3, with=FALSE]

#setkeyv(teamNames, cols=c("name","pos"))
#avgCost <- avgCost[unique(teamNames)]

#Subset
avgCost <- avgCost[, c("name","pos","adp_yahoo","adp_espn","adp_fp","adp_ffc","adpSD_ffc","cost_yahoo","costProjected_yahoo","costAvg_yahoo","costMax_yahoo","cost_espn","cost_fp"), with=FALSE]

#Remove duplicate cases
#avgCost[avgCost$name %in% avgCost$name[duplicated(avgCost$name)],]

#dropNames <- nameMerge(c("Alex Smith","Ryan Griffin","Zach Miller","Steve Smith","Mike Williams")) #,"Chris Givens"
#dropVariables <- c("pos_espn","pos_espn","team_espn","team_espn","team_espn") #,"team_espn"
#dropLabels <- c("TE","QB","CHI","FA","FA") #,"NO"

#avgCost2 <- ddply(avgCost, .(name), numcolwise(mean), na.rm=TRUE)

#for(i in 1:length(dropNames)){
#  if(dim(avgCost[-which(avgCost[,"name"] == dropNames[i] & avgCost[,dropVariables[i]] == dropLabels[i]),])[1] > 0){
#    avgCost <- avgCost[-which(avgCost[,"name"] == dropNames[i] & avgCost[,dropVariables[i]] == dropLabels[i]),]
#  }
#}

#avgCost <- merge(avgCost2, avgCost[,c("name","player","pos","team")], by="name")
#drops <- c("player","pos","team")
#avgCost <- avgCost[,!names(avgCost) %in% drops]

#Merge
setkeyv(projections, cols=c("name","pos"))
setkeyv(avgCost, cols=c("name","pos"))
projections <- merge(projections, avgCost, by=c("name","pos"), all.x=TRUE, allow.cartesian=TRUE)

#Remove duplicate cases
#projections[projections$name %in% projections$name[duplicated(projections$name)],]

###############
# Calculate projected cost
###############
#Select which source of cost to use
projections[,avgCost := costAvg_yahoo]

#Apply 10% price premium to 33 players with highest projected points, apply 10% price premium for players lower than rank 66
projections[,inflatedCost := ceiling(avgCost * (leagueCap/defaultCap) * 1.0)]
projections[overallRank <= 33, inflatedCost := ceiling(projections$avgCost[projections$overallRank <= 33] * (leagueCap/defaultCap) * 1.1)]
projections[overallRank >= 34 & projections$overallRank <= 66, inflatedCost := ceiling(projections$avgCost[projections$overallRank >= 34 & projections$overallRank <= 66] * (leagueCap/defaultCap) * 1.0)]
projections[overallRank >= 67, inflatedCost := ceiling(projections$avgCost[projections$overallRank >= 67] * (leagueCap/defaultCap) * 0.9)]

projections[is.na(inflatedCost) == TRUE, inflatedCost := 1]
projections[inflatedCost == 0, inflatedCost := 1]

projections[is.na(avgCost) == TRUE, avgCost := 1]
projections[avgCost == 0, avgCost := 1]

for(col in c("cost_yahoo","costProjected_yahoo","costAvg_yahoo","costMax_yahoo","cost_espn","cost_fp")){
  projections[is.na(get(col)), (col) := 1]
}

#Set Key
setkeyv(projections, cols=c("name","pos","team","sourceName"))

#Density Plot
ggplot(projections, aes(x=inflatedCost)) + geom_density(fill="green", alpha=.3) + xlab("Player's Intrinsic Value (Cost)") + ggtitle("Density Plot of Players' Intrinsic Value") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Inflated Cost.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste(getwd(), "/Data/AvgCost.RData", sep=""))
write.csv(projections, file=paste(getwd(), "/Data/AvgCost.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(), "/Data/Historical Cost/AvgCost-", season, ".RData", sep=""))
write.csv(projections, file=paste(getwd(), "/Data/Historical Cost/AvgCost-", season, ".csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(), "/Data/Rankings.RData", sep=""))
write.csv(projections, file=paste(getwd(), "/Data/Rankings.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(), "/Data/Historical Rankings/Rankings-", season, ".RData", sep=""))
write.csv(projections, file=paste(getwd(), "/Data/Historical Rankings/Rankings-", season, ".csv", sep=""), row.names=FALSE)
