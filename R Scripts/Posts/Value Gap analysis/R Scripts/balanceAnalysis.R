#############################
# File : balanceAnalysis.R
# Date : 07/26/2015
# Author : Dennis Andersen [andersen.dennis@live.com]
#############################
library(data.table)
library(ggplot2)

## Read the data
qbProjections <- data.table(read.csv("R Scripts/Posts/Value Gap analysis/Data/Posts/qbprojections.csv", stringsAsFactors = FALSE))
rbProjections <- data.table(read.csv("R Scripts/Posts/Value Gap analysis/Data/Posts/rbprojections.csv", stringsAsFactors = FALSE))
wrProjections <- data.table(read.csv("R Scripts/Posts/Value Gap analysis/Data/Posts/wrprojections.csv", stringsAsFactors = FALSE))
teProjections <- data.table(read.csv("R Scripts/Posts/Value Gap analysis/Data/Posts/teprojections.csv", stringsAsFactors = FALSE))

playerTeams <- data.table(read.csv("R Scripts/Posts/Value Gap analysis/Data/Posts/playerteams.csv", stringsAsFactors = FALSE))
players <- data.table(read.csv("R Scripts/Posts/Value Gap analysis/Data/Posts/players.csv", stringsAsFactors = FALSE))

## Merge team information
qbProjections <- merge(qbProjections, playerTeams[, c("playerId", "team"), with = FALSE], by = "playerId")
rbProjections <- merge(rbProjections, playerTeams[, c("playerId", "team"), with = FALSE], by = "playerId")
wrProjections <- merge(wrProjections, playerTeams[, c("playerId", "team"), with = FALSE], by = "playerId")
teProjections <- merge(teProjections, playerTeams[, c("playerId", "team"), with = FALSE], by = "playerId")

## Summarize data by team and analyst
qbProjections[, c("qbTeamPassYds", "qbTeamPassTds", "qbTeamPassComp") := list(sum(passYds, na.rm = TRUE), sum(passTds, na.rm = TRUE), sum(passComp, na.rm = TRUE)), by = c("team", "analystId")]
rbProjections[, c("rbTeamRecYds", "rbTeamRecTds", "rbTeamRec") := list(sum(recYds, na.rm = TRUE), sum(recTds, na.rm = TRUE), sum(rec, na.rm = TRUE)), by = c("team", "analystId")]
wrProjections[, c("wrTeamRecYds", "wrTeamRecTds", "wrTeamRec") := list(sum(recYds, na.rm = TRUE), sum(recTds, na.rm = TRUE), sum(rec, na.rm = TRUE)), by = c("team", "analystId")]
teProjections[, c("teTeamRecYds", "teTeamRecTds", "teTeamRec") := list(sum(recYds, na.rm = TRUE), sum(recTds, na.rm = TRUE), sum(rec, na.rm = TRUE)), by = c("team", "analystId")]

## Generate team data set
teamData <- merge(unique(qbProjections[, c("team", "analystId", "qbTeamPassYds", "qbTeamPassTds", "qbTeamPassComp"), with = FALSE]),
                  unique(rbProjections[, c("team", "analystId", "rbTeamRecYds", "rbTeamRecTds", "rbTeamRec"), with = FALSE]), by = c("team", "analystId"))
teamData <- merge(teamData, unique(wrProjections[, c("team", "analystId", "wrTeamRecYds", "wrTeamRecTds", "wrTeamRec"), with = FALSE]), by = c("team", "analystId"))
teamData <- merge(teamData, unique(teProjections[, c("team", "analystId", "teTeamRecYds", "teTeamRecTds", "teTeamRec"), with = FALSE]), by = c("team", "analystId"))

## Add stats across receiver positions
teamData[, teamRecYds := rbTeamRecYds + wrTeamRecYds + teTeamRecYds]
teamData[, teamRecTds := rbTeamRecTds + wrTeamRecTds + teTeamRecTds]
teamData[, teamRec := rbTeamRec + wrTeamRec + teTeamRec]

## Calculate the receiving yard share for NO receievers
newOrlRec <- rbindlist(list(rbProjections[team == "NO", c("playerId", "recYds"), with = FALSE], 
                            wrProjections[team == "NO", c("playerId", "recYds"), with = FALSE],
                            teProjections[team == "NO", c("playerId", "recYds"), with = FALSE]))
newOrlRec[, projRecYds := mean(recYds, na.rm = TRUE), by = "playerId"]
newOrlRecPlayers <- merge(players, unique(newOrlRec[, c("playerId", "projRecYds"), with = FALSE]), by = "playerId")
newOrlRecPlayers[, ydShare:= projRecYds/sum(projRecYds)]

## Calculate the receiving td share for NO receievers
newOrlTds <- rbindlist(list(rbProjections[team == "NO", c("playerId", "recTds"), with = FALSE], 
                            wrProjections[team == "NO", c("playerId", "recTds"), with = FALSE],
                            teProjections[team == "NO", c("playerId", "recTds"), with = FALSE]))
newOrlTds[, projRecTds := mean(recTds, na.rm = TRUE), by = "playerId"]
newOrlTdsPlayers <- merge(players, unique(newOrlTds[, c("playerId", "projRecTds"), with = FALSE]), by = "playerId")
newOrlTdsPlayers[, tdShare:= projRecTds/sum(projRecTds)]

## Calculate receiving Yard share for Chi receivers
chiOrlRec <- rbindlist(list(rbProjections[team == "CHI", c("playerId", "recYds"), with = FALSE], 
                            wrProjections[team == "CHI", c("playerId", "recYds"), with = FALSE],
                            teProjections[team == "CHI", c("playerId", "recYds"), with = FALSE]))
chiOrlRec[, projRecYds := mean(recYds, na.rm = TRUE), by = "playerId"]
chiOrlRecPlayers <- merge(players, unique(chiOrlRec[, c("playerId", "projRecYds"), with = FALSE]), by = "playerId")
chiOrlRecPlayers[, ydShare:= projRecYds/sum(projRecYds)]

## Calculate differeces between passing and receiving stats
teamData[, passYdsDiff := qbTeamPassYds - teamRecYds]
teamData[, passTdDiff := qbTeamPassTds - teamRecTds]
teamData[, passRecDiff := qbTeamPassComp - teamRec]

## Generate bar plot for receiving yards difference
tblData <- unique(teamData[, c("team", "passYdsDiff"), with = FALSE])
tblData <- data.table(aggregate(teamData$passYdsDiff, by = list(teamData$team), FUN = mean, data = teamData))
setnames(tblData, 1:2,  c("team", "passYdsDiff"))
tblData <- tblData[team != "FA" ]
tblData <- tblData[,team := reorder(team, passYdsDiff, function(x)-x)]
ggplot(tblData, aes(x =team , y=passYdsDiff, fill = passYdsDiff > 0),  position = 'dodge' ) + 
    geom_bar(stat = "identity") + xlab("Team") + ylab("PassYds - RecYds") +scale_fill_discrete(guide = 'none') + ggtitle("Pass and Receiving Yard difference")
ggsave("R Scripts/Posts/Value Gap analysis/Figures/passYdsDiffernce.png", width = 900/72, height = 545/72, units = "in")

## Generate bar plot for receiving td difference
tdData <-  data.table(aggregate(teamData$passTdDiff, by = list(teamData$team), FUN = mean, data = teamData))
setnames(tdData, 1:2,  c("team", "passTdDiff"))
tdData <- tdData[team != "FA" ]
tdData <- tdData[,team := reorder(team, passTdDiff, function(x)-x)]

ggplot(tdData, aes(x =team , y=passTdDiff, fill = passTdDiff > 0),  position = 'dodge' ) + 
    geom_bar(stat = "identity") + xlab("Team") + ylab("PassTds - RecTds") +scale_fill_discrete(guide = 'none') + ggtitle("Pass and Receiving Touchdown difference")
ggsave("R Scripts/Posts/Value Gap analysis/Figures/passTdDiffernce.png",  width = 900/72, height = 545/72, units = "in")

## Generate bar plot for reception difference
recData <-  data.table(aggregate(teamData$passRecDiff, by = list(teamData$team), FUN = mean, data = teamData))
setnames(recData, 1:2,  c("team", "passRecDiff"))
recData <- recData[team != "FA" ]
recData <- recData[,team := reorder(team, passRecDiff, function(x)-x)]
ggplot(recData, aes(x =team , y=passRecDiff, fill = passRecDiff > 0),  position = 'dodge' ) + 
    geom_bar(stat = "identity") + xlab("Team") + ylab("Completions - Recepetions") +scale_fill_discrete(guide = 'none') + ggtitle("Pass Completions and Receptions")
ggsave("R Scripts/Posts/Value Gap analysis/Figures/passCompDiffernce.png",  width = 900/72, height = 545/72, units = "in")

