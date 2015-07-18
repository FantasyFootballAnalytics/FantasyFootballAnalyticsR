###########################
# File: AAV by Teams and Position.R
# Description: Adjusts players' auction values by number of teams in league and position of player
# Date: 7/16/2015
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("nlme")
library("ggplot2")

#Functions
source(paste(getwd(), "/R Scripts/Functions/Functions.R", sep=""))

#Data
aavData <- read.csv(paste(getwd(), "/R Scripts/Posts/AAV by Teams and Position/Data/aav.csv", sep=""), stringsAsFactors = FALSE)

#####################
# Data Processing
#####################

#Wide to Long
aavDataLong <- reshape(aavData,
                       idvar = "player",
                       varying = paste("teams", seq(from=4, to=20, by=2), sep=""),
                       v.names = "aav",
                       timevar = "teams",
                       times = seq(from=4, to=20, by=2),
                       new.row.names = 1:(nrow(aavData)*9),
                       direction = "long")

row.names(aavDataLong) <- NULL

#Merge AAV for 10-team leagues
aavDataLong <- merge(aavDataLong, aavData[,c("player","teams10")], by="player")
aavDataLong$teams10[which(is.na(aavDataLong$teams10))] <- 0

#Convert position to factor with IDP as reference group
aavDataLong$position <- factor(aavDataLong$position, levels=c("IDP","QB","RB","WR","TE","K","DST"))

#Impute zeros for missing AAV values
aavDataLong$aavImputeZeros <- aavDataLong$aav
aavDataLong$aavImputeZeros[which(is.na(aavDataLong$aav))] <- 0

#Calculate AAV as a percent of available cap
aavDataLong$aavPercent <- aavDataLong$aav / (200 * aavDataLong$teams) * 100
aavDataLong$aavImputeZerosPercent <- aavDataLong$aavImputeZeros / (200 * aavDataLong$teams) * 100

#####################
# Multilevel Models
#####################

#All positions: Fixed slopes
fixedSlopes <- lme(aav ~ 1, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes)

#All positions: Fixed slopes with # of teams, AAV for 10 teams
fixedSlopes1 <- lme(aav ~ 1 + teams, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes1)
cor(getResponse(fixedSlopes1), predict(fixedSlopes1), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with # of teams, AAV for 10 teams, position rank
fixedSlopes2 <- lme(aav ~ 1 + teams + teams10 + positionRank, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes2)
cor(getResponse(fixedSlopes2), predict(fixedSlopes2), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with # of teams, AAV for 10 teams, position rank, position
fixedSlopes3 <- lme(aav ~ 1 + teams + teams10 + positionRank + position, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes3)
cor(getResponse(fixedSlopes3), predict(fixedSlopes3), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with interactions: teams X teams10
fixedSlopes4 <- lme(aav ~ 1 + teams + teams10 + positionRank + position + teams:teams10, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes4)
cor(getResponse(fixedSlopes4), predict(fixedSlopes4), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with interactions: teams X teams10, teams X positionRank
fixedSlopes5 <- lme(aav ~ 1 + teams + teams10 + positionRank + position + teams:teams10 + teams:positionRank, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes5)
cor(getResponse(fixedSlopes5), predict(fixedSlopes5), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with interactions: teams X teams10, teams X positionRank, teams X position
fixedSlopes6 <- lme(aav ~ 1 + teams + teams10 + positionRank + position + teams:teams10 + teams:positionRank + teams:position, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes6)
cor(getResponse(fixedSlopes6), predict(fixedSlopes6), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with interactions: teams X teams10, teams X positionRank, teams X position, teams10 X position rank
fixedSlopes7 <- lme(aav ~ 1 + teams + teams10 + positionRank + position + teams:teams10 + teams:positionRank + teams:position + teams10:positionRank, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes7)
cor(getResponse(fixedSlopes7), predict(fixedSlopes7), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with interactions: teams X teams10, teams X positionRank, teams X position, teams10 X position rank, teams10 X position
fixedSlopes8 <- lme(aav ~ 1 + teams + teams10 + positionRank + position + teams:teams10 + teams:positionRank + teams:position + teams10:positionRank + teams10:position, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes8)
cor(getResponse(fixedSlopes8), predict(fixedSlopes8), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with interactions: teams X teams10, teams X positionRank, teams X position, teams10 X position rank, teams10 X position, position rank X position
fixedSlopes9 <- lme(aav ~ 1 + teams + teams10 + positionRank + position + teams:teams10 + teams:positionRank + teams:position + teams10:positionRank + teams10:position + positionRank:position, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes9)
cor(getResponse(fixedSlopes9), predict(fixedSlopes9), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with all possible interactions
fixedSlopes10 <- lme(aav ~ 1 + teams + teams10 + positionRank + position + teams*teams10*positionRank*position, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes10)
cor(getResponse(fixedSlopes10), predict(fixedSlopes10), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with all possible interactions and quadratic slopes
fixedSlopes11 <- lme(aav ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*teams10*positionRank*position, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
summary(fixedSlopes11)
cor(getResponse(fixedSlopes11), predict(fixedSlopes11), use="pairwise.complete.obs", method="pearson")^2

#All positions: Fixed slopes with all possible interactions and quadratic slopes/interactions
fixedSlopes12 <- lme(aav ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1|player, method="ML", data=aavDataLong, na.action=na.exclude)
fixedSlopes12_reml <- lme(aav ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(fixedSlopes12)
cor(getResponse(fixedSlopes12), predict(fixedSlopes12), use="pairwise.complete.obs", method="pearson")^2

#All positions: Random slopes with # of teams
randomSlopes1a <- lme(aav ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(randomSlopes1a)
cor(getResponse(randomSlopes1a), predict(randomSlopes1a), use="pairwise.complete.obs", method="pearson")^2

#All positions: Random slopes with AAV for 10 teams
randomSlopes1b <- lme(aav ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams10|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(randomSlopes1b)
cor(getResponse(randomSlopes1b), predict(randomSlopes1b), use="pairwise.complete.obs", method="pearson")^2

#All positions: Random slopes with position rank
randomSlopes1c <- lme(aav ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + positionRank|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(randomSlopes1c)
cor(getResponse(randomSlopes1c), predict(randomSlopes1c), use="pairwise.complete.obs", method="pearson")^2

#All positions: Random slopes with position (takes too long to converge)
#randomSlopes1d <- lme(aav ~ 1 + teams + teams10 + positionRank + position + teams*teams10*positionRank*position, random = ~ 1 + position|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
#summary(randomSlopes1d)
#cor(getResponse(randomSlopes1d), predict(randomSlopes1d), use="pairwise.complete.obs", method="pearson")^2

#Compare models adding fixed effects
anova(fixedSlopes,fixedSlopes1,fixedSlopes2,fixedSlopes3,fixedSlopes4,fixedSlopes5,fixedSlopes6,fixedSlopes7,fixedSlopes8,fixedSlopes9,fixedSlopes10,fixedSlopes11,fixedSlopes12) #all fixed effects improve model fit

#Compare models with fixed vs. random slopes
anova(fixedSlopes12_reml,randomSlopes1a) #model fits significantly better with random than fixed effect of # of teams --- best fitting model
anova(fixedSlopes12_reml,randomSlopes1b) #model fits significantly better with random than fixed effect of AAV for 10 teams
anova(fixedSlopes12_reml,randomSlopes1c)
#anova(fixedSlopes12_reml,randomSlopes1d)

#AAV
aavModel <- lme(aav ~ 1 + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(aavModel)
cor(getResponse(aavModel), predict(aavModel), use="pairwise.complete.obs", method="pearson")^2

#AAV with Imputed Zeros
aavImputedModel <- lme(aavImputeZeros ~ 1 + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(aavImputedModel)
cor(getResponse(aavImputedModel), predict(aavImputedModel), use="pairwise.complete.obs", method="pearson")^2

#AAV Percent
aavPercentModel <- lme(aavPercent ~ 1 + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(aavPercentModel)
cor(getResponse(aavPercentModel), predict(aavPercentModel), use="pairwise.complete.obs", method="pearson")^2

#AAV Percent with Imputed Zeros
aavImputedPercentModel <- lme(aavImputeZerosPercent ~ 1 + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(aavImputedPercentModel)
cor(getResponse(aavImputedPercentModel), predict(aavImputedPercentModel), use="pairwise.complete.obs", method="pearson")^2

#Final Model
finalModelAAV <- aavImputedModel
finalModelAAVPercent <- aavImputedPercentModel

#Fitted Values
aavDataLong$aavFitted <- predict(finalModelAAV)
aavDataLong$aavPercentFitted <- predict(finalModelAAVPercent, level=1) #level=1 for data-specific estimates (based on player-specific random effects)
aavDataLong$aavPercentToAAVFitted <- (aavDataLong$aavPercentFitted/100) * (200*aavDataLong$teams)

#Convert negative to 0
aavDataLong$aavFitted[which(aavDataLong$aavFitted < 0)] <- 0
aavDataLong$aavPercentToAAVFitted[which(aavDataLong$aavPercentToAAVFitted < 0)] <- 0

#Check accuracy
calculateMASE(aavDataLong$aavFitted, aavDataLong$aav) #more accurate
calculateMASE(aavDataLong$aavPercentToAAVFitted, aavDataLong$aav)

#Test accuracy of population estimates for existing data set
existingData <- aavDataLong
existingData$name <- existingData$player
existingData$player <- NULL
existingData$predictedAAV <- aavDataLong$predictedAAV <- predict(finalModelAAV, newdata=existingData, level=0)
existingData$predictedAAVPercent <- aavDataLong$predictedAAVPercent <- predict(finalModelAAVPercent, newdata=existingData, level=0) #level=0 for population estimates (ignore random component), level=1 for data-specific estimates (need player name)

existingData$predictedAAVPercentToAAV <- (existingData$predictedAAVPercent/100) * (200*existingData$teams)
aavDataLong$predictedAAVPercentToAAV <- (aavDataLong$predictedAAVPercent/100) * (200*aavDataLong$teams)
  
existingData$predictedAAV[which(existingData$predictedAAV < 0)] <- 0
existingData$predictedAAVPercent[which(existingData$predictedAAVPercent < 0)] <- 0
existingData$predictedAAVPercentToAAV[which(existingData$predictedAAVPercentToAAV < 0)] <- 0
aavDataLong$predictedAAV[which(aavDataLong$predictedAAV < 0)] <- 0
aavDataLong$predictedAAVPercent[which(aavDataLong$predictedAAVPercent < 0)] <- 0
aavDataLong$predictedAAVPercentToAAV[which(aavDataLong$predictedAAVPercentToAAV < 0)] <- 0

calculateMASE(existingData$predictedAAV, existingData$aav)
calculateMASE(existingData$predictedAAVPercentToAAV, existingData$aav) #more accurate

#Examine population estimates for new data set
newData <- data.frame(position = factor(c("QB","QB","QB","RB","RB","RB","WR","WR","WR","TE","TE","TE","K","K","K","DST","DST","DST","IDP","IDP","IDP")),
                      positionRank = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                      teams10 = c(60,60,60,60,60,60,60,60,60,60,60,60,1,1,1,1,1,1,1,1,1),
                      teams = c(4,10,20,4,10,20,4,10,20,4,10,20,4,10,20,4,10,20,4,10,20))
newData$aavPercent <- predict(finalModelAAVPercent, newdata=newData, level=0)
newData$aavPercentToAAV <- (newData$aavPercent/100) * (200*newData$teams)

#####################
# Plots
#####################

#### AAV

#QB
qbAAV <- ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = aavImputeZeros, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("QB AAV") + xlab("Number of Teams in League") + ylab("AAV") + theme(legend.position="none")
qbAAVFitted <- ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("QB AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV")
qbAAVFittedBasedOnPercent <- ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("QB AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV")

#RB
rbAAV <- ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = aavImputeZeros, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("RB AAV") + xlab("Number of Teams in League") + ylab("AAV") + theme(legend.position="none")
rbAAVFitted <- ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("RB AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV") + theme(legend.text=element_text(size=6), legend.key.size = unit(0.15, "cm"))
rbAAVFittedBasedOnPercent <- ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("RB AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV") + theme(legend.text=element_text(size=6), legend.key.size = unit(0.15, "cm"))

#WR
wrAAV <- ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = aavImputeZeros, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("WR AAV") + xlab("Number of Teams in League") + ylab("AAV") + theme(legend.position="none")
wrAAVFitted <- ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("WR AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV") + theme(legend.text=element_text(size=6), legend.key.size = unit(0.15, "cm"))
wrAAVFittedBasedOnPercent <- ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("WR AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV") + theme(legend.text=element_text(size=6), legend.key.size = unit(0.15, "cm"))

#TE
teAAV <- ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = aavImputeZeros, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("TE AAV") + xlab("Number of Teams in League") + ylab("AAV") + theme(legend.position="none")
teAAVFitted <- ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("TE AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV")
teAAVFittedBasedOnPercent <- ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("TE AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV")

#K
kAAV <- ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = aav, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("K AAV") + xlab("Number of Teams in League") + ylab("AAV") + theme(legend.position="none")
kAAVFitted <- ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("K AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV")
kAAVFittedBasedOnPercent <- ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("K AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV")

#DST
dstAAV <- ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = aavImputeZeros, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("DST AAV") + xlab("Number of Teams in League") + ylab("AAV") + theme(legend.position="none")
dstAAVFitted <- ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("DST AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV")
dstAAVFittedBasedOnPercent <- ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 62)) + ggtitle("DST AAV (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV")

#### AAV Percent

#QB
qbAAVPercent <- ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = aavImputeZerosPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("QB AAV Percent") + xlab("Number of Teams in League") + ylab("AAV (Percent of Total Cap across Teams)") + theme(legend.position="none")
qbAAVPercentFitted <- ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("QB AAV Percent (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV (Percent of Total Cap across Teams)")

#RB
rbAAVPercent <- ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = aavImputeZerosPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("RB AAV Percent") + xlab("Number of Teams in League") + ylab("AAV (Percent of Total Cap across Teams)") + theme(legend.position="none")
rbAAVPercentFitted <- ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("RB AAV Percent (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV (Percent of Total Cap across Teams)") + theme(legend.text=element_text(size=6), legend.key.size = unit(0.15, "cm"))

#WR
wrAAVPercent <- ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = aavImputeZerosPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("WR AAV Percent") + xlab("Number of Teams in League") + ylab("AAV (Percent of Total Cap across Teams)") + theme(legend.position="none")
wrAAVPercentFitted <- ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("WR AAV Percent (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV (Percent of Total Cap across Teams)") + theme(legend.text=element_text(size=6), legend.key.size = unit(0.15, "cm"))

#TE
teAAVPercent <- ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = aavImputeZerosPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("TE AAV Percent") + xlab("Number of Teams in League") + ylab("AAV (Percent of Total Cap across Teams)") + theme(legend.position="none")
teAAVPercentFitted <- ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("TE AAV Percent (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV (Percent of Total Cap across Teams)")

#K
kAAVPercent <- ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = aavImputeZerosPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("K AAV Percent") + xlab("Number of Teams in League") + ylab("AAV (Percent of Total Cap across Teams)") + theme(legend.position="none")
kAAVPercentFitted <- ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("K AAV Percent (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV (Percent of Total Cap across Teams)")

#DST
dstAAVPercent <- ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = aavImputeZerosPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("DST AAV Percent") + xlab("Number of Teams in League") + ylab("AAV (Percent of Total Cap across Teams)") + theme(legend.position="none")
dstAAVPercentFitted <- ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("DST AAV Percent (Predicted)") + xlab("Number of Teams in League") + ylab("Predicted AAV (Percent of Total Cap across Teams)")

#### Save Plots
png(paste(getwd(),"/R Scripts/Posts/AAV by Teams and Position/Figures/QB AAV.png", sep=""), width=14, height=8, units="in", res=300)
grid.arrange(qbAAV, qbAAVFittedBasedOnPercent, ncol=2, widths=c(6.25,7.75))
dev.off()

png(paste(getwd(),"/R Scripts/Posts/AAV by Teams and Position/Figures/RB AAV.png", sep=""), width=14, height=8, units="in", res=300)
grid.arrange(rbAAV, rbAAVFittedBasedOnPercent, ncol=2, widths=c(6.25,7.75))
dev.off()

png(paste(getwd(),"/R Scripts/Posts/AAV by Teams and Position/Figures/WR AAV.png", sep=""), width=14, height=8, units="in", res=300)
grid.arrange(wrAAV, wrAAVFittedBasedOnPercent, ncol=2, widths=c(6.25,7.75))
dev.off()

png(paste(getwd(),"/R Scripts/Posts/AAV by Teams and Position/Figures/TE AAV.png", sep=""), width=14, height=8, units="in", res=300)
grid.arrange(teAAV, teAAVFittedBasedOnPercent, ncol=2, widths=c(6.25,7.75))
dev.off()

png(paste(getwd(),"/R Scripts/Posts/AAV by Teams and Position/Figures/K AAV.png", sep=""), width=14, height=8, units="in", res=300)
grid.arrange(kAAV, kAAVFittedBasedOnPercent, ncol=2, widths=c(6.25,7.75))
dev.off()

png(paste(getwd(),"/R Scripts/Posts/AAV by Teams and Position/Figures/DST AAV.png", sep=""), width=14, height=8, units="in", res=300)
grid.arrange(dstAAV, dstAAVFittedBasedOnPercent, ncol=2, widths=c(6.25,7.75))
dev.off()

#####################
# Model for OpenCPU
#####################

#Rename variables
aavDataLong$numTeams <- aavDataLong$teams
aavDataLong$auctionValue <- aavDataLong$teams10

#Model
adjustAAV <- lme(aavImputeZerosPercent ~ 1 + numTeams*I(numTeams^2)*auctionValue*positionRank*position, random = ~ 1 + numTeams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))

#Adjusted AAV
aavDataLong$aavPercent <- predict(adjustAAV, newdata = aavDataLong, level=0)
aavDataLong$aavAdjusted <- (aavDataLong$aavPercent/100) * (200*aavDataLong$numTeams)
aavDataLong$aavAdjusted[which(aavDataLong$aavAdjusted < 0)] <- 0

#Save model object
save(adjustAAV, file=paste(getwd(), "/R Scripts/Posts/AAV by Teams and Position/Data/adjustAAV.Rdata", sep=""))

#####################
# Save Data
#####################
write.csv(aavDataLong, paste(getwd(), "/R Scripts/Posts/AAV by Teams and Position/Data/aavFitted.csv", sep=""), row.names=FALSE)
