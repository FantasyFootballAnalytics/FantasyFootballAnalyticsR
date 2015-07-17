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

#Convert position to factor
aavDataLong$position <- as.factor(aavDataLong$position)

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
aavModel <- lme(aav ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(aavModel)
cor(getResponse(aavModel), predict(aavModel), use="pairwise.complete.obs", method="pearson")^2

#AAV with Imputed Zeros
aavImputedModel <- lme(aavImputeZeros ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(aavImputedModel)
cor(getResponse(aavImputedModel), predict(aavImputedModel), use="pairwise.complete.obs", method="pearson")^2

#AAV Percent
aavPercentModel <- lme(aavPercent ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(aavPercentModel)
cor(getResponse(aavPercentModel), predict(aavPercentModel), use="pairwise.complete.obs", method="pearson")^2

#AAV Percent with Imputed Zeros
aavImputedPercentModel <- lme(aavImputeZerosPercent ~ 1 + teams + I(teams^2) + teams10 + positionRank + position + teams*I(teams^2)*teams10*positionRank*position, random = ~ 1 + teams|player, method="REML", data=aavDataLong, na.action=na.exclude, control=list(opt="optim", msMaxIter=20000))
summary(aavImputedPercentModel)
cor(getResponse(aavImputedPercentModel), predict(aavImputedPercentModel), use="pairwise.complete.obs", method="pearson")^2

#Final Model
finalModelAAV <- aavImputedModel
finalModelAAVPercent <- aavImputedPercentModel

#Fitted Values
aavDataLong$aavFitted <- predict(finalModelAAV)
aavDataLong$aavPercentFitted <- predict(finalModelAAVPercent)
aavDataLong$aavPercentToAAVFitted <- (aavDataLong$aavPercentFitted/100) * (200*aavDataLong$teams)

#Convert negative to 0
aavDataLong$aavFitted[which(aavDataLong$aavFitted < 0)] <- 0
aavDataLong$aavPercentToAAVFitted[which(aavDataLong$aavPercentToAAVFitted < 0)] <- 0

#Check accuracy
calculateMASE(aavDataLong$aavFitted, aavDataLong$aav) #more accurate
calculateMASE(aavDataLong$aavPercentToAAVFitted, aavDataLong$aav)

#New Data
newdata <- aavDataLong #subset(aavDataLong, position %in% c("QB","RB") & positionRank < 10)
newdata$name <- newdata$player
newdata$player <- NULL
newdata$predictedAAV <- aavDataLong$predictedAAV <- predict(finalModelAAV, newdata=newdata, level=0)
newdata$predictedAAVPercent <- aavDataLong$predictedAAVPercent <- predict(finalModelAAVPercent, newdata=newdata, level=0)

newdata$predictedAAVPercentToAAV <- (newdata$predictedAAVPercent/100) * (200*newdata$teams)
aavDataLong$predictedAAVPercentToAAV <- (aavDataLong$predictedAAVPercent/100) * (200*aavDataLong$teams)
  
newdata$predictedAAV[which(newdata$predictedAAV < 0)] <- 0
newdata$predictedAAVPercent[which(newdata$predictedAAVPercent < 0)] <- 0
newdata$predictedAAVPercentToAAV[which(newdata$predictedAAVPercentToAAV < 0)] <- 0
aavDataLong$predictedAAV[which(aavDataLong$predictedAAV < 0)] <- 0
aavDataLong$predictedAAVPercent[which(aavDataLong$predictedAAVPercent < 0)] <- 0
aavDataLong$predictedAAVPercentToAAV[which(aavDataLong$predictedAAVPercentToAAV < 0)] <- 0

calculateMASE(newdata$predictedAAV, newdata$aav)
calculateMASE(newdata$predictedAAVPercentToAAV, newdata$aav) #more accurate

#####################
# Plots
#####################

#### AAV

#QB
ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = aav, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("QB AAV")
ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("QB AAV")
ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("QB AAV")

#RB
ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = aav, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("RB AAV")
ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("RB AAV")
ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("RB AAV")

#WR
ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = aav, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("WR AAV")
ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("WR AAV")
ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("WR AAV")

#TE
ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = aav, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("TE AAV")
ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("TE AAV")
ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("TE AAV")

#K
ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = aav, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("K AAV")
ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("K AAV")
ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("K AAV")

#DST
ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = aav, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("DST AAV")
ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = predictedAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("DST AAV")
ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = predictedAAVPercentToAAV, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 60)) + ggtitle("DST AAV")

#### AAV Percent

#QB
ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = aavPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("QB AAV Percent")
ggplot(data = subset(aavDataLong, position=="QB"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("QB AAV Percent")

#RB
ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = aavPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("RB AAV Percent")
ggplot(data = subset(aavDataLong, position=="RB"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("RB AAV Percent")

#WR
ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = aavPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("WR AAV Percent")
ggplot(data = subset(aavDataLong, position=="WR"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("WR AAV Percent")

#TE
ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = aavPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("TE AAV Percent")
ggplot(data = subset(aavDataLong, position=="TE"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("TE AAV Percent")

#K
ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = aavPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("K AAV Percent")
ggplot(data = subset(aavDataLong, position=="K"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("K AAV Percent")

#DST
ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = aavPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("DST AAV Percent")
ggplot(data = subset(aavDataLong, position=="DST"), aes(x = teams, y = predictedAAVPercent, color = player)) + geom_line(aes(group = player)) + geom_point() + scale_x_continuous(breaks=seq(from=4, to=20, by=2)) + scale_y_continuous(limits = c(0, 5)) + ggtitle("DST AAV Percent")

#####################
# Save Data
#####################
write.csv(aavDataLong, paste(getwd(), "/R Scripts/Posts/AAV by Teams and Position/Data/aavFitted.csv", sep=""), row.names=FALSE)
