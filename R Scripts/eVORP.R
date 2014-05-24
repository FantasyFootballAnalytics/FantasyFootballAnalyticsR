###########################
# File: eVORP.R
# Description: Calculates Expected Values by Position
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Libraries

#Data
adp <- read.csv(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/adp.csv", sep=""))
actual <- read.csv(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/actual.csv", sep=""))

#Merge
task <- merge(adp, actual, by=c("year","name"), all=TRUE)

#Rename vars
task$projectedOverallRank <- task$overallRank.x
task$projectedPositionRank <- task$positionRank.x
task$pos <- task$pos.x
task$team <- task$team.x

#Subset
task <- task[,c("year","name","pos","team","projectedOverallRank","projectedPositionRank","points","vor")]

#Convert NAs for points to 0
task[is.na(task$points), "points"] <- 0

#Remove players without projected position rank
task <- task[!is.na(task$projectedPositionRank),]

#Remove players with missing data
task <- task[-which(task$pos == "Def" & task$points==0),]

#Order players by points
task <- task[order(-task$year, -task$points),]

#Save Data
write.csv(task, file=paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/adpPlusActual.csv", sep=""), row.names=FALSE)

### All Data Points
taskQB <- task[task$pos == "QB",]
taskRB <- task[task$pos == "RB",]
taskWR <- task[task$pos == "WR",]
taskTE <- task[task$pos == "TE",]
taskK <- task[task$pos == "K",]
taskDef <- task[task$pos == "Def",]

qbPositions <- unique(taskQB$projectedPositionRank)
rbPositions <- unique(taskRB$projectedPositionRank)
wrPositions <- unique(taskWR$projectedPositionRank)
tePositions <- unique(taskTE$projectedPositionRank)
kPositions <- unique(taskK$projectedPositionRank)
defPositions <- unique(taskDef$projectedPositionRank)

qbPositions <- data.frame(projectedPositionRank=qbPositions[order(qbPositions)])
rbPositions <- data.frame(projectedPositionRank=rbPositions[order(rbPositions)])
wrPositions <- data.frame(projectedPositionRank=wrPositions[order(wrPositions)])
tePositions <- data.frame(projectedPositionRank=tePositions[order(tePositions)])
kPositions <- data.frame(projectedPositionRank=kPositions[order(kPositions)])
defPositions <- data.frame(projectedPositionRank=defPositions[order(defPositions)])

#Nonlinear functions
exponentialRegression <- function(x,a,b) {a * exp(-x/b)}
logRegression <- function(x,a,b) {a * log(x) + b}

#Linear Regression
qbLinearAll <- lm(points ~ projectedPositionRank, data=taskQB)
rbLinearAll <- lm(points ~ projectedPositionRank, data=taskRB)
wrLinearAll <- lm(points ~ projectedPositionRank, data=taskWR)
teLinearAll <- lm(points ~ projectedPositionRank, data=taskTE)
kLinearAll <- lm(points ~ projectedPositionRank, data=taskK)
defLinearAll <- lm(points ~ projectedPositionRank, data=taskDef)

summary(qbLinearAll)$r.squared  #r-squared = .34 #best (0.3395624)
summary(rbLinearAll)$r.squared  #r-squared = .38
summary(wrLinearAll)$r.squared  #r-squared = .29
summary(teLinearAll)$r.squared  #r-squared = .19
summary(kLinearAll)$r.squared   #r-squared = .07 #best (0.07450332)
summary(defLinearAll)$r.squared #r-squared = .13 #best (0.1267398)

#Exponential Regression
qbExpAll <- nls(points ~ exponentialRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskQB)
rbExpAll <- nls(points ~ exponentialRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskRB)
wrExpAll <- nls(points ~ exponentialRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskWR)
teExpAll <- nls(points ~ exponentialRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskTE) #ran
kExpAll <- nls(points ~ exponentialRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskK)
defExpAll <- nls(points ~ exponentialRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskDef)

1-(deviance(teExpAll)/sum((task[task$pos == "TE",]$points-mean(task[task$pos == "TE",]$points))^2)) #r-squared = .21

#Logarithmic Regression
qbLogAll <- nls(points ~ logRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskQB)
rbLogAll <- nls(points ~ logRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskRB)
wrLogAll <- nls(points ~ logRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskWR)
teLogAll <- nls(points ~ logRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskTE)
kLogAll <- nls(points ~ logRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskK)
defLogAll <- nls(points ~ logRegression(projectedPositionRank,a,b), start=c(a=1, b=1), data=taskDef)

1-(deviance(qbLogAll)/sum((task[task$pos == "QB",]$points-mean(task[task$pos == "QB",]$points))^2))    #r-squared = .31
1-(deviance(rbLogAll)/sum((task[task$pos == "RB",]$points-mean(task[task$pos == "RB",]$points))^2))    #r-squared = .40 #best (0.4012861)
1-(deviance(wrLogAll)/sum((task[task$pos == "WR",]$points-mean(task[task$pos == "WR",]$points))^2))    #r-squared = .31 #best (0.3132296)
1-(deviance(teLogAll)/sum((task[task$pos == "TE",]$points-mean(task[task$pos == "TE",]$points))^2))    #r-squared = .23 #best (0.2326401)
1-(deviance(kLogAll)/sum((task[task$pos == "K",]$points-mean(task[task$pos == "K",]$points))^2))       #r-squared = .06
1-(deviance(defLogAll)/sum((task[task$pos == "Def",]$points-mean(task[task$pos == "Def",]$points))^2)) #r-squared = .02

#Predictability
qbPredictabilityAll <- summary(qbLinearAll)$r.squared
rbPredictabilityAll <- 1-(deviance(rbLogAll)/sum((task[task$pos == "RB",]$points-mean(task[task$pos == "RB",]$points))^2))
wrPredictabilityAll <- 1-(deviance(wrLogAll)/sum((task[task$pos == "WR",]$points-mean(task[task$pos == "WR",]$points))^2)) 
tePredictabilityAll <- 1-(deviance(teLogAll)/sum((task[task$pos == "TE",]$points-mean(task[task$pos == "TE",]$points))^2))
kPredictabilityAll <- summary(kLinearAll)$r.squared
defPredictabilityAll <- summary(defLinearAll)$r.squared

qbPredictabilityAllInverse <- 1 - qbPredictabilityAll
rbPredictabilityAllInverse <- 1 - rbPredictabilityAll
wrPredictabilityAllInverse <- 1 - wrPredictabilityAll
tePredictabilityAllInverse <- 1 - tePredictabilityAll
kPredictabilityAllInverse <- 1 - kPredictabilityAll
defPredictabilityAllInverse <- 1 - defPredictabilityAll

data.frame(qbPredictabilityAllInverse, rbPredictabilityAllInverse, wrPredictabilityAllInverse, tePredictabilityAllInverse, kPredictabilityAllInverse, defPredictabilityAllInverse)/10

predictability = data.frame(qbPredictabilityAllInverse, rbPredictabilityAllInverse, wrPredictabilityAllInverse, tePredictabilityAllInverse, kPredictabilityAllInverse, defPredictabilityAllInverse)
predictability <- predictability - min(predictability)

#Plots
jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/QB Expected Values All.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(task[task$pos == "QB","projectedPositionRank"], task[task$pos == "QB","points"], xlab="Position Rank", ylab="Expected Points", main="QB Expected Values", ylim=c(0,300))
abline(qbLinearAll, col="blue", lwd=2)
text(50,250, paste("R-squared = ", round(summary(qbLinearAll)$r.squared, 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/RB Expected Values All.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(task[task$pos == "RB","projectedPositionRank"], task[task$pos == "RB","points"], xlab="Position Rank", ylab="Expected Points", main="RB Expected Values", ylim=c(0,300))
curve(logRegression(x, a=coef(rbLogAll)[1], b=coef(rbLogAll)[2]), add = TRUE, col="red", lwd=2)
text(90,250, paste("R-squared = ", round(1-(deviance(rbLogAll)/sum((task[task$pos == "RB",]$points-mean(task[task$pos == "RB",]$points))^2)), 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/WR Expected Values All.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(task[task$pos == "WR","projectedPositionRank"], task[task$pos == "WR","points"], xlab="Position Rank", ylab="Expected Points", main="WR Expected Values", ylim=c(0,300))
curve(logRegression(x, a=coef(wrLogAll)[1], b=coef(wrLogAll)[2]), add = TRUE, col="green", lwd=2)
text(90,250, paste("R-squared = ", round(1-(deviance(wrLogAll)/sum((task[task$pos == "WR",]$points-mean(task[task$pos == "WR",]$points))^2)), 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/TE Expected Values All.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(task[task$pos == "TE","projectedPositionRank"], task[task$pos == "TE","points"], xlab="Position Rank", ylab="Expected Points", main="TE Expected Values", ylim=c(0,300))
curve(logRegression(x, a=coef(teLogAll)[1], b=coef(teLogAll)[2]), add = TRUE, col="purple", lwd=2)
text(30,250, paste("R-squared = ", round(1-(deviance(teLogAll)/sum((task[task$pos == "TE",]$points-mean(task[task$pos == "TE",]$points))^2)), 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/K Expected Values All.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(task[task$pos == "K","projectedPositionRank"], task[task$pos == "K","points"], xlab="Position Rank", ylab="Expected Points", main="K Expected Values", ylim=c(0,300))
abline(kLinearAll, col="black", lwd=2)
text(25,250, paste("R-squared = ", round(summary(kLinearAll)$r.squared, 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/Def Expected Values All.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(task[task$pos == "Def","projectedPositionRank"], task[task$pos == "Def","points"], xlab="Position Rank", ylab="Expected Points", main="Defense Expected Values", ylim=c(0,300))
abline(defLinearAll, col="orange", lwd=2)
text(25,250, paste("R-squared = ", round(summary(defLinearAll)$r.squared, 3), sep=""))
dev.off()

#Predicted values
qbAll <- predict(qbLinearAll, newdata=qbPositions)
rbAll <- predict(rbLogAll, newdata=rbPositions)
wrAll <- qbEV <- predict(wrLogAll, newdata=wrPositions)
teAll <- predict(teLogAll, newdata=tePositions)
kAll <- predict(kLinearAll, newdata=kPositions)
defAll <- predict(defLinearAll, newdata=defPositions)

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/Expected Values.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(qbEVORP, xlab="Position Rank", ylab="Expected Points", main="Expected Values", xlim=c(0,110), ylim=c(0,300), col="white")
lines(x=qbExpected$rank, y=na.omit(qbAll), col="blue", lwd=5)
lines(x=rbExpected$rank, y=na.omit(rbAll), col="red", lwd=5)
lines(x=wrExpected$rank, y=na.omit(wrAll), col="green", lwd=5)
lines(x=teExpected$rank, y=na.omit(teAll), col="purple", lwd=5)
lines(x=kExpected$rank, y=na.omit(kAll), col="black", lwd=5)
lines(x=defExpected$rank, y=na.omit(defAll), col="orange", lwd=5)
legend("topright", legend=c("QB","RB","WR","TE","K","Def"), col=c("blue","red","green","purple","black","orange"), lwd=5)
dev.off()

n <- max(length(qb), length(rb), length(wr), length(te), length(k), length(def))
length(qb) <- n                      
length(rb) <- n
length(wr) <- n
length(te) <- n
length(k) <- n
length(def) <- n
adp <- 1:n

expectedValues <- data.frame(adp, qb, rb, wr, te, k, def)
round(expectedValues, 2)

### Hodges-Lehmann estimate of pseudo-median
qbEVORP <- vector()
rbEVORP <- vector()
wrEVORP <- vector()
teEVORP <- vector()
kEVORP <- vector()
defEVORP <- vector()

for(i in 1:200){
  if(length(task[task$pos=="QB" & task$projectedPositionRank==i, "points"]) > 1){
    qbEVORP[i] <- tryCatch(wilcox.test(task[task$pos=="QB" & task$projectedPositionRank==i, "points"], conf.int=TRUE)$estimate, error=function(e) median(task[task$pos=="QB" & task$projectedPositionRank==i, "points"], na.rm=TRUE))
  } else{
    qbEVORP[i] <- NA
  }
}

for(i in 1:200){
  if(length(task[task$pos=="RB" & task$projectedPositionRank==i, "points"]) > 1){
    rbEVORP[i] <- tryCatch(wilcox.test(task[task$pos=="RB" & task$projectedPositionRank==i, "points"], conf.int=TRUE)$estimate, error=function(e) median(task[task$pos=="RB" & task$projectedPositionRank==i, "points"], na.rm=TRUE))
  } else{
    rbEVORP[i] <- NA
  }
}

for(i in 1:200){
  if(length(task[task$pos=="WR" & task$projectedPositionRank==i, "points"]) > 1){
    wrEVORP[i] <- tryCatch(wilcox.test(task[task$pos=="WR" & task$projectedPositionRank==i, "points"], conf.int=TRUE)$estimate, error=function(e) median(task[task$pos=="WR" & task$projectedPositionRank==i, "points"], na.rm=TRUE))
  } else{
    wrEVORP[i] <- NA
  }
}

for(i in 1:200){
  if(length(task[task$pos=="TE" & task$projectedPositionRank==i, "points"]) > 1){
    teEVORP[i] <- tryCatch(wilcox.test(task[task$pos=="TE" & task$projectedPositionRank==i, "points"], conf.int=TRUE)$estimate, error=function(e) median(task[task$pos=="TE" & task$projectedPositionRank==i, "points"], na.rm=TRUE))
  } else{
    teEVORP[i] <- NA
  }
}

for(i in 1:200){
  if(length(task[task$pos=="K" & task$projectedPositionRank==i, "points"]) > 1){
    kEVORP[i] <- tryCatch(wilcox.test(task[task$pos=="K" & task$projectedPositionRank==i, "points"], conf.int=TRUE)$estimate, error=function(e) median(task[task$pos=="K" & task$projectedPositionRank==i, "points"], na.rm=TRUE))
  } else{
    kEVORP[i] <- NA
  }
}

for(i in 1:200){
  if(length(task[task$pos=="Def" & task$projectedPositionRank==i, "points"]) > 1){
    defEVORP[i] <- tryCatch(wilcox.test(task[task$pos=="Def" & task$projectedPositionRank==i, "points"], conf.int=TRUE)$estimate, error=function(e) median(task[task$pos=="Def" & task$projectedPositionRank==i, "points"], na.rm=TRUE))
  } else{
    defEVORP[i] <- NA
  }
}

qbEVORP <- print(qbEVORP[!is.na(qbEVORP)])
rbEVORP <- print(rbEVORP[!is.na(rbEVORP)])
wrEVORP <- print(wrEVORP[!is.na(wrEVORP)])
teEVORP <- print(teEVORP[!is.na(teEVORP)])
kEVORP <- print(kEVORP[!is.na(kEVORP)])
defEVORP <- print(defEVORP[!is.na(defEVORP)])

qbExpected <- cbind(qbEVORP, 1:length(qbEVORP))
rbExpected <- cbind(rbEVORP, 1:length(rbEVORP))
wrExpected <- cbind(wrEVORP, 1:length(wrEVORP))
teExpected <- cbind(teEVORP, 1:length(teEVORP))
kExpected <- cbind(kEVORP, 1:length(kEVORP))
defExpected <- cbind(defEVORP, 1:length(defEVORP))

colnames(qbExpected) <- c("eVORP","rank")
colnames(rbExpected) <- c("eVORP","rank")
colnames(wrExpected) <- c("eVORP","rank")
colnames(teExpected) <- c("eVORP","rank")
colnames(kExpected) <- c("eVORP","rank")
colnames(defExpected) <- c("eVORP","rank")

qbExpected <- data.frame(qbExpected)
rbExpected <- data.frame(rbExpected)
wrExpected <- data.frame(wrExpected)
teExpected <- data.frame(teExpected)
kExpected <- data.frame(kExpected)
defExpected <- data.frame(defExpected)

#Nonlinear functions
exponentialRegression <- function(x,a,b) {a * exp(-x/b)}
logRegression <- function(x,a,b) {a * log(x) + b}

#Linear Regression
qbLinear <- lm(eVORP ~ rank, data=qbExpected)
rbLinear <- lm(eVORP ~ rank, data=rbExpected)
wrLinear <- lm(eVORP ~ rank, data=wrExpected)
teLinear <- lm(eVORP ~ rank, data=teExpected)
kLinear <- lm(eVORP ~ rank, data=kExpected)
defLinear <- lm(eVORP ~ rank, data=defExpected)

summary(qbLinear)$r.squared  #r-squared = .89 #best
summary(rbLinear)$r.squared  #r-squared = .82
summary(wrLinear)$r.squared  #r-squared = .72
summary(teLinear)$r.squared  #r-squared = .67
summary(kLinear)$r.squared   #r-squared = .51 #best
summary(defLinear)$r.squared #r-squared = .49 #best

#Exponential Regression
qbExp <- nls(eVORP ~ exponentialRegression(rank,a,b), start=c(a=1, b=1), data=qbExpected)
rbExp <- nls(eVORP ~ exponentialRegression(rank,a,b), start=c(a=1, b=1), data=rbExpected)
wrExp <- nls(eVORP ~ exponentialRegression(rank,a,b), start=c(a=1, b=1), data=wrExpected)
teExp <- nls(eVORP ~ exponentialRegression(rank,a,b), start=c(a=1, b=1), data=teExpected) #ran
kExp <- nls(eVORP ~ exponentialRegression(rank,a,b), start=c(a=1, b=1), data=kExpected)
defExp <- nls(eVORP ~ exponentialRegression(rank,a,b), start=c(a=1, b=1), data=defExpected)

1-(deviance(teExp)/sum((teExpected$eVORP-mean(teExpected$eVORP))^2)) #r-squared = .75

#Logarithmic Regression
qbLog <- nls(eVORP ~ logRegression(rank,a,b), start=c(a=1, b=1), data=qbExpected)
rbLog <- nls(eVORP ~ logRegression(rank,a,b), start=c(a=1, b=1), data=rbExpected)
wrLog <- nls(eVORP ~ logRegression(rank,a,b), start=c(a=1, b=1), data=wrExpected)
teLog <- nls(eVORP ~ logRegression(rank,a,b), start=c(a=1, b=1), data=teExpected)
kLog <- nls(eVORP ~ logRegression(rank,a,b), start=c(a=1, b=1), data=kExpected)
defLog <- nls(eVORP ~ logRegression(rank,a,b), start=c(a=1, b=1), data=defExpected)

1-(deviance(qbLog)/sum((qbExpected$eVORP-mean(qbExpected$eVORP))^2))    #r-squared = .79
1-(deviance(rbLog)/sum((rbExpected$eVORP-mean(rbExpected$eVORP))^2))    #r-squared = .86 #best
1-(deviance(wrLog)/sum((wrExpected$eVORP-mean(wrExpected$eVORP))^2))    #r-squared = .80 #best
1-(deviance(teLog)/sum((teExpected$eVORP-mean(teExpected$eVORP))^2))    #r-squared = .85 #best
1-(deviance(kLog)/sum((kExpected$eVORP-mean(kExpected$eVORP))^2))       #r-squared = .42
1-(deviance(defLog)/sum((defExpected$eVORP-mean(defExpected$eVORP))^2)) #r-squared = .37

#Plots
jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/QB Expected Values.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(qbEVORP, xlab="Position Rank", ylab="Expected Points", main="QB Expected Values", ylim=c(0,300))
abline(qbLinear, col="blue", lwd=2)
text(40,250, paste("R-squared = ", round(summary(qbLinear)$r.squared, 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/RB Expected Values.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(rbEVORP, xlab="Position Rank", ylab="Expected Points", main="RB Expected Values", ylim=c(0,300))
curve(logRegression(x, a=coef(rbLog)[1], b=coef(rbLog)[2]), add = TRUE, col="red", lwd=2)
text(80,250, paste("R-squared = ", round(1-(deviance(rbLog)/sum((rbExpected$eVORP-mean(rbExpected$eVORP))^2)), 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/WR Expected Values.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(wrEVORP, xlab="Position Rank", ylab="Expected Points", main="WR Expected Values", ylim=c(0,300))
curve(logRegression(x, a=coef(wrLog)[1], b=coef(wrLog)[2]), add = TRUE, col="green", lwd=2)
text(90,250, paste("R-squared = ", round(1-(deviance(wrLog)/sum((wrExpected$eVORP-mean(wrExpected$eVORP))^2)), 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/TE Expected Values.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(teEVORP, xlab="Position Rank", ylab="Expected Points", main="TE Expected Values", ylim=c(0,300))
curve(logRegression(x, a=coef(teLog)[1], b=coef(teLog)[2]), add = TRUE, col="purple", lwd=2)
text(30,250, paste("R-squared = ", round(1-(deviance(teLog)/sum((teExpected$eVORP-mean(teExpected$eVORP))^2)), 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/K Expected Values.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(kEVORP, xlab="Position Rank", ylab="Expected Points", main="K Expected Values", ylim=c(0,300))
abline(kLinear, col="black", lwd=2)
text(25,250, paste("R-squared = ", round(summary(kLinear)$r.squared, 3), sep=""))
dev.off()

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/Def Expected Values.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(defEVORP, xlab="Position Rank", ylab="Expected Points", main="Defense Expected Values", ylim=c(0,300))
abline(defLinear, col="orange", lwd=2)
text(25,250, paste("R-squared = ", round(summary(defLinear)$r.squared, 3), sep=""))
dev.off()

#Predicted values
qb <- predict(qbLinear, newdata=qbExpected)
rb <- predict(rbLog, newdata=rbExpected)
wr <- qbEV <- predict(wrLog, newdata=wrExpected)
te <- predict(teLog, newdata=teExpected)
k <- predict(kLinear, newdata=kExpected)
def <- predict(defLinear, newdata=defExpected)

jpeg(paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/Plots/Expected Values.jpg", sep=""), width=1000, height=1000, pointsize=24)
plot(qbEVORP, xlab="Position Rank", ylab="Expected Points", main="Expected Values", xlim=c(0,110), ylim=c(0,300), col="white")
lines(x=qbExpected$rank, y=na.omit(qb), col="blue", lwd=5)
lines(x=rbExpected$rank, y=na.omit(rb), col="red", lwd=5)
lines(x=wrExpected$rank, y=na.omit(wr), col="green", lwd=5)
lines(x=teExpected$rank, y=na.omit(te), col="purple", lwd=5)
lines(x=kExpected$rank, y=na.omit(k), col="black", lwd=5)
lines(x=defExpected$rank, y=na.omit(def), col="orange", lwd=5)
legend("topright", legend=c("QB","RB","WR","TE","K","Def"), col=c("blue","red","green","purple","black","orange"), lwd=5)
dev.off()

n <- max(length(qb), length(rb), length(wr), length(te), length(k), length(def))
length(qb) <- n                      
length(rb) <- n
length(wr) <- n
length(te) <- n
length(k) <- n
length(def) <- n
adp <- 1:n

expectedValues <- data.frame(adp, qb, rb, wr, te, k, def)
round(expectedValues, 2)

### 3 factors
#1. Predictability (higher r-squared -> lower discount b/c more confident about expected value)
qbPredictability <- summary(qbLinear)$r.squared
rbPredictability <- 1-(deviance(rbLog)/sum((rbExpected$eVORP-mean(rbExpected$eVORP))^2))
wrPredictability <- 1-(deviance(wrLog)/sum((wrExpected$eVORP-mean(wrExpected$eVORP))^2))
tePredictability <- 1-(deviance(teLog)/sum((teExpected$eVORP-mean(teExpected$eVORP))^2))
kPredictability <- summary(kLinear)$r.squared
defPredictability <- summary(defLinear)$r.squared

qbPredictabilityRescaled <- 1 - qbPredictability
rbPredictabilityRescaled <- 1 - rbPredictability
wrPredictabilityRescaled <- 1 - wrPredictability
tePredictabilityRescaled <- 1 - tePredictability
kPredictabilityRescaled <- 1 - kPredictability
defPredictabilityRescaled <- 1 - defPredictability

#2. Position rank (later position rank -> lower discount b/c of regression to the mean)
qbPositionRank <- qbExpected$rank
rbPositionRank <- rbExpected$rank
wrPositionRank <- wrExpected$rank
tePositionRank <- teExpected$rank
kPositionRank <- kExpected$rank
defPositionRank <- defExpected$rank

qbPositionRankRescaled <- 1 - (qbPositionRank/length(qbPositionRank))
rbPositionRankRescaled <- 1 - (rbPositionRank/length(rbPositionRank))
wrPositionRankRescaled <- 1 - (wrPositionRank/length(wrPositionRank))
tePositionRankRescaled <- 1 - (tePositionRank/length(tePositionRank))
kPositionRankRescaled <- 1 - (kPositionRank/length(kPositionRank))
defPositionRankRescaled <- 1 - (defPositionRank/length(defPositionRank))

#3. Dropoff to position rank + 1 (i.e, steepness of slope; higher dropoff/steepness -> lower discount b/c bigger difference between player and next available)
qbDropoff <- abs(diff(qb))
rbDropoff <- abs(diff(rb))
wrDropoff <- abs(diff(wr))
teDropoff <- abs(diff(te))
kDropoff <- abs(diff(k))
defDropoff <- abs(diff(def))

length(qbDropoff) <- length(qbPositionRank)
length(rbDropoff) <- length(rbPositionRank)
length(wrDropoff) <- length(wrPositionRank)
length(teDropoff) <- length(tePositionRank)
length(kDropoff) <- length(kPositionRank)
length(defDropoff) <- length(defPositionRank)

qbDropoff[length(qbDropoff)] <- min(qbDropoff, na.rm=TRUE)
rbDropoff[length(rbDropoff)] <- min(rbDropoff, na.rm=TRUE)
wrDropoff[length(wrDropoff)] <- min(wrDropoff, na.rm=TRUE)
teDropoff[length(teDropoff)] <- min(teDropoff, na.rm=TRUE)
kDropoff[length(kDropoff)] <- min(kDropoff, na.rm=TRUE)
defDropoff[length(defDropoff)] <- min(defDropoff, na.rm=TRUE)

maxDropoff <- max(c(qbDropoff, rbDropoff, wrDropoff, teDropoff, kDropoff, defDropoff), na.rm=TRUE)

qbDropoffRescaled <- 1 - (qbDropoff/maxDropoff)
rbDropoffRescaled <- 1 - (rbDropoff/maxDropoff)
wrDropoffRescaled <- 1 - (wrDropoff/maxDropoff)
teDropoffRescaled <- 1 - (teDropoff/maxDropoff)
kDropoffRescaled <- 1 - (kDropoff/maxDropoff)
defDropoffRescaled <- 1 - (defDropoff/maxDropoff)

#Calculate Discount (higher discount = lower R-squared, earlier position rank, less dropoff)
qbDiscount <- mapply(function(x,y) mean(c(qbPredictabilityRescaled, x, y), na.rm=TRUE), x=qbPositionRankRescaled, y=qbDropoffRescaled)
rbDiscount <- mapply(function(x,y) mean(c(rbPredictabilityRescaled, x, y), na.rm=TRUE), x=rbPositionRankRescaled, y=rbDropoffRescaled)
wrDiscount <- mapply(function(x,y) mean(c(wrPredictabilityRescaled, x, y), na.rm=TRUE), x=wrPositionRankRescaled, y=wrDropoffRescaled)
teDiscount <- mapply(function(x,y) mean(c(tePredictabilityRescaled, x, y), na.rm=TRUE), x=tePositionRankRescaled, y=teDropoffRescaled)
kDiscount <- mapply(function(x,y) mean(c(kPredictabilityRescaled, x, y), na.rm=TRUE), x=kPositionRankRescaled, y=kDropoffRescaled)
defDiscount <- mapply(function(x,y) mean(c(defPredictabilityRescaled, x, y), na.rm=TRUE), x=defPositionRankRescaled, y=defDropoffRescaled)

length(qbDiscount) <- n                      
length(rbDiscount) <- n
length(wrDiscount) <- n
length(teDiscount) <- n
length(kDiscount) <- n
length(defDiscount) <- n
adp <- 1:n

discount <- data.frame(adp, qbDiscount, rbDiscount, wrDiscount, teDiscount, kDiscount, defDiscount)
discount[,2:7] <- discount[,2:7] - min(discount[,2:7], na.rm=TRUE)

round(discount, 2)

min(discount[,2:7], na.rm=TRUE)
max(discount[,2:7], na.rm=TRUE)

#Save data
write.csv(expectedValues, file=paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/expectedValues.csv", sep=""), row.names=FALSE)
write.csv(discount, file=paste(path, "/Fantasy Football/Research/FantasyPros/Expected VBD/discount.csv", sep=""), row.names=FALSE)
