###########################
# File: Weekly Simulation.R
# Description: Simulates weekly performance for each player based on projected points
# Date: 7/19/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Library

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Data
load(paste(getwd(),"/Data/LeagueProjections.RData", sep=""))
load(paste(getwd(),"/Data/sdAverage.RData", sep=""))

#Simulation
simulations <- 100
games <- 16

#Pass Yards
passYds <- list()
passTds <- list()
passInt <- list()
rushYds <- list()
rushTds <- list()
rec <- list()
recYds <- list()
recTds <- list()
twoPts <- list()
fumbles <- list()

pb <- txtProgressBar(min = 0, max = simulations, style = 3)
for(i in 1:simulations){
  setTxtProgressBar(pb, i)
  passYds[[i]] <- t(sapply(projections$passYdsMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sdAverage$sdPassYds), error=function(e) rep(NA, games))))
  passTds[[i]] <- t(sapply(projections$passTdsMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sdAverage$sdPassTds), error=function(e) rep(NA, games))))
  passInt[[i]] <- t(sapply(projections$passIntMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sdAverage$sdPassInt), error=function(e) rep(NA, games))))
  rushYds[[i]] <- t(sapply(projections$rushYdsMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sdAverage$sdRushYds), error=function(e) rep(NA, games))))
  rushTds[[i]] <- t(sapply(projections$rushTdsMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sdAverage$sdRushTds), error=function(e) rep(NA, games))))
  rec[[i]] <- t(sapply(projections$recMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sdAverage$sdRec), error=function(e) rep(NA, games))))
  recYds[[i]] <- t(sapply(projections$recYdsMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sdAverage$sdRecYds), error=function(e) rep(NA, games))))
  recTds[[i]] <- t(sapply(projections$recTdsMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sdAverage$sdRecTds), error=function(e) rep(NA, games))))
  twoPts[[i]] <- t(sapply(projections$twoPtsMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sd(c(rep(0, games - 1), 1))), error=function(e) rep(NA, games))))
  fumbles[[i]] <- t(sapply(projections$fumblesMedian, function(x) tryCatch(simulateIntegers(n=games, sum=x, sd=sd(c(rep(0, games - 1), 1))), error=function(e) rep(NA, games))))
}

#Convert NAs to 0
for(i in 1:simulations){
  passYds[[i]][is.na(passYds[[i]])] <- 0
  passTds[[i]][is.na(passTds[[i]])] <- 0
  passInt[[i]][is.na(passInt[[i]])] <- 0
  rushYds[[i]][is.na(rushYds[[i]])] <- 0
  rushTds[[i]][is.na(rushTds[[i]])] <- 0
  rec[[i]][is.na(rec[[i]])] <- 0
  recYds[[i]][is.na(recYds[[i]])] <- 0
  recTds[[i]][is.na(recTds[[i]])] <- 0
  twoPts[[i]][is.na(twoPts[[i]])] <- 0
  fumbles[[i]][is.na(fumbles[[i]])] <- 0
}

#Calculate fantasy points per week
passYdsPts <- list()
passTdsPts <- list()
passIntPts <- list()
rushYdsPts <- list()
rushTdsPts <- list()
recPts <- list()
recYdsPts <- list()
recTdsPts <- list()
twoPtsPts <- list()
fumblesPts <- list()
fantasyPts <- list()

for(i in 1:simulations){  
  passYdsPts[[i]] <- passYds[[i]] * passYdsMultiplier
  passTdsPts[[i]] <- passTds[[i]] * passTdsMultiplier
  passIntPts[[i]] <- passInt[[i]] * passIntMultiplier
  rushYdsPts[[i]] <- rushYds[[i]] * rushYdsMultiplier
  rushTdsPts[[i]] <- rushTds[[i]] * rushTdsMultiplier
  recPts[[i]] <- rec[[i]] * recMultiplier
  recYdsPts[[i]] <- recYds[[i]] * recYdsMultiplier
  recTdsPts[[i]] <- recTds[[i]] * recTdsMultiplier
  twoPtsPts[[i]] <- twoPts[[i]] * twoPtsMultiplier
  fumblesPts[[i]] <- fumbles[[i]] * fumlMultiplier
  
  fantasyPts[[i]] <- passYdsPts[[i]] + passTdsPts[[i]] + passIntPts[[i]] + rushYdsPts[[i]] + rushTdsPts[[i]] + recPts[[i]] + recYdsPts[[i]] + recTdsPts[[i]] + twoPtsPts[[i]] + fumblesPts[[i]]
}

#Check if enough simulations
hist(fantasyPts[[i]][1,])
plot(density(na.omit(fantasyPts[[i]][1,])))

#Calculate sd of fantasy points per week
sdWeeklyPts <- matrix(nrow=NROW(fantasyPts[[1]]), ncol=simulations)

for(i in 1:simulations){  
  sdWeeklyPts[,i] <- apply(fantasyPts[[i]], 1, function(x) sd(x, na.rm=TRUE))
}

#Convert 0s to NA
sdWeeklyPts[sdWeeklyPts == 0] <- NA

#Calculate robust average of weekly SD
projections$weeklySD <- apply(sdWeeklyPts, 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))

#Calculate weekly variability controlling for total points
projections$weeklyVariability <- scale(resid(lm(weeklySD ~ projectedPtsMedian, data=projections, na.action="na.exclude")))

#Rescale weekly variability to have mean of 5 and sd of 2
projections$weeklyVariability <- ((projections$weeklyVariability * 2/(sd(projections$weeklyVariability, na.rm=TRUE))) + (5-(mean(projections$weeklyVariability, na.rm=TRUE))))

#Calculate total TDs and yards per rec
projections$ydsPerRec <- projections$recYdsMedian / projections$recMedian
projections$ydsPerRec[is.infinite(projections$ydsPerRec)] <- NA
projections$totalTds <- rowSums(projections[,c("passTdsMedian","rushTdsMedian","recTdsMedian")], na.rm=TRUE)

#Check that weeklySD makes sense (higher TDs + higher ydsPerRec)
projections[,c("player","totalTds","ydsPerRec","weeklySD","weeklyVariability")]

cor.test(projections$totalTds, projections$weeklySD)
cor.test(projections$ydsPerRec, projections$weeklySD)

cor.test(projections$totalTds, projections$weeklyVariability)
cor.test(projections$ydsPerRec, projections$weeklyVariability)

#Save file
save(projections, file = paste(getwd(),"/Data/WeeklyVariability.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/WeeklyVariability.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Projections/WeeklyVariability-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/WeeklyVariability-2014.csv", sep=""), row.names=FALSE)
