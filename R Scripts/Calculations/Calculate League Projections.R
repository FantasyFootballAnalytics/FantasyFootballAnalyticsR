###########################
# File: Calculate League Projections.R
# Description: Calculates league projections based on league settings
# Date: 3/2/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Library
library("reshape")
library("MASS")
library("psych")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Import projections data
filenames <- paste(getwd(),"/Data/", sourcesOfProjections, "-Projections.RData", sep="")
listProjections <- sapply(filenames, function(x) get(load(x)), simplify = FALSE)

#Exclude defenses and kickers for now (will add later)
for (i in 1:length(listProjections)){
  listProjections[[i]] <- listProjections[[i]][which(listProjections[[i]]$pos %in% c("QB","RB","WR","TE")),]
}

#Merge projections data
projections <- merge_recurse(listProjections, by=c("name","pos"))

#Set player name as most common instance across sources
nametable <- apply(projections[,paste("name", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) sort(table(x), TRUE))  
projections$player <- names(sapply(nametable,`[`,1) )
projections$player[which(projections$player == "")] <- NA

#Set team name as most common instance across sources
teamtable <- apply(projections[,paste("team", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) sort(table(x), TRUE)) #more efficient: names(sort(table(x), TRUE))[1]
projections$team <- names(sapply(teamtable,`[`,1) )
projections$team[which(projections$team == "")] <- NA

#Duplicate last names
projections$lastName <- gsub("Sr", "", gsub("Jr", "", gsub("III", "", gsub("[[:punct:]]", "", gsub(" ", "", sapply(str_split(projections$player, " "), "[[", 2))))))

qb <- projections[which(projections$pos == "QB"),]
rb <- projections[which(projections$pos == "RB"),]
wr <- projections[which(projections$pos == "WR"),]
te <- projections[which(projections$pos == "TE"),]

qb[qb$lastName %in% qb$lastName[duplicated(qb$lastName)], c("player","team")]
rb[rb$lastName %in% rb$lastName[duplicated(rb$lastName)], c("player","team")]
wr[wr$lastName %in% wr$lastName[duplicated(wr$lastName)], c("player","team")]
te[te$lastName %in% te$lastName[duplicated(te$lastName)], c("player","team")]

#Remove duplicate cases
projections[projections$name %in% projections$name[duplicated(projections$name)],]

#Same name, different player

#Same player, different position
dropNames <- c("DENARDROBINSON","DEXTERMCCLUSTER","THEORIDDICK","ORSONCHARLES","JOEWEBB","EMILIGWENAGU","EVANRODRIGUEZ","BRADSMELLEY","RICHIEBROCKEL","BEARPASCOE","JEDCOLLINS","MARCUSTHIGPEN","JORDANLYNCH")
dropVariables <- c("pos","pos","pos","pos","pos","pos","pos","pos","pos","pos","pos","pos","pos")
dropLabels <- c("RB","WR","WR","TE","WR","RB","TE","TE","RB","RB","TE","WR","QB")

projections2 <- ddply(projections, .(name), numcolwise(mean), na.rm=TRUE)

for(i in 1:length(dropNames)){
  if(dim(projections[-which(projections[,"name"] == dropNames[i] & projections[,dropVariables[i]] == dropLabels[i]),])[1] > 0){
    projections <- projections[-which(projections[,"name"] == dropNames[i] & projections[,dropVariables[i]] == dropLabels[i]),]
  }
}

projections <- merge(projections2, projections[,c("name","player","pos","team")], by="name", all.x=TRUE)

#Calculate stat categories for each source
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste("passIncomp", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passAtt", sourcesOfProjectionsAbbreviation[i], sep="_")] - projections[,paste("passComp", sourcesOfProjectionsAbbreviation[i], sep="_")]
}

#Calculate average of categories
projections$passAtt <- rowMeans(projections[,paste("passAtt", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passComp <- rowMeans(projections[,paste("passComp", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passIncomp <- rowMeans(projections[,paste("passIncomp", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passYds <- rowMeans(projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passTds <- rowMeans(projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passInt <- rowMeans(projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushAtt <- rowMeans(projections[,paste("rushAtt", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushYds <- rowMeans(projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushTds <- rowMeans(projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rec <- rowMeans(projections[,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$recYds <- rowMeans(projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$recTds <- rowMeans(projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$returnTds <- rowMeans(projections[,paste("returnTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$twoPts <- rowMeans(projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$fumbles <- rowMeans(projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)

#Calculate Hodges-Lehmann (pseudo-median) robust average of categories
projections$passAttMedian <- apply(projections[,paste("passAtt", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passCompMedian <- apply(projections[,paste("passComp", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passIncompMedian <- apply(projections[,paste("passIncomp", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passYdsMedian <- apply(projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passTdsMedian <- apply(projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passIntMedian <- apply(projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$rushAttMedian <- apply(projections[,paste("rushAtt", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$rushYdsMedian <- apply(projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$rushTdsMedian <- apply(projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$recMedian <- apply(projections[,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$recYdsMedian <- apply(projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$recTdsMedian <- apply(projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$returnTdsMedian <- apply(projections[,paste("returnTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$twoPtsMedian <- apply(projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$fumblesMedian <- apply(projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))

#Check projections
projections[,c("name",paste("passAtt", sourcesOfProjectionsAbbreviation, sep="_"), c("passAtt","passAttMedian"))]
projections[,c("name",paste("passComp", sourcesOfProjectionsAbbreviation, sep="_"), c("passComp","passCompMedian"))]
projections[,c("name",paste("passIncomp", sourcesOfProjectionsAbbreviation, sep="_"), c("passIncomp","passIncompMedian"))]
projections[,c("name",paste("passYds", sourcesOfProjectionsAbbreviation, sep="_"), c("passYds","passYdsMedian"))]
projections[,c("name",paste("passTds", sourcesOfProjectionsAbbreviation, sep="_"), c("passTds","passTdsMedian"))]
projections[,c("name",paste("passInt", sourcesOfProjectionsAbbreviation, sep="_"), c("passInt","passIntMedian"))]
projections[,c("name",paste("rushAtt", sourcesOfProjectionsAbbreviation, sep="_"), c("rushAtt","rushAttMedian"))]
projections[,c("name",paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_"), c("rushYds","rushYdsMedian"))]
projections[,c("name",paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_"), c("rushTds","rushTdsMedian"))]
projections[,c("name",paste("rec", sourcesOfProjectionsAbbreviation, sep="_"), c("rec","recMedian"))]
projections[,c("name",paste("recYds", sourcesOfProjectionsAbbreviation, sep="_"), c("recYds","recYdsMedian"))]
projections[,c("name",paste("recTds", sourcesOfProjectionsAbbreviation, sep="_"), c("recTds","recTdsMedian"))]
projections[,c("name",paste("returnTds", sourcesOfProjectionsAbbreviation, sep="_"), c("returnTds","returnTdsMedian"))]
projections[,c("name",paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_"), c("twoPts","twoPtsMedian"))]
projections[,c("name",paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_"), c("fumbles","fumblesMedian"))]

#Calculate projected points for your league (average projections)
projections$passAttPts <- projections$passAtt * passAttMultiplier
projections$passCompPts <- projections$passComp * passCompMultiplier
projections$passIncompPts <- projections$passIncomp * passIncompMultiplier
projections$passYdsPts <- projections$passYds * passYdsMultiplier
projections$passTdsPts <- projections$passTds * passTdsMultiplier
projections$passIntPts <- projections$passInt * passIntMultiplier
projections$rushAttPts <- projections$rushAtt * rushAttMultiplier
projections$rushYdsPts <- projections$rushYds * rushYdsMultiplier
projections$rushTdsPts <- projections$rushTds * rushTdsMultiplier
projections$recPts <- projections$rec * recMultiplier
projections$recYdsPts <- projections$recYds * recYdsMultiplier
projections$recTdsPts <- projections$recTds * recTdsMultiplier
projections$returnTdsPts <- projections$returnTds * returnTdsMultiplier
projections$twoPtsPts <- projections$twoPts * twoPtsMultiplier
projections$fumblesPts <- projections$fumbles * fumlMultiplier

projections$passAttMedianPts <- projections$passAttMedian * passAttMultiplier
projections$passCompMedianPts <- projections$passCompMedian * passCompMultiplier
projections$passIncompMedianPts <- projections$passIncompMedian * passIncompMultiplier
projections$passYdsMedianPts <- projections$passYdsMedian * passYdsMultiplier
projections$passTdsMedianPts <- projections$passTdsMedian * passTdsMultiplier
projections$passIntMedianPts <- projections$passIntMedian * passIntMultiplier
projections$rushAttMedianPts <- projections$rushAttMedian * rushAttMultiplier
projections$rushYdsMedianPts <- projections$rushYdsMedian * rushYdsMultiplier
projections$rushTdsMedianPts <- projections$rushTdsMedian * rushTdsMultiplier
projections$recMedianPts <- projections$recMedian * recMultiplier
projections$recYdsMedianPts <- projections$recYdsMedian * recYdsMultiplier
projections$recTdsMedianPts <- projections$recTdsMedian * recTdsMultiplier
projections$returnTdsMedianPts <- projections$returnTdsMedian * returnTdsMultiplier
projections$twoPtsMedianPts <- projections$twoPtsMedian * twoPtsMultiplier
projections$fumblesMedianPts <- projections$fumblesMedian * fumlMultiplier

#Check projections
projections[,c("passAttPts","passAttMedianPts")]
projections[,c("passCompPts","passCompMedianPts")]
projections[,c("passIncompPts","passIncompMedianPts")]
projections[,c("passYdsPts","passYdsMedianPts")]
projections[,c("passTdsPts","passTdsMedianPts")]
projections[,c("passIntPts","passIntMedianPts")]
projections[,c("rushAttPts","rushAttMedianPts")]
projections[,c("rushYdsPts","rushYdsMedianPts")]
projections[,c("rushTdsPts","rushYdsMedianPts")]
projections[,c("recPts","recMedianPts")]
projections[,c("recYdsPts","recYdsMedianPts")]
projections[,c("recTdsPts","recTdsMedianPts")]
projections[,c("returnTdsPts","returnTdsMedianPts")]
projections[,c("twoPtsPts","twoPtsMedianPts")]
projections[,c("fumblesPts","fumblesMedianPts")]

projections$projectedPtsMean <- mySum(projections[,c("passAttPts","passCompPts","passIncompPts","passYdsPts","passTdsPts","passIntPts","rushAttPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","returnTdsPts","twoPtsPts","fumblesPts")])
projections$projectedPtsMedian <- mySum(projections[,c("passAttMedianPts","passCompMedianPts","passIncompMedianPts","passYdsMedianPts","passTdsMedianPts","passIntMedianPts","rushAttMedianPts","rushYdsMedianPts","rushTdsMedianPts","recMedianPts","recYdsMedianPts","recTdsMedianPts","returnTdsMedianPts","twoPtsMedianPts","fumblesMedianPts")])

#If variable is all missing for source, impute mean of other sources
if(sum(unlist(lapply(projections[,paste("passAtt", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("passAtt", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("passAtt", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$passAttMedian
}
if(sum(unlist(lapply(projections[,paste("passComp", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("passComp", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("passComp", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$passCompMedian
}
if(sum(unlist(lapply(projections[,paste("passIncomp", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("passIncomp", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("passIncomp", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$passIncompMedian
}
if(sum(unlist(lapply(projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$passYdsMedian
}
if(sum(unlist(lapply(projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$passTdsMedian
}
if(sum(unlist(lapply(projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$passIntMedian
}
if(sum(unlist(lapply(projections[,paste("rushAtt", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("rushAtt", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("rushAtt", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$rushAttMedian
}
if(sum(unlist(lapply(projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$rushYdsMedian
}
if(sum(unlist(lapply(projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$rushTdsMedian
}
if(sum(unlist(lapply(projections[,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$recMedian
}
if(sum(unlist(lapply(projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$recYdsMedian
}
if(sum(unlist(lapply(projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$recTdsMedian
}
if(sum(unlist(lapply(projections[,paste("returnTds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("returnTds", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("returnTds", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$returnTdsMedian
}
if(sum(unlist(lapply(projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$twoPtsMedian
}
if(sum(unlist(lapply(projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))) > 0){
  projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")][,unlist(lapply(projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")], function(x) all(is.na(x))))] <- projections$fumblesMedian
}

#Calculate projections for each source
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste(c("passAttPts","passCompPts","passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","returnTdsPts","twoPtsPts","fumblesPts"), sourcesOfProjectionsAbbreviation[i], sep="_")] <- NA
  
  projections[,paste("passAttPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passAtt", sourcesOfProjectionsAbbreviation[i], sep="_")] * passAttMultiplier
  projections[,paste("passCompPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passComp", sourcesOfProjectionsAbbreviation[i], sep="_")] * passCompMultiplier
  projections[,paste("passIncompPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passIncomp", sourcesOfProjectionsAbbreviation[i], sep="_")] * passIncompMultiplier
  projections[,paste("passYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * passYdsMultiplier
  projections[,paste("passTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * passTdsMultiplier
  projections[,paste("passIntPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("passInt", sourcesOfProjectionsAbbreviation[i], sep="_")] * passIntMultiplier
  projections[,paste("rushAttPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rushAtt", sourcesOfProjectionsAbbreviation[i], sep="_")] * rushAttMultiplier
  projections[,paste("rushYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rushYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * rushYdsMultiplier
  projections[,paste("rushTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rushTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * rushTdsMultiplier
  projections[,paste("recPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("rec", sourcesOfProjectionsAbbreviation[i], sep="_")] * recMultiplier
  projections[,paste("recYdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("recYds", sourcesOfProjectionsAbbreviation[i], sep="_")] * recYdsMultiplier
  projections[,paste("recTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("recTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * recTdsMultiplier
  projections[,paste("returnTdsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("returnTds", sourcesOfProjectionsAbbreviation[i], sep="_")] * returnTdsMultiplier
  projections[,paste("twoPtsPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("twoPts", sourcesOfProjectionsAbbreviation[i], sep="_")] * twoPtsMultiplier
  projections[,paste("fumblesPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- projections[,paste("fumbles", sourcesOfProjectionsAbbreviation[i], sep="_")] * fumlMultiplier
  
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- NA
  
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] <- mySum(projections[,paste(c("passAttPts","passIncompPts","passYdsPts","passTdsPts","passIntPts","rushAttPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","returnTdsPts","twoPtsPts","fumblesPts"), sourcesOfProjectionsAbbreviation[i], sep="_")])
}

#Remove WalterFootball projections because they don't separate rushing TDs from receiving TDs
projections$projectedPts_wf <- NA

#Check projections
projections[,c("name",paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))]
projections[,c("name","projectedPtsMean","projectedPtsMedian")]

projectionVars <- projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))]

#Convert Zeros to NA
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")][which(projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] == 0)] <- NA
}

is.na(projectionVars) <- projectionVars==0

#Describe
describe(projectionVars)
plot(density(na.omit(projections$projectedPtsMean)), col="black")
lines(density(na.omit(projections$projectedPtsMedian)), col="blue")

#Correlations among projections
cor(projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))], use="pairwise.complete.obs")

#Set criterion for projections based on whose projections are most accurate
projections$projections <- projections$projectedPtsMedian

#Calculate overall rank
projections$overallRank <- rank(-projections$projections, ties.method="min")

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]

#Keep important variables
projections <- projections[,c("name","player","pos","team","overallRank","projections",
                              paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"),
                              c("passAttMedian","passCompMedian","passIncompMedian","passYdsMedian","passTdsMedian","passIntMedian","rushAttMedian","rushYdsMedian","rushTdsMedian","recMedian","recYdsMedian","recTdsMedian","returnTdsMedian","twoPtsMedian","fumblesMedian"),
                              c("projectedPtsMean","projectedPtsMedian"))]

#View projections
projections
projections[,c("name","pos","team","projectedPts_fp","projectedPtsMean","projectedPtsMedian")]
projections[,c("name","pos","team",c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"),"projectedPtsMean","projectedPtsMedian"))]

#Density Plot
pointDensity <- c(projections$projectedPts_accu, projections$projectedPts_cbs1, projections$projectedPts_cbs2, projections$projectedPts_espn, projections$projectedPts_nfl, projections$projectedPts_fs, projections$projectedPts_fp, projections$projectedPts_fftoday, projections$projectedPts_fbg1, projections$projectedPts_fbg2, projections$projectedPts_fbg3, projections$projectedPts_fbg4, projections$projectedPts_fox) #,projections$projectedPts_accu, projections$projectedPts_cbs, projections$projectedPts_yahoo, projections$projectedPtsLatent, projections$projectedPtsMean
sourceDensity <- c(rep("Accuscore",dim(projections)[1]), rep("CBS1",dim(projections)[1]), rep("CBS2",dim(projections)[1]), rep("ESPN",dim(projections)[1]), rep("NFL.com",dim(projections)[1]), rep("FantasySharks",dim(projections)[1]), rep("FantasyPros",dim(projections)[1]), rep("FFtoday",dim(projections)[1]), rep("Footballguys1",dim(projections)[1]), rep("Footballguys2",dim(projections)[1]), rep("Footballguys3",dim(projections)[1]), rep("Footballguys4",dim(projections)[1]), rep("FOX",dim(projections)[1])) #,rep("Accuscore",dim(projections)[1]), rep("CBS",dim(projections)[1]), rep("Yahoo",dim(projections)[1]), rep("Latent",dim(projections)[1]), rep("Average",dim(projections)[1])
densityData <- data.frame(pointDensity, sourceDensity)

ggplot(densityData, aes(x=pointDensity, fill=sourceDensity)) + geom_density(alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Projected Points") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Calculate projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/LeagueProjections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/LeagueProjections.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Projections/LeagueProjections-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/LeagueProjections-2014.csv", sep=""), row.names=FALSE)
