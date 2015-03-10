###########################
# File: statProjections.R
# Description: Download projections from any site listed in the site configurations
# Date: 2/22/2015
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# To do: 
###########################
#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")
library("data.table")
library("httr")
library("pbapply")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

source(paste(getwd(),"/R Scripts/Functions/scrapeFunctions.R", sep=""))
load(paste(getwd(), "/Config/siteConfig.RData", sep=""))
load(paste(getwd(), "/Config/nflPlayers.RData", sep=""))
setnames(nflPlayers, "playerName", "player")

# Helper variables
is.season <- (weekNo == 0)
periodSelect <- ifelse(is.season, "Season", "Week")
posList <- list(qb = 1, rb = 2, wr = 3, te = 4, k = 5, dst = 6)

# Generate table of scrape sites
siteTable <- merge(projAnalysts, projSites, by = "projDataSiteId")
siteTable <- merge(siteTable, sitePositions, by = "projDataSiteId", allow.cartesian = TRUE)
siteTable <- subset(siteTable, apply(siteTable, 1, function(x)(is.na(x["limitPos"])| (length(grep(x["sitePosName"], x["limitPos"]))>0))))
siteTable <- subset(siteTable, analystName %in% sourcesOfProjections)

# Filtering out the ones that do season and/or weekly projections
if(weekNo == 0){
  siteTable <- subset(siteTable, seasonProj == 1)
} else {
  siteTable <- subset(siteTable, weekProj == 1)
}

# Generate the list of site URLs to use for scrapes
siteList <- apply(siteTable, 1, urlList)
siteUrls <- rbindlist(siteList)
rm(siteList, siteTable)

# Scrape the data
scrapeData <- pbapply(siteUrls, 1, scrapeUrl)

# Create a list of row numbers from the site URL table that represents each position
posRows <- lapply(posList, function(p)which(siteUrls$posId == p))
rm(siteUrls)

# Combine all of the projections for each position
posProj <- lapply(posRows, function(r)rbindlist(scrapeData[r], fill = TRUE))

## Combining data for dual position players
posComb <- combn(1:4, 2) # Just for QBs, RBs, WRs, and TEs. Does not include K and DEF
copyData <- apply(posComb,2, function(comb){
  data1 <- posProj[[comb[1]]]
  data2 <- posProj[[comb[2]]]
  commonPlayers <- intersect(data1$playerId, data2$playerId)
  newData1 <- data.table()
  newData2 <- data.table()
  if (length(commonPlayers) >0){
    commonData <- intersect(names(data1), names(data2))
    for(pl in commonPlayers){
      addSources1 <- setdiff(data1$analystId[data1$playerId == pl], data2$analystId[data2$playerId == pl])
      addSources2 <- setdiff(data2$analystId[data2$playerId == pl], data1$analystId[data1$playerId == pl])
      
      if(length(addSources1) >0 ){
        addData1 <- posProj[[comb[1]]][playerId == pl & analystId %in% addSources1, commonData, with = FALSE]
        newData2<- rbindlist(list(newData2, addData1), use.names = TRUE, fill = TRUE)
      }
      if(length(addSources2) >0 ){
        addData2 <- posProj[[comb[2]]][playerId == pl & analystId %in% addSources2, commonData, with = FALSE]
        newData1<- rbindlist(list(newData1, addData2), use.names = TRUE, fill = TRUE)
      }
      
    }
    result = list(newData1, newData2)
    names(result) <- c(names(posList)[comb[1]], names(posList)[comb[2]])
    return(result)
  }
}
)

appendData <- lapply(names(posList), function(pos)rbindlist(lapply(copyData, function(dt)dt[[pos]]), fill = TRUE))

for(p in 1:length(posProj)){
  posProj[[p]] <- rbindlist(list(posProj[[p]], appendData[[p]]), fill = TRUE, use.names = TRUE)
  posProj[[p]][, weight := sourceWeights[projAnalysts[projAnalystId == analystId, analystName]], by=c("analystId")]
}


# Adding up Field goals by distance for the kickers who only have that stat projection
posProj[["k"]][is.na(fg) , 
               fg:= ifelse(is.na(fg0019), 0, fg0019) + ifelse(is.na(fg2029), 0, fg2029) + 
                 ifelse(is.na(fg3039), 0 , fg3039) + ifelse(is.na(fg4049), 0, fg4049) + ifelse(is.na(fg50), 0, fg50)]

# Calculate average stats
avgProj <- lapply(posList, function(pId){
  setkeyv(posProj[[pId]], "playerId")
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "player", "analystId", "name"))
  proj <- posProj[[pId]][, lapply(.SD, function(col)mean(col, na.rm = TRUE)), by = key(posProj[[pId]]), .SDcols = dataCols]
  if(pId == 5){# Splitting out the Field goals made estimate on field goals per distance
    proj[, totFG := (fg0019+fg2029+fg3039+fg4049+fg50)]
    proj[, c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019*fg/totFG, fg2029*fg/totFG, fg3039*fg/totFG, fg4049*fg/totFG, fg50*fg/totFG)]
    proj[, totFG:=NULL]
  }
  proj[ , analystId := 18]
})


# Calculate robust average from Hodges-Lehman location estimate
medProj <- lapply(posList, function(pId){
  setkeyv(posProj[[pId]], "playerId")
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "player", "analystId", "name"))
  proj <- posProj[[pId]][, lapply(.SD, function(col)wilcox.loc(as.numeric(col))), by = key(posProj[[pId]]), .SDcols = dataCols]
  if(pId == 5){# Splitting out the Field goals made estimate on field goals per distance
    proj[, totFG := (fg0019+fg2029+fg3039+fg4049+fg50)]
    proj[, c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019*fg/totFG, fg2029*fg/totFG, fg3039*fg/totFG, fg4049*fg/totFG, fg50*fg/totFG)]
    proj[, totFG:=NULL]
  }
  proj[ , analystId := 17]
})

# Calculate weighted average
wgtProj <- lapply(posList, function(pId){
  setkeyv(posProj[[pId]], "playerId")
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "player", "analystId", "name"))
  proj <- posProj[[pId]][, lapply(.SD, function(col)weighted.mean(as.numeric(col), weight, na.rm = TRUE)), by = key(posProj[[pId]]), .SDcols = dataCols]
  if(pId == 5){# Splitting out the Field goals made estimate on field goals per distance
    proj[, totFG := (fg0019+fg2029+fg3039+fg4049+fg50)]
    proj[, c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019*fg/totFG, fg2029*fg/totFG, fg3039*fg/totFG, fg4049*fg/totFG, fg50*fg/totFG)]
    proj[, totFG:=NULL]
  }
  proj[ , analystId := 19]
})

## Updating kicker projcetions to distribute the total field goals out by distance based on average projections
posProj[["k"]] <- merge( posProj[["k"]], avgProj[["k"]][ , c("playerId", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"), with = FALSE], 
                         by = "playerId", suffixes = c("", "_avg"))
posProj[["k"]][, tot_avg := fg0019_avg + fg2029_avg + fg3039_avg + fg4049_avg+ fg50_avg]

posProj[["k"]][fg>0  & is.na(fg0019) & is.na(fg2029) & is.na(fg3039) & is.na(fg4049) & is.na(fg50) ,
               c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019_avg*fg/tot_avg, fg2029_avg*fg/tot_avg, fg3039_avg*fg/tot_avg, fg4049_avg*fg/tot_avg, fg50_avg*fg/tot_avg) ]
posProj[["k"]][, c("fg0019_avg", "fg2029_avg", "fg3039_avg", "fg4049_avg", "fg50_avg", "tot_avg") := list(NULL, NULL, NULL, NULL, NULL, NULL)]
for(colNo in 1:length(posProj[["k"]])){
  set(posProj[["k"]], which(is.na(posProj[["k"]][[colNo]])), colNo, 0)
}

aggProj <- lapply(posList, function(p)rbindlist(list(medProj[[p]], avgProj[[p]], wgtProj[[p]])))


# For analysts with missing variables set the missing variable equal to the average projection
for(pId in posList){
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "projAnalystId", "nobs"))
  for(analyst in unique(posProj[[pId]][, analystId])){
    missingVars <- names(posProj[[pId]])[posProj[[pId]][analystId == as.numeric(analyst), apply(.SD, 2, function(x)all(is.na(x)))]]
    missingVars <- intersect(missingVars, dataCols)
    posProj[[pId]][analystId == analyst, 
                   (missingVars) := avgProj[[pId]][playerId %in% posProj[[pId]][analystId == as.numeric(analyst), playerId], missingVars, with = FALSE]]
  }
}

totProj <- lapply(posList, function(p)rbindlist(list(posProj[[p]], merge(aggProj[[p]], unique(posProj[[p]][, c("playerId", "player", "name"), with = FALSE]), by = "playerId")), fill = TRUE))
totProj <- lapply(posList, function(p)totProj[[p]][, pos := toupper(names(posList)[p])])
totProj <- rbindlist(totProj, fill = TRUE)
totProj <- merge(totProj, nflPlayers[, c("playerId", "playerTeam"), with = FALSE], by = "playerId")
save(totProj, file = paste(getwd(), "/Data/totProj.RData", sep = ""))
write.csv(totProj, file = paste(getwd(), "/Data/totProj.csv", sep = ""), row.names = FALSE)

# Scoring calculations
for(pId in posList){
  setkeyv(posProj[[pId]], "playerId")
  scoringCols <- intersect(as.character(scoringRules[[pId]]$dataCol), names(posProj[[pId]]))
  multiply <- scoringRules[[pId]]$multiplier[scoringRules[[pId]]$dataCol %in% scoringCols]
  if(pId == 6 & !any(scoringCols == "dstPtsAllowed")){
    scoringCols <- c(scoringCols, "dstPtsAllowed")
    multiply <- c(multiply, 0)
  }
  posProj[[pId]][, points := sum(.SD * multiply, na.rm = TRUE) + ifelse(pId == 6, dstPts(dstPtsAllowed, scoringRules$ptsBracket), 0), .SDcols = scoringCols, by = c("playerId", "analystId")]
  posProj[[pId]][, c("points.Lo", "points.Hi"):= tryCatch(wilcox.sign(points)$conf.int, 
                                                          error = function(e)list(mean(points, na.rm = TRUE) - sd(points, na.rm = TRUE), 
                                                                                  mean(points, na.rm = TRUE) + sd(points, na.rm = TRUE)))
                 , by = key(posProj[[pId]])]
  posProj[[pId]][, c("points.Lo", "points.Hi"):= list(min(points.Lo), max(points.Hi)) , by = key(posProj[[pId]])]
  posProj[[pId]][, sdPts := mad(points, na.rm = TRUE), by = key(posProj[[pId]])]
  aggProj[[pId]][, points := sum(.SD * multiply, na.rm = TRUE) + ifelse(pId == 6, dstPts(dstPtsAllowed, scoringRules$ptsBracket), 0), .SDcols = scoringCols, by = c("playerId", "analystId")]
}

robustPoints <- lapply(posList, function(pId){
  merge(
    aggProj[[pId]][analystId == 17, c("playerId", "points", "analystId"), with = FALSE],
    unique(posProj[[pId]][, c("playerId", "player", "name", "points.Lo", "points.Hi", "sdPts"), with = FALSE]), 
    by = "playerId"
  )
})


for(pId in posList){
  posProj[[pId]][, c("points.Lo", "points.Hi", "sdPts") := list(mean(points, na.rm = TRUE) - 1.96 * sd(points, na.rm = TRUE) / sqrt(.N),
                                                                mean(points, na.rm = TRUE) + 1.96 * sd(points, na.rm = TRUE) / sqrt(.N),
                                                                sd(points, na.rm = TRUE))
                 , by = key(posProj[[pId]])]
}

averagePoints <- lapply(posList, function(pId){
  merge(
    aggProj[[pId]][analystId == 18, c("playerId", "points", "analystId"), with = FALSE],
    unique(posProj[[pId]][, c("playerId", "player", "name", "points.Lo", "points.Hi", "sdPts"), with = FALSE]), 
    by = "playerId"
  )
})

for(pId in posList){
  posProj[[pId]][, meanPts := weighted.mean(points, weight), by = key(posProj[[pId]])]
  posProj[[pId]][, sdPts  := sqrt(sum(weight * (points - meanPts)^2)/((.N-1)/.N * sum(weight))), by = key(posProj[[pId]])]
  posProj[[pId]][, c("points.Lo", "points.Hi") := list(meanPts - 1.96 * sdPts / sqrt(.N), meanPts + 1.96 * sdPts /sqrt(.N)), by = key(posProj[[pId]])]
  posProj[[pId]][, meanPts := NULL]
}

weightedPoints <- lapply(posList, function(pId){
  merge(
    aggProj[[pId]][analystId == 19, c("playerId", "points", "analystId"), with = FALSE],
    unique(posProj[[pId]][, c("playerId", "player", "name", "points.Lo", "points.Hi", "sdPts"), with = FALSE]), 
    by = "playerId"
  )
})

for(pId in posList){
  posProj[[pId]][, c("points.Lo", "points.Hi", "sdPts") := list(NULL, NULL, NULL)]
}

posPoints <- lapply(posList, function(pId){
  posProj[[pId]][,c("playerId", "player", "name", "analystId", "points"), with = FALSE]
})

aggPoints <- lapply(posList, function(pId){
  pts <- rbindlist(list(posPoints[[pId]], robustPoints[[pId]]) , fill = TRUE)
  pts <- rbindlist(list(pts, averagePoints[[pId]]), fill = TRUE )
  pts <- rbindlist(list(pts,weightedPoints[[pId]]), fill = TRUE )
  pts$pos <- toupper(names(posList)[pId])
  return(pts)
})

aggPoints <- rbindlist(aggPoints)
aggPoints <- merge(aggPoints, nflPlayers[, c("playerId", "playerTeam"), with = FALSE], by = "playerId")

save(aggPoints, file = paste(getwd(), "/Data/aggProj.RData", sep =""))
write.csv(aggPoints, file =paste(getwd(), "/Data/aggProj.csv", sep ="") )

rm(scrapeData)
gc()



