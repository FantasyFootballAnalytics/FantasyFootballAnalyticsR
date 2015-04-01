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
source(paste(getwd(),"/R Scripts/Functions/mySqlDBFunctions.R", sep=""))

# Helper variables
is.season <- (weekNo == 0)
periodSelect <- ifelse(is.season, "season", "week")


playerDb <- connectDb("ffplayerinfo")
siteDb <- connectDb("projectionsites")

nflPlayers <- data.table(dbReadTable(playerDb, "players"))
positions <- dbReadTable(playerDb, "playerpositions")
posList <- as.list(positions$positionId)
posMap <- dbGetQuery(playerDb, "select positionCode, detailPosition from positionmap;")
names(posList) <- positions$positionCode
urlTable <- dbReadTable(siteDb, "urlinfo")
urlTable <- urlTable[urlTable$urlPeriod == periodSelect,]

analysts <- dbReadTable(siteDb, "analysts")

calcWeighted <- FALSE
if(diff(range(sourceWeights)) > sqrt(.Machine$double.eps)){
  for(r in 1:nrow(analysts)){
    calcWeighted <- TRUE
    analysts$weight[r] <- sourceWeights[analysts$analystName]
  }
}

sites <- dbReadTable(siteDb, "sites")
siteTables <- dbReadTable(siteDb, "siteTables")
tableColumns <- dbReadTable(siteDb, "tablecolumns")
dataColumns <- dbReadTable(siteDb, "datacolumns")
tableColumns <- tableColumns[tableColumns$columnPeriod == periodSelect,]
tableColumns <- merge (x = tableColumns, y = dataColumns, by.x = "dataColID", by.y = "dataColId")
tableRowRemove <- dbReadTable(siteDb, "tablerowremove")

finalVarNames <- c( "playerId", "analystId", 
                   unique(tableColumns$columnName[tableColumns$removeColumn == 0]))

selectAnalysts <- analysts$analystId[analysts$analystName %in% sourcesOfProjections]

# Generate table of scrape sites
urlTable <- urlTable[urlTable$analystId %in% selectAnalysts, ]

# Generate the list of site URLs to use for scrapes
siteList <- apply(urlTable, 1, urlList)
siteUrls <- rbindlist(siteList)
rm(siteList, urlTable)

# Scrape the data
scrapeData <- pbapply(siteUrls, 1, scrapeUrl)

# Create a list of row numbers from the site URL table that represents each position
posRows <- lapply(posList, function(p)which(siteUrls$positionId == p))
rm(siteUrls, sites, siteTables, tableColumns, dataColumns, tableRowRemove)

# Combine all of the projections for each position
posProj <- lapply(posRows, function(r)rbindlist(scrapeData[r], fill = TRUE))
rm(scrapeData, posRows)

## Combining data for dual position players
posComb <- cbind(combn(c(1:4), 2), combn(c(7:9), 2))

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
}

rm(posComb, copyData, appendData, p)

# Adding up Field goals by distance for the kickers who only have that stat projection
posProj[["K"]][is.na(fg) , 
               fg:= ifelse(is.na(fg0019), 0, fg0019) + ifelse(is.na(fg2029), 0, fg2029) + 
                 ifelse(is.na(fg3039), 0 , fg3039) + ifelse(is.na(fg4049), 0, fg4049) + ifelse(is.na(fg50), 0, fg50)]

# Connecting to projections database and writing the projections to the database
playerProj <- connectDb("playerprojections")
scrapeId <- getScrapeId(weekNo, season)
writeProj2DB(scrapeId, posProj, selectAnalysts)

# Reading ALL projections from the database as a basis for calculations
posProj <- readProjFromDB(scrapeId, posList)
names(posProj) <- names(posList)

for(p in 1:length(posProj)){
  if(exists("analystId", posProj[[p]])){
    posProj[[p]] <- merge(x=posProj[[p]], y=analysts[, c("analystId", "weight")], by = "analystId")
  }
}

# Calculate average stats
avgProj <- lapply(posList, function(pId){
  if(nrow(posProj[[pId]])== 0){
    return(data.table())
  }
  setkeyv(posProj[[pId]], "playerId")
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "analystId", "dataScrapeId", "weight"))
  
  proj <- posProj[[pId]][, lapply(.SD, function(col)mean(col, na.rm = TRUE)), by = key(posProj[[pId]]), .SDcols = dataCols]
  
  if(pId == 5){# Splitting out the Field goals made estimate on field goals per distance
    proj[, totFG := (fg0019+fg2029+fg3039+fg4049+fg50)]
    proj[, c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019*fg/totFG, fg2029*fg/totFG, fg3039*fg/totFG, fg4049*fg/totFG, fg50*fg/totFG)]
    proj[, totFG:=NULL]
  }
  return(proj)
})

# Writting the average stats to the database
writeProj2DB(scrapeId, avgProj, dataType = "averageproj")

# Calculate robust average from Hodges-Lehman location estimate
medProj <- lapply(posList, function(pId){
  if(nrow(posProj[[pId]])== 0){
    return(data.table())
  }
  
  setkeyv(posProj[[pId]], "playerId")
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "weight", "analystId", "dataScrapeId"))
  proj <- posProj[[pId]][, lapply(.SD, function(col)wilcox.loc(as.numeric(col))), by = key(posProj[[pId]]), .SDcols = dataCols]
  if(pId == 5){# Splitting out the Field goals made estimate on field goals per distance
    proj[, totFG := (fg0019+fg2029+fg3039+fg4049+fg50)]
    proj[, c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019*fg/totFG, fg2029*fg/totFG, fg3039*fg/totFG, fg4049*fg/totFG, fg50*fg/totFG)]
    proj[, totFG:=NULL]
  }
  return(proj)
})

# Writing robust average stats to the database
writeProj2DB(scrapeId, medProj, dataType = "robustproj")

# Calculate weighted average
if(calcWeighted){
  wgtProj <- lapply(posList, function(pId){
    if(nrow(posProj[[pId]])== 0){
      return(data.table())
    }
    setkeyv(posProj[[pId]], "playerId")
    dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "weight", "analystId", "dataScrapeId"))
    proj <- posProj[[pId]][, lapply(.SD, function(col)weighted.mean(as.numeric(col), weight, na.rm = TRUE)), by = key(posProj[[pId]]), .SDcols = dataCols]
    if(pId == 5){# Splitting out the Field goals made estimate on field goals per distance
      proj[, totFG := (fg0019+fg2029+fg3039+fg4049+fg50)]
      proj[, c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019*fg/totFG, fg2029*fg/totFG, fg3039*fg/totFG, fg4049*fg/totFG, fg50*fg/totFG)]
      proj[, totFG:=NULL]
    }
  })
  
  # Writing the weighted average stats to the database
  writeProj2DB(scrapeId, posProj, dataType = "weightproj")
}

## Updating kicker projcetions to distribute the total field goals out by distance based on average projections
posProj[["K"]] <- merge( posProj[["K"]], avgProj[["K"]][ , c("playerId", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"), with = FALSE], 
                         by = "playerId", suffixes = c("", "_avg"))
posProj[["K"]][, tot_avg := fg0019_avg + fg2029_avg + fg3039_avg + fg4049_avg+ fg50_avg]

posProj[["K"]][fg>0  & is.na(fg0019) & is.na(fg2029) & is.na(fg3039) & is.na(fg4049) & is.na(fg50) ,
               c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019_avg*fg/tot_avg, fg2029_avg*fg/tot_avg, fg3039_avg*fg/tot_avg, fg4049_avg*fg/tot_avg, fg50_avg*fg/tot_avg) ]
posProj[["K"]][, c("fg0019_avg", "fg2029_avg", "fg3039_avg", "fg4049_avg", "fg50_avg", "tot_avg") := list(NULL, NULL, NULL, NULL, NULL, NULL)]
for(colNo in 1:length(posProj[["K"]])){
  set(posProj[["K"]], which(is.na(posProj[["K"]][[colNo]])), colNo, 0)
}


### Below are calculations of points 

# For analysts with missing variables set the missing variable equal to the average projection
for(pId in posList){
  if(nrow(posProj[[pId]]) > 0 ){
    
    dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "analystId"))
    for(analyst in unique(posProj[[pId]][, analystId])){
      missingVars <- names(posProj[[pId]])[posProj[[pId]][analystId == as.numeric(analyst), apply(.SD, 2, function(x)all(is.na(x)))]]
      missingVars <- intersect(missingVars, dataCols)
      
      playerList <- posProj[[pId]][analystId == analyst, playerId]
      posProj[[pId]][analystId == analyst, 
                     (missingVars) := avgProj[[pId]][playerId %in%  playerList, missingVars, with = FALSE]]
    }
  }
}


#totProj <- lapply(posList, function(p)rbindlist(list(posProj[[p]], merge(aggProj[[p]], unique(posProj[[p]][, c("playerId", "player", "name"), with = FALSE]), by = "playerId")), fill = TRUE))
#totProj <- lapply(posList, function(p)totProj[[p]][, pos := toupper(names(posList)[p])])
#totProj <- rbindlist(totProj, fill = TRUE)
#totProj <- merge(totProj, nflPlayers[, c("playerId", "playerTeam"), with = FALSE], by = "playerId")
#save(totProj, file = paste(getwd(), "/Data/totProj.RData", sep = ""))
#write.csv(totProj, file = paste(getwd(), "/Data/totProj.csv", sep = ""), row.names = FALSE)

# Scoring calculations
for(pId in posList){
  if(nrow(posProj[[pId]]) > 0){    
    setkeyv(posProj[[pId]], "playerId")
    scoringCols <- intersect(as.character(scoringRules[[pId]]$dataCol), names(posProj[[pId]]))
    multiply <- scoringRules[[pId]]$multiplier[scoringRules[[pId]]$dataCol %in% scoringCols]
    if(pId == 6 & !any(scoringCols == "dstPtsAllow")){
      scoringCols <- c(scoringCols, "dstPtsAllow")
      multiply <- c(multiply, 0)
    }
    posProj[[pId]][, points := sum(.SD * multiply, na.rm = TRUE) + ifelse(pId == 6, dstPts(dstPtsAllow, scoringRules$ptsBracket), 0), .SDcols = scoringCols, by = c("playerId", "analystId")]
    posProj[[pId]][, c("points.Lo", "points.Hi"):= tryCatch(wilcox.sign(points)$conf.int, 
                                                            error = function(e)list(mean(points, na.rm = TRUE) - sd(points, na.rm = TRUE), 
                                                                                    mean(points, na.rm = TRUE) + sd(points, na.rm = TRUE)))
                   , by = key(posProj[[pId]])]
    posProj[[pId]][, c("points.Lo", "points.Hi"):= list(min(points.Lo), max(points.Hi)) , by = key(posProj[[pId]])]
    posProj[[pId]][, sdPts := mad(points, na.rm = TRUE), by = key(posProj[[pId]])]
    medProj[[pId]][, points := sum(.SD * multiply, na.rm = TRUE) + ifelse(pId == 6, dstPts(dstPtsAllow, scoringRules$ptsBracket), 0), .SDcols = scoringCols, by = c("playerId")]
    avgProj[[pId]][, points := sum(.SD * multiply, na.rm = TRUE) + ifelse(pId == 6, dstPts(dstPtsAllow, scoringRules$ptsBracket), 0), .SDcols = scoringCols, by = c("playerId")]
    if(calcWeighted)
      wgtProj[[pId]][, points := sum(.SD * multiply, na.rm = TRUE) + ifelse(pId == 6, dstPts(dstPtsAllow, scoringRules$ptsBracket), 0), .SDcols = scoringCols, by = c("playerId")]
  }
}

robustPoints <- lapply(posList, function(pId){
  if(nrow(medProj[[pId]]) >0 & nrow(posProj[[pId]])>0){
    merge(
      medProj[[pId]][, c("playerId", "points"), with = FALSE],
      unique(posProj[[pId]][, c("playerId",  "points.Lo", "points.Hi", "sdPts"), with = FALSE]), 
      by = "playerId"
    )
  }
})


for(pId in posList){
  if(nrow(posProj[[pId]])> 0){
    posProj[[pId]][, c("points.Lo", "points.Hi", "sdPts") := list(mean(points, na.rm = TRUE) - 1.96 * sd(points, na.rm = TRUE) / sqrt(.N),
                                                                  mean(points, na.rm = TRUE) + 1.96 * sd(points, na.rm = TRUE) / sqrt(.N),
                                                                  sd(points, na.rm = TRUE))
                   , by = key(posProj[[pId]])]
  }
}

averagePoints <- lapply(posList, function(pId){
  if(nrow(avgProj[[pId]]) >0 & nrow(posProj[[pId]])>0){
    merge(
      avgProj[[pId]][, c("playerId", "points"), with = FALSE],
      unique(posProj[[pId]][, c("playerId", "points.Lo", "points.Hi", "sdPts"), with = FALSE]), 
      by = "playerId"
    )
  }
})

if(calcWeighted){
  for(pId in posList){
    posProj[[pId]][, meanPts := weighted.mean(points, weight), by = key(posProj[[pId]])]
    posProj[[pId]][, sdPts  := weighted.sd(points, weight), by = key(posProj[[pId]])]
    posProj[[pId]][, c("points.Lo", "points.Hi") := list(meanPts - 1.96 * sdPts / sqrt(.N), meanPts + 1.96 * sdPts /sqrt(.N)), by = key(posProj[[pId]])]
    posProj[[pId]][, meanPts := NULL]
  }
  

  weightedPoints <- lapply(posList, function(pId){
    merge(
      wgtProj[[pId]][, c("playerId", "points", "analystId"), with = FALSE],
      unique(posProj[[pId]][, c("playerId", "points.Lo", "points.Hi", "sdPts"), with = FALSE]), 
      by = "playerId"
    )
  })
}
for(pId in posList){
  if(nrow(posProj[[pId]])>0){
    posProj[[pId]][, c("points.Lo", "points.Hi", "sdPts") := list(NULL, NULL, NULL)]
  }
}

posPoints <- lapply(posList, function(pId){
  if(nrow(posProj[[pId]])>0){
    posProj[[pId]][,c("playerId", "analystId", "points"), with = FALSE]
  }
})

#aggPoints <- lapply(posList, function(pId){
#  pts <- rbindlist(list(posPoints[[pId]], robustPoints[[pId]]) , fill = TRUE)
#  pts <- rbindlist(list(pts, averagePoints[[pId]]), fill = TRUE )
#  pts <- rbindlist(list(pts,weightedPoints[[pId]]), fill = TRUE )
#  pts$pos <- toupper(names(posList)[pId])
#  return(pts)
#})

#aggPoints <- rbindlist(aggPoints)
#aggPoints <- merge(aggPoints, nflPlayers[, c("playerId", "playerTeam"), with = FALSE], by = "playerId")

#save(aggPoints, file = paste(getwd(), "/Data/aggProj.RData", sep =""))
#write.csv(aggPoints, file =paste(getwd(), "/Data/aggProj.csv", sep ="") )

rm(scrapeData)
gc()

dbDisconnect(playerDb)
dbDisconnect(siteDb)
dbDisconnect(playerProj)
