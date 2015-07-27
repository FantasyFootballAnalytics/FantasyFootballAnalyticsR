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

### EDIT THIS LIST TO SET WHICH ANALYSTS TO SCRAPE DATA FOR : 
projectionAnalysts <- c(
  ## CBS:
  "Jamey Eisenberg", "Dave Richard",  
  ## Footballguys:
  #"Dodds-Norton", "Dodds", "Tremblay", "Herman", "Henry", "Wood", "Bloom",               
  ## Others:
  #"Yahoo Sports", 
  "ESPN", "NFL", "FOX Sports",  "FFToday", "FFToday - IDP", "NumberFire",
  "FantasyPros", 
  #"EDS Football", "FantasySharks", 
  "Fantasyfootballnerd" #, "WalterFootball"      
)


#### SET WEEK AND SEASON
weekNo <- 17
season <- 2014
siteCredentials <- list(footballguys = c(user = "", pwd = "")) 

#Functions
source(paste(getwd(),"/R Scripts/Functions/packageFunctions.R", sep=""))
analystIds <- selectAnalysts(projectionAnalysts)

calcWeighted <- FALSE
if(diff(range(sourceWeights)) > sqrt(.Machine$double.eps)){
  for(r in 1:nrow(analysts)){
    calcWeighted <- TRUE
    analysts$weight[r] <- sourceWeights[analysts$analystName]
  }
}


getProjections <- function(weekNo, season, projectionAnalysts, siteCredentials = list()){
  if(!exists("playerDb")){
    playerDb <- connectDb("ffplayerinfo")
  }
  if(!dbIsValid(playerDb)){
    playerDb <- connectDb("ffplayerinfo")
  }
  
  positions <- dbGetQuery(playerDb, "SELECT positionId, positionCode from playerpositions;")
  dbDisconnect(playerDb)
  
  
  analystIds <- selectAnalysts(projectionAnalysts)
  
  # Generate table of scrape sites
  urlTable <- createUrls(weekNo, season, analystIds)
  
  posList <- as.list(positions$positionId[positions$positionId %in% unique(urlTable$positionId)])
  names(posList) <- positions$positionCode[positions$positionId %in% unique(urlTable$positionId)]
  
  # Scrape the data
  scrapeData <- pbapply(urlTable, 1, scrapeUrl, siteCredentials)
  
  # Create a list of row numbers from the site URL table that represents each position
  posRows <- lapply(posList, function(p)which(urlTable$positionId == p))
  
  # Combine all of the projections for each position
  posProj <- lapply(posRows, function(r)rbindlist(scrapeData[r], fill = TRUE))
  
  ## Combining data for dual position players
  offpos <- intersect(names(posProj), c("QB", "RB", "WR", "TE"))
  defpos <- intersect(names(posProj), c("DL", "DB", "LB"))
  
  if(length(defpos) > 0 & length(offpos) > 0){
    posComb <- cbind(combn(offpos, 2), combn(defpos, 2))
  }
  
  if(length(defpos) == 0 & length(offpos) > 0){
    posComb <- combn(offpos, 2)
  }
  
  if(length(defpos) > 0 & length(offpos) == 0){
    posComb <- combn(offpos, 2)
  }
  
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
  
  # Adding up Field goals by distance for the kickers who only have that stat projection
  posProj[["K"]][is.na(fg) , 
                 fg:= ifelse(is.na(fg0019), 0, fg0019) + ifelse(is.na(fg2029), 0, fg2029) + 
                   ifelse(is.na(fg3039), 0 , fg3039) + ifelse(is.na(fg4049), 0, fg4049) + ifelse(is.na(fg50), 0, fg50)]
  
  # Connecting to projections database and writing the projections to the database
  scrapeId <- getScrapeId(weekNo , season)
  writeProj2DB(scrapeId, posProj, selectAnalysts(projectionAnalysts))
  
  return(posProj)
  
}  
  

getCalculations <- function(weekNo, season, positions, analysts, calcType = "robust", scoringRules){
  scrapeId <- getScrapeId(weekNo , season)
  # Reading ALL projections from the database as a basis for calculations
  posProj <- readProjFromDB(scrapeId, positions, analysts)
  
  if(!exists("siteDb")){
    siteDb <- connectDb("projectionsites")
  }
  
  if(!dbIsValid(siteDb)){
    siteDb <- connectDb("projectionsites")
  }
  
  # Adding analysts weights
  analystWeights <- dbGetQuery(siteDb, paste("SELECT analystId, weight from analysts where analystId in (", paste(analysts, collapse = ","), ")"))
  dbDisconnect(siteDb)
  
  for(p in 1:length(posProj)){
    if(exists("analystId", posProj[[p]])){
      posProj[[p]] <- merge(x=posProj[[p]], y=analystWeights, by = "analystId")
    }
  }  
  
  # Aggretating projections:
  aggregate <- lapply(names(posProj), function(pId){
    if(nrow(posProj[[pId]])== 0){
      return(data.table())
    }
    setkeyv(posProj[[pId]], "playerId")
    dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "analystId", "dataScrapeId", "weight"))
    proj <- switch(calcType,
                   "average" = posProj[[pId]][, lapply(.SD, function(col)mean(col, na.rm = TRUE)), by = key(posProj[[pId]]), .SDcols = dataCols],
                   "robust" = posProj[[pId]][, lapply(.SD, function(col)wilcox.loc(as.numeric(col))), by = key(posProj[[pId]]), .SDcols = dataCols],
                   "weighted" = posProj[[pId]][, lapply(.SD, function(col, weight)weighted.mean(as.numeric(col), weight, na.rm = TRUE)), by = key(posProj[[pId]]), .SDcols = dataCols]
    )
    
    if(pId == "K"){# Splitting out the Field goals made estimate on field goals per distance
      proj[, totFG := (fg0019+fg2029+fg3039+fg4049+fg50)]
      proj[, c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019*fg/totFG, fg2029*fg/totFG, fg3039*fg/totFG, fg4049*fg/totFG, fg50*fg/totFG)]
      proj[, totFG:=NULL]
    }
    return(proj)
  })
  names(aggregate) <- names(posProj)
  
  ## Updating kicker projcetions to distribute the total field goals out by distance based on average projections
  if(any(names(posProj) == "K")){
    posProj[["K"]] <- merge( posProj[["K"]], aggregate[["K"]][ , c("playerId", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"), with = FALSE], 
                             by = "playerId", suffixes = c("", "_avg"))
    posProj[["K"]][, tot_avg := fg0019_avg + fg2029_avg + fg3039_avg + fg4049_avg+ fg50_avg]
    
    posProj[["K"]][fg>0  & is.na(fg0019) & is.na(fg2029) & is.na(fg3039) & is.na(fg4049) & is.na(fg50) ,
                   c("fg0019", "fg2029", "fg3039", "fg4049", "fg50") := list(fg0019_avg*fg/tot_avg, fg2029_avg*fg/tot_avg, fg3039_avg*fg/tot_avg, fg4049_avg*fg/tot_avg, fg50_avg*fg/tot_avg) ]
    posProj[["K"]][, c("fg0019_avg", "fg2029_avg", "fg3039_avg", "fg4049_avg", "fg50_avg", "tot_avg") := list(NULL, NULL, NULL, NULL, NULL, NULL)]
    for(colNo in 1:length(posProj[["K"]])){
      set(posProj[["K"]], which(is.na(posProj[["K"]][[colNo]])), colNo, 0)
    }
  }
  
  # For analysts with missing variables set the missing variable equal to the average projection
  for(pId in names(posProj)){
    if(nrow(posProj[[pId]]) > 0 ){
      
      dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "analystId"))
      for(analyst in unique(posProj[[pId]][, analystId])){
        missingVars <- names(posProj[[pId]])[posProj[[pId]][analystId == as.numeric(analyst), apply(.SD, 2, function(x)all(is.na(x)))]]
        missingVars <- intersect(missingVars, dataCols)
        
        playerList <- posProj[[pId]][analystId == analyst, playerId]
        posProj[[pId]][analystId == analyst, 
                       (missingVars) := aggregate[[pId]][playerId %in%  playerList, missingVars, with = FALSE]]
      }
    }
  }
  
  # Scoring calculations
  projPoints <- lapply(names(posProj), function(pId){
    calcPts <- data.table()
    if(nrow(posProj[[pId]]) > 0){    
      setkeyv(posProj[[pId]], "playerId")
      scoringCols <- intersect(as.character(scoringRules[[pId]]$dataCol), names(posProj[[pId]]))
      multiply <- scoringRules[[pId]]$multiplier[scoringRules[[pId]]$dataCol %in% scoringCols]
      if(pId == "DST" & !any(scoringCols == "dstPtsAllow")){
        scoringCols <- c(scoringCols, "dstPtsAllow")
        multiply <- c(multiply, 0)
      }
      posProj[[pId]][, points := sum(.SD * multiply, na.rm = TRUE) + ifelse(pId == "DST", dstPts(dstPtsAllow, scoringRules$ptsBracket), 0), .SDcols = scoringCols, by = c("playerId", "analystId")]
      posProj[[pId]][, sdPts := switch(calcType,
                                       "robust" = mad(points, na.rm = TRUE),
                                       "average" = sd(points, na.rm = TRUE),
                                       "weighted" = weighted.sd(points, weight))
                     , by = key(posProj[[pId]])]
      posProj[[pId]][, c("points.Lo", "points.Hi"):= switch(calcType, 
                                                            "robust" = tryCatch(wilcox.sign(points)$conf.int, 
                                                                                error = function(e)list(mean(points, na.rm = TRUE) - sd(points, na.rm = TRUE), 
                                                                                                        mean(points, na.rm = TRUE) + sd(points, na.rm = TRUE))),
                                                            "average" = list(mean(points, na.rm = TRUE) - 1.96 * sd(points, na.rm = TRUE) / sqrt(.N),
                                                                             mean(points, na.rm = TRUE) + 1.96 * sd(points, na.rm = TRUE) / sqrt(.N)),
                                                            "weighted" = list(weighted.mean(points, weight, na.rm = TRUE) - 1.96 * sdPts / sqrt(.N),
                                                                              weighted.mean(points, weight, na.rm = TRUE) + 1.96 * sdPts /sqrt(.N))),
                     by = key(posProj[[pId]])]
      posProj[[pId]][, c("points.Lo", "points.Hi"):= list(min(points.Lo), max(points.Hi)) , by = key(posProj[[pId]])]
      
      aggregate[[pId]][, points := sum(.SD * multiply, na.rm = TRUE) + ifelse(pId == "DST", dstPts(dstPtsAllow, scoringRules$ptsBracket), 0), .SDcols = scoringCols, by = c("playerId")]
      
      if(nrow(aggregate[[pId]]) >0 & nrow(posProj[[pId]])>0){
        calcPts <- merge(
          aggregate[[pId]][, c("playerId", "points"), with = FALSE],
          unique(posProj[[pId]][, c("playerId",  "points.Lo", "points.Hi", "sdPts"), with = FALSE]), 
          by = "playerId"
        )
      }
      
      posProj[[pId]][ , c("points.Lo", "points.Hi", "sdPts") := c(NULL, NULL, NULL)]
    }
    return(calcPts)
  } 
  )
  names(projPoints) <- names(posProj)
  return(list(statprojections = posProj, aggregates = aggregate, points = projPoints))
}
  

# Reading ALL projections from the database as a basis for calculations
posProj <- readProjFromDB(scrapeId, posList)

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
