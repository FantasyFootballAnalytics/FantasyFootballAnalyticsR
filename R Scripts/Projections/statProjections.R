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

# Calculate average stats
avgProj <- lapply(posList, function(pId){
  setkeyv(posProj[[pId]], "playerId")
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "player", "analystId", "name"))
  proj <- posProj[[pId]][, lapply(.SD, function(col)mean(col, na.rm = TRUE)), by = key(posProj[[pId]]), .SDcols = dataCols]
  proj[ , analystId := 18]
})


# Calculate robust average from Hodges-Lehman location estimate
medProj <- lapply(posList, function(pId){
  setkeyv(posProj[[pId]], "playerId")
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "player", "analystId", "name"))
  proj <- posProj[[pId]][, lapply(.SD, function(col)wilcox.loc(as.numeric(col))), by = key(posProj[[pId]]), .SDcols = dataCols]
  proj[ , analystId := 17]
})

aggProj <- lapply(posList, function(p)rbindlist(list(medProj[[p]], avgProj[[p]])))

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
  aggProj[[pId]][, points := sum(.SD * multiply, na.rm = TRUE) + ifelse(pId == 6, dstPts(dstPtsAllowed, scoringRules$ptsBracket), 0), .SDcols = scoringCols, by = c("playerId", "analystId")]
}

save(aggProj, posProj, file = paste(getwd(), "/Data/consolidatedProj.RData", sep =""))

rm(scrapeData)
gc()
