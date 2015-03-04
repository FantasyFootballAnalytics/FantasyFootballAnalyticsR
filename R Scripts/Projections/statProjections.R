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

# Set week number and Season. If seasonal projections set weekNo = 0
weekNo <- 0
season <- 2014

# Helper variables
is.season <- (weekNo == 0)
periodSelect <- ifelse(is.season, "Season", "Week")
posList <- list(qb = 1, rb = 2, wr = 3, te = 4, k = 5, dst = 6)

# Generate table of scrape sites
siteTable <- merge(projAnalysts, projSites, by = "projDataSiteId")
siteTable <- merge(siteTable, sitePositions, by = "projDataSiteId", allow.cartesian = TRUE)
siteTable <- subset(siteTable, apply(siteTable, 1, function(x)(is.na(x["limitPos"])| (length(grep(x["sitePosName"], x["limitPos"]))>0))))
siteTable <- subset(siteTable, projDataSiteId < 7)

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

# Calculate average stats and points
avgProj <- lapply(posList, function(pId){
  setkeyv(posProj[[pId]], "playerId")
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "projAnalystId", "nobs"))
  proj <- posProj[[pId]][, lapply(.SD, function(col)mean(col, na.rm = TRUE)), by = key(posProj[[pId]]), .SDcols = dataCols]
  proj[ , projAnalystId := 18]
})

hle <- function(vec){
  n <- length(vec)
  wAvg <- rep(0, n * (n + 1) / 2)
  pair <- 1
  for (i in 1:n){
    for (j in i:n){
      wAvg[pair] <- mean(c(vec[i], vec[j]), na.rm = TRUE)
      pair <- pair + 1
    }
  }
  return(median(wAvg, na.rm = TRUE))
}

# Calculate robust average from Hodges-Lehman and points
medProj <- lapply(posList, function(pId){
  setkeyv(posProj[[pId]], "playerId")
  dataCols <- setdiff(names(posProj[[pId]]), c("playerId", "projAnalystId", "nobs"))
  proj <- posProj[[pId]][, lapply(.SD, function(col)hle(col)), by = key(posProj[[pId]]), .SDcols = dataCols]
  proj[ , projAnalystId := 17]
})

aggProj <- lapply(posList, function(p)rbindlist(list(medProj[[p]], avgProj[[p]])))

save(aggProj, paste(getwd(), "/Data/aggProj.RData"))
rm(scrapeData)
gc()
