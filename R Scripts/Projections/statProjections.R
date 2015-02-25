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
siteTable <- subset(siteTable, projDataSiteId != 11)
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
scrapeData <- apply(siteUrls, 1, scrapeUrl)

# Create a list of row numbers from the site URL table that represents each position
posRows <- lapply(posList, function(p)which(siteUrls$posId == p))
rm(siteUrls)
# Combine all of the projections for each position
posProj <- lapply(posRows, function(r)rbindlist(scrapeData[r], fill = TRUE))

rm(scrapeData)
gc()
