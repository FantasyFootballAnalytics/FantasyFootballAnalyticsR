source("R Scripts/Functions/scrapeFunctions.R")
source("R Scripts/Functions/dualPositionData.R")
source("R Scripts/Functions/adjustFieldGoals.R")
source("R Scripts/Functions/updateADP.R")
source("R Scripts/Functions/getRanks.R")
source("R Scripts/Functions/createUrls.R")

require(pbapply)
require(XLConnect)

# Set week and season to desired period. Set weekNo = 0 for season data.
scrapePeriod = c(weekNo = 0, season = 2015)

# Set type of league: standard, ppr or half
leagueType <- "standard"

# Select analysts to scrape for
scrapeAnalysts <- c(  
                      "cbav"  # CBS: CBS Average
                  # , "jaei"  # CBS: Jamey Eisenberg
                  # , "dari"  # CBS: Dave Richard
                    , "yasp"  # Yahoo Sports
                    , "essit" # ESPN
                  #  , "nfla"  # NFL
                  #  , "fosp"  # FOX Sports
                  #  , "ffto"  # FFToday
                  #  , "numf"  # NumberFire
                  #  , "fapro" # FantasyPros
                  # , "acsc"  # Accuscore (requires subsription)
                  # , "bohe"  # Footballguys: Bob Henry (requires subsription)
                  # , "jono"  # Footballguys: John Norton (requires subsription)
                  # , "dado"  # Footballguys: David Dodds (requires subsription)
                  # , "jawo"  # Footballguys: Jason Wood (requires subsription)
                  # , "matr"  # Footballguys: Maurile Tremblay (requires subsription)
                  # , "sibl"  # Footballguys: Sigmund Bloom (requires subsription)
                  # , "mihe"  # Footballguys: Mike Herman (requires subsription)
                  # , "aaru"  # Footballguys: Aaron Rudnicki (requires subsription)
                  # , "dono"  # Footballguys: Dodds-Norton (requires subsription)
                  #  , "edfo"  # EDS Football
                  #  , "fash"  # FantasySharks
                  #  , "ffne"  # FantasyFootballNerd
                    , "wafo"  # WalterFootball
                  # , "4fo4"  # 4for4 (requires subsription)
                  # , "rowi"  # Rotowire (requires subsription)
                  #  , "rtsp"  # RTSports.com
                    )

# Add login for footballguys if you have one
logins <- list(footballguys = c(user = "", pwd = ""))

# Generate the data URLs based on which analysts are selected above. 
dataUrls <- createUrls(weekNo = scrapePeriod[["weekNo"]], 
                       season = scrapePeriod[["season"]],
                       analystIds = scrapeAnalysts)

# Step through each URL to scrape data. Resulting data is a list of tables
scrapeData <- pbapply(dataUrls, 1, function(urow)scrapeUrl(urlData = urow, siteCredentials = logins))

# Determine which URLs contains data for which positions.
posRows <- lapply(playerPositions$positionId, function(p)which(dataUrls$positionId == p))
names(posRows) <- playerPositions$positionCode

# Combining data for each position
posProj <- lapply(posRows, function(posRow)rbindlist(scrapeData[posRow], fill = TRUE))

# Finding data for players with dual positions
dualPosData <- dualPositionData(posProj)

# Adding the dual position data to existing data
if(length(dualPosData) > 0){
  for(p in 1:length(posProj)){
    posProj[[p]] <- rbindlist(list(posProj[[p]], dualPosData[[p]]), fill = TRUE, use.names = TRUE)
  }
}

# Adjusting field goal data for kickers such that all field goal columns are present
if(any(names(posProj) == "K")){
  knames <- names(posProj[["K"]])
  posProj[["K"]] <- merge(posProj[["K"]], adjustFieldGoalStats(kickerData = posProj[["K"]]), by = c("playerId", "analystId"), suffixes = c("_old", ""))[, knames, with = FALSE]
}

# Get ADP and  AAV data
if(scrapePeriod[["weekNo"]] == 0){
  adpData <- updateValues(weekNo = scrapePeriod[["weekNo"]], season = scrapePeriod[["season"]], value = "adp", 
                          type = ifelse(leagueType == "standard", leagueType, "ppr"), 
                          sources =  c("MFL", "ESPN", "FFC", "CBS", "NFL", "Yahoo"))
  aavData <- updateValues(weekNo = scrapePeriod[["weekNo"]], season = scrapePeriod[["season"]], value = "aav", 
                          type = ifelse(leagueType == "standard", leagueType, "ppr"), 
                          sources = c("MFL", "ESPN", "FFC", "CBS", "NFL", "Yahoo"))
  write.csv(adpData, file = paste("Data/statProjections/ADPdata_", scrapePeriod[["season"]], ".csv", sep = ""), row.names = FALSE)
  write.csv(aavData, file = paste("Data/statProjections/AAVdata_", scrapePeriod[["season"]], ".csv", sep = ""), row.names = FALSE)
}


# Get ECR Ranks
expertRank <- getRanks(posName = "consensus", leagueType = ifelse(leagueType == "standard", "std", leagueType), 
                       weekNo = scrapePeriod[["weekNo"]])
for(pos in playerPositions$positionCode){
  expertRank <- rbindlist(list(getRanks(posName = pos,  
                                        leagueType = ifelse(leagueType == "standard", "std", leagueType), 
                                        weekNo = scrapePeriod[["weekNo"]]), expertRank), fill = TRUE)
}
fname <- paste("ECR_", scrapePeriod[["season"]], ifelse(scrapePeriod[["weekNo"]] == 0 , "", 
                                                        paste("wk", scrapePeriod[["weekNo"]], sep = "")), sep = "")
write.csv(expertRank, file = paste("Data/statProjections/", fname, ".csv", sep = ""), row.names = FALSE)
  
# Save data as csv files:
for(pos in names(posProj)){
  fname <- paste(tolower(pos), "projections_", scrapePeriod[["season"]], 
                 ifelse(scrapePeriod[["weekNo"]] == 0 , "", paste("wk", scrapePeriod[["weekNo"]], sep = "")), sep = "")
  write.csv(posProj[[pos]], paste("Data/statProjections/", fname, ".csv", sep = ""), row.names = FALSE)
}