###########################
# File: createUrls.R
# Description: Function to generate a table of URLs to scrape data from
# Date: 2/13/2016
# Author: Dennis Andersen (andersen.dennis@live.com)
# Notes:
# 
# To do:
###########################
createUrls <- function(weekNo = 0, season, analystIds = NULL, urls = allUrls,
                       periodParameters = c("{$WeekNo}", "{$Season}", "{$Segment}"),
                       pageParameter = "{$PgeID}",
                       otherParameters = c("{$YahooLeague}", "{$FFNKEY}"),
                       otherValues = c(yahooLeague, ffnAPIKey),
                       pageVars = c("startPage", "endPage", "stepPage"), urlVar = "siteUrl", whichTblVar = "whichTable", posIdVar = "positionId",
                       returnVars = c("siteTableId", "analystId", "positionId", "urlData", "urlPeriod", "whichTable", "playerLinkString"),
                       sharkSegment = c("2015" = 522, "2016" = 554)){
  
  # Function to replace paramaters within the urls provided in each row of the allUrls table.
  urlList <- function(siteRow, weekNo, season, periodParameters, pageParameter, otherParameters, otherValues, pageVars, 
                      urlVar, whichTblVar, posIdVar, returnVars, sharkSegment){
    if(length(periodParameters) != 3){
      stop("periodParameter should have exactly 3 elements", call. = FALSE)
    }
    
    if(length(pageParameter) != 1){
      stop("pageParameter should have exactly 1 element", call. = FALSE)
    }
    
    retValue = data.table()  
    
    # Determine FantaysShark segment for the period provided
    segment <- 0
    if(as.character(season) %in% names(sharkSegment))
      segment <- ifelse(weekNo == 0, sharkSegment[[as.character(season)]], sharkSegment[[as.character(season)]] + weekNo + 9)
    
    # Defining replacement parameters and values for period parameters
    replPar <- c(periodParameters, otherParameters, pageParameter)
    periodValues <- c(weekNo, season, segment)
    
    # Generating one url for each page defined in the allUrls table row
    for(pg in seq(from= as.numeric(siteRow[pageVars[1]]), to= as.numeric(siteRow[pageVars[2]]), by = as.numeric(siteRow[pageVars[3]]))){
      tmpUrl <- siteRow[urlVar]
      
      # Defining replacement values
      replVal <- c(periodValues, otherValues, pg)
      
      # Fantasy Football Today has a different way of designating week numbers for post season
      if(length(grep("fftoday", tolower(tmpUrl[[1]]))) > 0)
        replVal[1] <- 20 + (as.numeric(weekNo) - 17)
      
      
      for(i in 1:length(replPar)){
        tmpUrl <- gsub(replPar[i], as.character(replVal[i]), tmpUrl, fixed = TRUE)
      }
      
      # For Walterfootball we need to assign the position Ids to the variable designating the poistion
      # to allow to get the right sheet from .xls.
      if(length(grep("walterfootball", tolower(tmpUrl[[1]])))>0){
        siteRow[whichTblVar] <- siteRow[posIdVar]
      }
      
      retData <- as.data.frame(t(siteRow))
      retValue <- rbind.fill(retValue, data.table(retData[, returnVars] , siteUrl = tmpUrl))
    }
    return(retValue)
  }
  
  # Variable to use for period selection
  periodSelect <- ifelse(weekNo == 0, "season", "week")
  
  # Selecting urls for specified period
  urlTable <- urls[urlPeriod == periodSelect]
  
  # If analysts are specified just select those
  if(length(analystIds) > 0){
    urlTable <- urlTable[analystId %in% analystIds]
  }
  
  # Step through the urls to replace parameters.
  siteUrls <- rbindlist(apply(urlTable,1, urlList, weekNo, season, periodParameters, pageParameter, otherParameters, otherValues, 
                              pageVars, urlVar, whichTblVar, posIdVar, returnVars, sharkSegment))
  return(siteUrls)
}