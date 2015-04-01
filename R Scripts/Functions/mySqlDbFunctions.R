##############################################################
## File: mySqlDbFunctions.R
## Description: Functions for interaction with MySQL database
## Date: 3/29/2015
## Author: Dennis Andersen (andersen.dennis@live.com)
## -----------------------------------------------------------
## Notes:
## Functions in this script helps with handling reading and 
## writing to a MySQL database. The access credentials is read
## from a text file that contains three columns:
## database name, host name, user name and password.
##############################################################
require(RMySQL)
# Set the path and filename for the credentials file here:
CREDENTIAL_FILE = paste(getwd(), "/Config/mysqlaccess.txt", sep = "")

## This functions reads the credential file and find the database
## requested by db and connects
connectDb <- function(db){
  if(!file.exists(CREDENTIAL_FILE)){
    stop("Credentials file do not exists")
  }
  
  mySqlCred <- read.table(file = CREDENTIAL_FILE, 
                          col.names = c("db_name", "host_name", "user_name", "pwd"), sep =",", 
                          stringsAsFactors = FALSE)
  if(!any(mySqlCred$db_name == db)){
    stop("Specified database not found in credentials")
  }
  
  mySqlCred <- mySqlCred[mySqlCred$db_name == db, ]
  conn <- tryCatch(dbConnect(RMySQL::MySQL(), dbname = db, host = mySqlCred$host_name, 
                    username = mySqlCred$user_name, password = mySqlCred$pwd),
                   error = function(e){
                     print(paste("Error connecting to", db, "at", mySqlCred$host_name))
                     print(e)
                   })
  rm(mySqlCred)
  return(conn)
}

## Function to look up scrapeId based on week and season values
lookupScrapeId <- function(weekNo, season){
  if(!dbIsValid(playerProj)){
    playerProj <- connectDb("playerprojections")  
  }
  id <- dbGetQuery(playerProj, paste("SELECT dataScrapeId from datascrapes where weekNo =", weekNo, " and seasonYear=", season, ";"))$dataScrapeId
  return(id)
}

## Function to return the scrapeId based on week and season values
getScrapeId <- function(weekNo, season){
  if(!dbIsValid(playerProj)){
    playerProj <- connectDb("playerprojections")  
  }
  scrapeId <- lookupScrapeId(weekNo, season)
  if(!(scrapeId > 0)){
    res <- dbSendQuery(playerProj, paste("Insert into dataScrapes (weekNo, seasonYear, scrapeDate) values (", weekNo, ",", season, ",'", Sys.time(), "')"))
    dbClearResult(res)
    scrapeId <- lookupScrapeId(weekNo, season)
  }
  return(scrapeId)
}

## Function to write projections data to the database
writeProj2DB <- function(scrapeId, projData, projAnalysts = vector(), dataType = "projections"){
  for(nm in names(projData)){
    data <- data.frame(projData[[nm]])
    tblName <- paste(tolower(nm), tolower(dataType), sep = "")
    if(dbExistsTable(playerProj, tblName)){
      if(!dbIsValid(playerProj)){
        playerProj <- connectDb("playerprojections")
      }
      
      data$dataScrapeId <- scrapeId
      colNames <- as.character(dbColumnInfo(playerProj, tblName)$name)
      colNames <- colNames[colNames != "projectionId"]
      
      colNames <- intersect(colNames, names(data))
      
      data <- data[, colNames]
      valueList <- apply(data,1, function(x)paste("(", paste(as.numeric(x), collapse = ","), ")", sep = ""))
      valueList <- gsub("NA", "NULL", valueList, fixed = TRUE)
      valueList <- gsub("NaN", "NULL", valueList, fixed = TRUE)
      
      if(dataType == "projections"){
        delString <- paste("delete from ", tblName, " where projectionId > 0 and dataScrapeId = ", scrapeId, 
                           " and analystId in (", paste(projAnalysts, collapse = ","), ");", sep ="")
      }
      else {
        delString <- paste("delete from ", tblName, " where projectionId > 0 and dataScrapeId = ", scrapeId, ";", sep ="")
      }
      
      res <- dbSendQuery(playerProj, delString)
      dbClearResult(res)
      qryString <- paste("Insert into", tblName, "(", paste(colNames, collapse =", "), ") values",
                         paste(valueList, collapse = ",") ,";")
      res <- dbSendQuery(playerProj, qryString)
      dbClearResult(res)
    }
  }
}

## Function to read projection data from the database
readProjFromDB <- function(scrapeId, positionList){
  lapply(names(positionList), function(pnme){
    tblName <- paste(tolower(pnme), "projections", sep = "")
    if(dbExistsTable(playerProj, tblName)){
      if(!dbIsValid(playerProj)){
        playerProj <- connectDb("playerprojections")
      }
      colNames <- as.character(dbColumnInfo(playerProj, tblName)$name)
      colNames <- colNames[colNames != "projectionId"]
      
      data <- dbGetQuery(playerProj, paste("select ", paste(colNames, collapse = ","), " from ", tblName, 
                                           " where dataScrapeId = ", scrapeId ,";", sep = ""))
      return(data.table(data))
    }
    else {return(data.table())}
  })
}