require(XML)
require(plyr)
require(RMySQL)

lastInit <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

yahPlayer <- data.frame(Player = as.character(), playerId = as.numeric(), Pos = as.character(), Team = as.character())
for(ltr in lastInit){
  inpUrl <- paste("http://sports.yahoo.com/nfl/players?type=lastname&query=", ltr, sep ="")
  plyrTbl <- readHTMLTable(inpUrl, skip.rows =1 , stringsAsFactors = FALSE)[[9]]
  if(length(plyrTbl) > 1 ){
    names(plyrTbl) <- c("Player","Pos", "Team")
    plyrTbl$Player <- getPlayerName(plyrTbl$Player)
    
    pgeLinks <- getHTMLLinks(inpUrl)
    plyrTbl$playerId <- as.numeric(unique(gsub("[^0-9]","",
                                               pgeLinks[grep("/nfl/players/[0-9]{1,9}", pgeLinks)] )))
    yahPlayer <- rbind.fill(yahPlayer, plyrTbl[plyrTbl$Pos %in% c("QB", "RB","WR", "TE", "K", "DE",  "DT",  "NT", "LB", "S", "CB"), ])
  }
}

playerDb <- connectDb("ffplayerinfo")

players <- dbReadTable(playerDb, "players")
players <- players[is.na(players$yahooId), ]

teams <- dbReadTable(playerDb, "nflteams")
teams$fullName <- paste(teams$teamArea, teams$teamName)
teams <- teams[, c("teamCode", "fullName")]

yahPlayer <- merge(x=yahPlayer, y= teams, by.x = "Team", by.y = "fullName", all.x = TRUE)
yahPlayer$Team <- NULL
yahPlayer <- yahPlayer[yahPlayer$Player %in% players$player, c("Player", "playerId", "teamCode")]

if(nrow(yahPlayer) >0 ){
  print(paste("Updating", nrow(yahPlayer), "players"))
  valueList <- paste("('", yahPlayer$Player, "', ", yahPlayer$playerId, ", '", yahPlayer$teamCode, "')", sep="")
  
  res <- dbSendQuery(playerDb, "create table yahooidupd (Player varchar(255), playerId int(11), teamCode varchar(10)) ENGINE=InnoDB DEFAULT CHARSET=utf8;")
  dbClearResult(res)
  
  res <- dbSendQuery(playerDb, paste("insert into yahooidupd values ", paste(valueList, collapse =", "), ";", sep = ""))
  dbClearResult(res)
  res <- dbSendQuery(playerDb, "call updateyahooid();")
  dbClearResult(res)
  dbRemoveTable(playerDb, "yahooidupd")
  
}

dbDisconnect(playerDb)