require(plyr)
require(XML)

getNflPlayers <- function(posData){
  nflPlayers <- data.frame(Pos = as.character(), Num = as.numeric(), PlayerName= as.character(), 
                            Status = as.character(), Team = as.character())
  playerTblCol <- c("Pos", "Num","PlayerName","Status", "Team")
  for (p in 1:posData[["pgs"]])
  { 
    urlQry <- paste("http://www.nfl.com/players/search?category=position&filter=", posData[["posName"]], 
                    "&playerType=current&conference=ALL&d-447263-p=", as.character(p), sep="")
    playerTbl <- tryCatch(readHTMLTable(urlQry,  stringsAsFactors = FALSE, header=FALSE)$result[,c(1:4,13)],
                          error = function(e){
                            print(paste("Error retrieving", posData[["posName"]], "- pg", as.character(p)))
                            return(data.frame())
                          }
    )
    if (length(playerTbl) > 0 )
    {
    names(playerTbl) <- playerTblCol
    pgeLinks <- getHTMLLinks(urlQry)
    playerTbl$playerId <- unique(gsub("[^0-9]","",
                                       pgeLinks[grep("/profile$", pgeLinks)]
    ))
    nflPlayers <- rbind.fill(nflPlayers, playerTbl)
    }
  }

  nameMatrix <- matrix(unlist(strsplit(nflPlayers$PlayerName, ", ", fixed=TRUE)), nrow=nrow(nflPlayers), byrow=TRUE)
  firstName <- nameMatrix[,2]
  lastName <- nameMatrix[,1]
  nflPlayers$PlayerName <- paste(firstName, lastName, sep = " ")
  
  nflPlayers$PlayerName <- getPlayerName(nflPlayers$PlayerName)

  
  nflPlayers$Pos[nflPlayers$Pos == "FB"] <- "RB"
  
  return(nflPlayers)
}


readNflPlayers <- function(){
  
  nflPosition <- list (qb = list(posName = "quarterback", pgs=2),
                       rb = list(posName = "runningback", pgs=4),
                       wr = list(posName = "widereceiver", pgs=6),
                       te = list(posName = "tightend", pgs = 3),
                       pk = list(posName = "kicker", pgs = 1),
                       dl = list(posName = "defensivelineman", pgs= 5),
                       lb = list(posName = "linebacker", pgs=5),
                       db = list(posName = "defensiveback", pgs = 7)
  )
  
  nflPlayers <- lapply(nflPosition, getNflPlayers)
  
  nflAllPlayers <- data.frame(Pos = as.character(), Num = as.numeric(), PlayerName = as.character(), Status = as.character(), Team = as.character(), playerId = as.numeric())
  
  for(plList in nflPlayers){
    nflAllPlayers <- rbind.fill(nflAllPlayers, plList)
  }
  return(nflAllPlayers)
}



