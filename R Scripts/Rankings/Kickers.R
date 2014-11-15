###########################
# File: Kickers.R
# Description: Kicker rankings
# Date: 6/9/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Risk - "Experts"
kickers <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/k-cheatsheets.php", stringsAsFactors = FALSE)$data

name1 <- str_sub(kickers[,c("Player (team/bye)")], end=str_locate(kickers[,c("Player (team/bye)")], '\\(')[,1]-2)
name2 <- str_sub(kickers[,c("Player (team/bye)")], end=str_locate(kickers[,c("Player (team/bye)")], '\\)')[,1]-1)

name1[is.na(name1)] <- name2[is.na(name1)]

kickers$player <- name1
kickers$name <- nameMerge(kickers$player)
kickers$team <- str_sub(kickers[,c("Player (team/bye)")], start=str_locate(kickers[,c("Player (team/bye)")], '\\(')[,1]+1, end=str_locate(kickers[,c("Player (team/bye)")], '\\/')[,1]-1)

kickers$rank <- as.numeric(kickers[,"Ave"])
kickers$risk <- as.numeric(kickers[,"Std Dev"])

#Subset columns
kickers <- kickers[,c("name","player","team","rank","risk")]

#Remove rows with all NAs
kickers <- kickers[rowSums(is.na(kickers)) != ncol(kickers),]

#Sort by rank
kickers <- kickers[order(kickers$rank),]

#View Rankings
kickers

#Save file
save(kickers, file = paste(getwd(),"/Data/kickers.RData", sep=""))
write.csv(kickers, file=paste(getwd(),"/Data/kickers.csv", sep=""), row.names=FALSE)

save(kickers, file = paste(getwd(),"/Data/Historical Rankings/kickers-2015.RData", sep=""))
write.csv(kickers, file=paste(getwd(),"/Data/Historical Rankings/kickers-2015.csv", sep=""), row.names=FALSE)
