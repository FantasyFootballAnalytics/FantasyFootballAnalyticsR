###########################
# File: Kickers.R
# Description: Kicker rankings
# Date: 6/9/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/League Settings.R", sep=""))

#Risk - "Experts"
kickers <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/k-cheatsheets.php", stringsAsFactors = FALSE)$data

name1 <- str_sub(kickers[,c("Player (team/bye)")], end=str_locate(kickers[,c("Player (team/bye)")], '\\(')[,1]-2)
name2 <- str_sub(kickers[,c("Player (team/bye)")], end=str_locate(kickers[,c("Player (team/bye)")], '\\)')[,1]-1)

name1[is.na(name1)] <- name2[is.na(name1)]

kickers$name <- name1
kickers$team <- str_sub(kickers[,c("Player (team/bye)")], start=str_locate(kickers[,c("Player (team/bye)")], '\\(')[,1]+1, end=str_locate(kickers[,c("Player (team/bye)")], '\\/')[,1]-1)

kickers$rank <- as.numeric(kickers[,"Ave"])
kickers$risk <- as.numeric(kickers[,"Std Dev"])

kickers <- kickers[,c("name","team","rank","risk")]

#View Rankings
kickers

#Save file
save(kickers, file = paste(getwd(),"/Data/kickers-2013.RData", sep=""))
write.csv(kickers, file=paste(getwd(),"/Data/CSV/kickers-2013.csv", sep=""), row.names=FALSE)
