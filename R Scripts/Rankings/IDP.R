###########################
# File: IDP.R
# Description: IDP rankings
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
IDP <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/idp-cheatsheets.php", stringsAsFactors = FALSE)$data

IDP$player <- str_sub(IDP[,c("Player (team/bye)")], end=str_locate(IDP[,c("Player (team/bye)")], '\\(')[,1]-2)
IDP$name <- nameMerge(IDP$player)
IDP$team <- str_sub(IDP[,c("Player (team/bye)")], start=str_locate(IDP[,c("Player (team/bye)")], '\\(')[,1]+1, end=str_locate(IDP[,c("Player (team/bye)")], '\\/')[,1]-1)

IDP$pos <- IDP$Pos
IDP$pos <- gsub("\\d", "", IDP$pos)

IDP$rank <- as.numeric(IDP[,"Ave"])
IDP$risk <- as.numeric(IDP[,"Std Dev"])

IDP <- IDP[,c("name","player","pos","team","rank","risk")]

IDP <- IDP[order(IDP$rank),]

#View Rankings
IDP

#Save file
save(IDP, file = paste(getwd(),"/Data/IDP.RData", sep=""))
write.csv(IDP, file=paste(getwd(),"/Data/IDP.csv", sep=""), row.names=FALSE)

save(IDP, file = paste(getwd(),"/Data/Historical Rankings/IDP-2014.RData", sep=""))
write.csv(IDP, file=paste(getwd(),"/Data/Historical Rankings/IDP-2014.csv", sep=""), row.names=FALSE)

