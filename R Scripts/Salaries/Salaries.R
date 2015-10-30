###########################
# File: Historical Actual.R
# Description: Scrapes Salaries of Players
# Date: 10/30/2015
# Author: Irving Duran (irving.duran@gmail.com)
# Notes: This script gets all of the salary data for each player and ranks them overall, yearly, and by position
# To do:
#  TODO: For some reason script dies when trying to fetch salaries on Win.  No issues on Linux.
###########################

#Libraries
library("XML")
library("stringr")
library("plyr")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))

#Data
years <- 2013:2023
# list of positions from overthecap.com
position <- c("quarterback", "running-back", "fullback", "wide-receiver", "tight-end", "offensive-line", "left-tackle", "left-guard", "center", "right-guard", "right-tackle", "defensive-line", "3-4-defensive-tackle", "4-3-defensive-tackle", "3-4-defensive-end", "4-3-defensive-end", "linebacker", "3-4-outside-linebacker", "4-3-outside-linebacker", "inside-linebacker", "defensive-back", "cornerback", "safety", "kicker", "punter")
posyr <- data.frame(position=rep(position, length(years)), years)

#Loop to import, process, merge, and save historical actual data
actual <- list()

pb <- txtProgressBar(min = 1, max = nrow(posyr), style = 3)
for(i in 1:nrow(posyr)){
	setTxtProgressBar(pb, i)
	actual[i] <- readHTMLTable(paste("http://overthecap.com/position/", posyr$position, "/", posyr$years, sep="")[i], stringsAsFactors = FALSE) #get players salary
	tryCatch(if(is.null(actual[[i]])!=TRUE) { actual[[i]] <- data.frame(actual[[i]], posyr$position[i], posyr$years[i], stringsAsFactors=FALSE) }, error=function(e) NULL) #attach position and year to player salary
	if(i==nrow(posyr)) { rm(posyr) }
}

actual <- do.call(rbind.data.frame, actual) #combine list into one data frame
names(actual) <- c("name_info", "team", "salary_cap_value", "cash_spent", "pos", "years")

#Clean-up name field
actual$name <- nameMerge(actual$name_info)

#Rename players
ifelse(length(which(actual$name == "CHRIS WELLS")) > 0, actual$name[actual$name == "CHRIS WELLS"] <- "BEANIE WELLS", doNothing <- 0)
ifelse(length(which(actual$name == "CHAD JOHNSON")) > 0, actual$name[actual$name == "CHAD JOHNSON"] <- "CHAD OCHOCINCO", doNothing <- 0)
ifelse(length(which(actual$name == "STEVE JOHNSON")) > 0, actual$name[actual$name == "STEVE JOHNSON"] <- "STEVIE JOHNSON", doNothing <- 0)

#Remove dollar sign and commas
actual$salary_cap_value <- gsub("[$,]", "", actual$salary_cap_value) 
actual$cash_spent <- gsub("[$,]", "", actual$cash_spent)
#Convert variables from character strings to numeric
actual$salary_cap_value <- as.numeric(actual$salary_cap_value)
actual$cash_spent <- as.numeric(actual$cash_spent)

#Calculate overall rank
actual$overallRank <- rank(-actual$salary_cap_value, ties.method="min")
#Calculate rank in the year
#actual <- transform(actual, 
x <- transform(actual, 
		yearRank = ave(salary_cap_value, years,
				FUN = function(x) rank(-x, ties.method = "min")))
x <- x[order(x$years, x$yearRank), ]
actual <- x; rm(x);

# Convert factors to characters
i <- sapply(actual, is.factor)
actual[i] <- lapply(actual[i], as.character)

actual[actual=="quarterback"] <- "QB"
actual[actual=="running-back"] <- "RB"
actual[actual=="wide-receiver"] <- "WR"
actual[actual=="tight-end"] <- "TE"
actual[actual=="kicker"] <- "K"
actual[actual==c("fullback", "offensive-line", "left-tackle", "left-guard", "center", "right-guard", "right-tackle", "defensive-line", "3-4-defensive-tackle", "4-3-defensive-tackle", "3-4-defensive-end", "4-3-defensive-end", "linebacker", "3-4-outside-linebacker", "4-3-outside-linebacker", "inside-linebacker", "defensive-back", "cornerback", "safety", "punter")] <- "Def"

#VORP
qb <- actual[actual$pos=="QB",][order(actual[actual$pos=="QB",]$overallRank),]
rb <- actual[actual$pos=="RB",][order(actual[actual$pos=="RB",]$overallRank),]
wr <- actual[actual$pos=="WR",][order(actual[actual$pos=="WR",]$overallRank),]
te <- actual[actual$pos=="TE",][order(actual[actual$pos=="TE",]$overallRank),]
k <- actual[actual$pos=="K",][order(actual[actual$pos=="K",]$overallRank),]

if(years[i] >= 2003){
	def <- actual[actual$pos=="Def", ][order(actual[actual$pos=="Def", ]$overallRank), ]
}

qb$positionRank <- rank(-qb$salary_cap_value, ties.method="min")
rb$positionRank <- rank(-rb$salary_cap_value, ties.method="min")
wr$positionRank <- rank(-wr$salary_cap_value, ties.method="min")
te$positionRank <- rank(-te$salary_cap_value, ties.method="min")
k$positionRank <- rank(-k$salary_cap_value, ties.method="min")

if(years[i] >= 2003){
	def$positionRank <- rank(-def$salary_cap_value, ties.method="min")
}

#Merge across positions
if(years[i] >= 2003){
	actual <- rbind(qb,rb,wr,te,k,def)
} else{
	actual <- rbind(qb,rb,wr,te,k)
}

#Save data
write.csv(actual, file=paste(getwd(),"/Data/Historical Salaries/actual.csv", sep=""), row.names=FALSE)

#Order variables in data set
actual <- actual[ ,c("name", "pos", "team", "salary_cap_value", "overallRank", "yearRank", "positionRank")]
