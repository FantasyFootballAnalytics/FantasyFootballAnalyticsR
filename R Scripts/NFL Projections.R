###########################
# File: NFL Projections.R
# Description: Downloads Fantasy Football Projections from NFL.com
# Date: 3/3/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# -These projections are from last year (NFL.com has not yet updated them for the upcoming season)
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")

#Functions
source(paste(getwd(),"/R Scripts/Functions.R", sep=""))

#Download fantasy football projections from NFL.com
qb1_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?position=1&sort=projectedPts&statCategory=projectedStats&statSeason=2012&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
qb2_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?offset=26&position=1&sort=projectedPts&statCategory=projectedStats&statSeason=2012&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
rb1_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?position=2&statCategory=projectedStats&statSeason=2012&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
rb2_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?offset=26&position=2&sort=projectedPts&statCategory=projectedStats&statSeason=2012&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
rb3_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?offset=51&position=2&sort=projectedPts&statCategory=projectedStats&statSeason=2012&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
wr1_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?position=3&sort=projectedPts&statCategory=projectedStats&statSeason=2012&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
wr2_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?offset=26&position=3&sort=projectedPts&statCategory=projectedStats&statSeason=2012&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
wr3_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?offset=51&position=3&sort=projectedPts&statCategory=projectedStats&statSeason=2012&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
te_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?position=4&statCategory=projectedStats&statSeason=2012&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`

#Add variable names for each object
fileList <- c("qb1_nfl","qb2_nfl","rb1_nfl","rb2_nfl","rb3_nfl","wr1_nfl","wr2_nfl","wr3_nfl","te_nfl")

for(i in 1:length(fileList)){
  assign(fileList[i],get(fileList[i])[1:dim(get(fileList[i]))[1],])
  t <- get(fileList[i])
  names(t) <-  c("player_nfl","opp_nfl","gp_nfl","passYds_nfl","passTds_nfl","passInt_nfl","rushYds_nfl","rushTds_nfl","recYds_nfl","recTds_nfl","fumbleTds_nfl","twoPts_nfl","fumbles_nfl","pts_nfl")
  t[t == "-"] <- 0
  assign(fileList[i], t)
}

#Merge players within position
qb_nfl <- rbind(qb1_nfl,qb2_nfl)
rb_nfl <- rbind(rb1_nfl,rb2_nfl,rb3_nfl)
wr_nfl <- rbind(wr1_nfl,wr2_nfl,wr3_nfl)

#Add variable for player position
qb_nfl$pos <- as.factor("QB")
rb_nfl$pos <- as.factor("RB")
wr_nfl$pos <- as.factor("WR")
te_nfl$pos <- as.factor("TE")

#Calculate position rank
qb_nfl$positionRank_nfl <- 1:dim(qb_nfl)[1]
rb_nfl$positionRank_nfl <- 1:dim(rb_nfl)[1]
wr_nfl$positionRank_nfl <- 1:dim(wr_nfl)[1]
te_nfl$positionRank_nfl <- 1:dim(te_nfl)[1]

#Merge players across positions
projections_nfl <- rbind(qb_nfl,rb_nfl,wr_nfl,te_nfl)

#Convert variables from character strings to numeric
projections_nfl$gp_nfl <- as.numeric(projections_nfl$gp_nfl)
projections_nfl$passYds_nfl <- as.numeric(projections_nfl$passYds_nfl)
projections_nfl$passTds_nfl <- as.numeric(projections_nfl$passTds_nfl)
projections_nfl$passInt_nfl <- as.numeric(projections_nfl$passInt_nfl)
projections_nfl$rushYds_nfl <- as.numeric(projections_nfl$rushYds_nfl)
projections_nfl$rushTds_nfl <- as.numeric(projections_nfl$rushTds_nfl)
projections_nfl$recYds_nfl <- as.numeric(projections_nfl$recYds_nfl)
projections_nfl$recTds_nfl <- as.numeric(projections_nfl$recTds_nfl)
projections_nfl$fumbleTds_nfl <- as.numeric(projections_nfl$fumbleTds_nfl)
projections_nfl$twoPts_nfl <- as.numeric(projections_nfl$twoPts_nfl)
projections_nfl$fumbles_nfl <- as.numeric(projections_nfl$fumbles_nfl)
projections_nfl$pts_nfl <- as.numeric(projections_nfl$pts_nfl)

#Player names
qbnames <- str_sub(projections_nfl$player_nfl, end=str_locate(string=projections_nfl$player_nfl, c("QB -"))[,1]-2)
rbnames <- str_sub(projections_nfl$player_nfl, end=str_locate(string=projections_nfl$player_nfl, c("RB -"))[,1]-2)
wrnames <- str_sub(projections_nfl$player_nfl, end=str_locate(string=projections_nfl$player_nfl, c("WR -"))[,1]-2)
tenames <- str_sub(projections_nfl$player_nfl, end=str_locate(string=projections_nfl$player_nfl, c("TE -"))[,1]-2)

projections_nfl$name <- c(na.omit(qbnames),na.omit(rbnames),na.omit(wrnames),na.omit(tenames))

#Player teams
projections_nfl$team_nfl <- str_trim(str_sub(projections_nfl$player_nfl, start=str_locate(string=projections_nfl$player_nfl, c(" - "))[,1]+3, end=str_locate(string=projections_nfl$player_nfl, c(" - "))[,1]+6)) #, end=str_locate(string=projections_nfl$player_nfl, c("-"))[,1]+5

#Remove duplicate cases
projections_nfl[duplicated(projections_nfl$name),]
projections_nfl[which(projections_nfl$name=="Charles Clay"),"pos"] <- "TE"

#Calculate overall rank
projections_nfl$overallRank_nfl <- rank(-projections_nfl$pts_nfl, ties.method="min")

#Order variables in data set
projections_nfl <- projections_nfl[,c("name","pos","team_nfl","positionRank_nfl","overallRank_nfl",
                                        "passYds_nfl","passTds_nfl","passInt_nfl",
                                        "rushYds_nfl","rushTds_nfl","recYds_nfl","recTds_nfl","twoPts_nfl","fumbles_nfl","pts_nfl")]

#Order players by overall rank
projections_nfl <- projections_nfl[order(projections_nfl$overallRank_nfl),]
row.names(projections_nfl) <- 1:dim(projections_nfl)[1]

#Density Plot
ggplot(projections_nfl, aes(x=pts_nfl), fill=pos) + geom_density(fill="green", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of NFL.com Projected Points from 2012")
ggsave(paste(getwd(),"/Figures/NFL projections 2012.jpg", sep=""))

#Save file
save(projections_nfl, file = paste(getwd(),"/Data/NFL-Projections-2012.RData", sep=""))