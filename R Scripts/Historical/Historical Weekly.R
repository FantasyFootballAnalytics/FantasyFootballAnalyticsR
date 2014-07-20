###########################
# File: Historical Weekly.R
# Description: Scrapes Historical Weekly Performance of Players
# Date: 7/19/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
#http://www.spxn.com/Stats.asp?SportID=4
#http://fftoday.com/stats/playerstats.php?Season=2013&GameWeek=1&PosID=10
###########################

#Libraries
library("XML")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))

#Specify year
years <- 2011:2013 #1960:2013

#Scrape data
qb <- list()
rb <- list()
rb1 <- list()
rb2 <- list()
rb3 <- list()
wr <- list()
wr1 <- list()
wr2 <- list()
wr3 <- list()
wr4 <- list()
wr5 <- list()
wr6 <- list()

pb <- txtProgressBar(min = 1, max = 16, style = 3)
for(i in 1:16){
  setTxtProgressBar(pb, i)
  qb[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=R&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=pass_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pass_att", sep=""), stringsAsFactors = FALSE)$stats
  rb1[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=R&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=rush_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rush_yds", sep=""), stringsAsFactors = FALSE)$stats
  rb2[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rush_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rush_yds&order_by_asc=&offset=100", sep=""), stringsAsFactors = FALSE)$stats
  rb3[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rush_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rush_yds&order_by_asc=&offset=200", sep=""), stringsAsFactors = FALSE)$stats
  wr1[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=R&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds", sep=""), stringsAsFactors = FALSE)$stats
  wr2[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=100", sep=""), stringsAsFactors = FALSE)$stats
  wr3[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=200", sep=""), stringsAsFactors = FALSE)$stats
  wr4[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=300", sep=""), stringsAsFactors = FALSE)$stats
  wr5[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=400", sep=""), stringsAsFactors = FALSE)$stats
  wr6[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=500", sep=""), stringsAsFactors = FALSE)$stats
}

qbDF <- do.call(rbind, qb)
rb1DF <- do.call(rbind, rb1)
rb2DF <- do.call(rbind, rb2)
rb3DF <- do.call(rbind, rb3)
wr1DF <- do.call(rbind, wr1)
wr2DF <- do.call(rbind, wr2)
wr3DF <- do.call(rbind, wr3)
wr4DF <- do.call(rbind, wr4)
wr5DF <- do.call(rbind, wr5)
wr6DF <- do.call(rbind, wr6)

rbDF <- rbind(rb1DF, rb2DF, rb3DF)
wrDF <- rbind(wr1DF, wr2DF, wr3DF, wr4DF, wr5DF, wr6DF)

#Variable names
names(qbDF) <- c("rank","player","age","date","league","team","blank","opponent","result","game","week","day","passComp","passAtt","passCompPct","passYds","passTds","passInt","passRating","passYdsPerAtt","passYdsPerAttAdj")
names(rbDF) <- c("rank","player","age","date","league","team","blank","opponent","result","game","week","day","rushAtt","rushYds","rushYdsPerAtt","rushTds")
names(wrDF) <- c("rank","player","age","date","league","team","blank","opponent","result","game","week","day","rec","recYds","ydsPerRec","recTds")

#Player name
qbDF$name <- nameMerge(qbDF$player)
rbDF$name <- nameMerge(rbDF$player)
wrDF$name <- nameMerge(wrDF$player)

#Year
qbDF$year <- str_trim(sapply(str_split(qbDF$date, "\\-"), "[", 1))
rbDF$year <- str_trim(sapply(str_split(rbDF$date, "\\-"), "[", 1))
wrDF$year <- str_trim(sapply(str_split(wrDF$date, "\\-"), "[", 1))

#Pass Completion %
qbDF$passCompPct <- str_trim(sapply(str_split(qbDF$passCompPct, "\\%"), "[", 1))

#Merge
weeklyData <- rbind.fill(qbDF, rbDF, wrDF)

#weeklyData <- merge_recurse(listProjections, by=c("name","year","week","team"))

#Cleanup data frames
weeklyData <- weeklyData[-which(weeklyData$rank == "Rk"),]

#qbDF <- qbDF[-which(qbDF$rank == "Rk"),]
#rbDF <- rbDF[-which(rbDF$rank == "Rk"),]
#wrDF <- wrDF[-which(wrDF$rank == "Rk"),]

#Convert to numeric
weeklyData[,c("rank","game","week","passComp","passAtt","passCompPct","passYds","passTds","passInt","passRating","passYdsPerAtt","passYdsPerAttAdj","year","rushAtt","rushYds","rushYdsPerAtt","rushTds","rec","recYds","ydsPerRec","recTds")] <- convert.magic(weeklyData[,c("rank","game","week","passComp","passAtt","passCompPct","passYds","passTds","passInt","passRating","passYdsPerAtt","passYdsPerAttAdj","year","rushAtt","rushYds","rushYdsPerAtt","rushTds","rec","recYds","ydsPerRec","recTds")], "numeric")

#Calculate average across same player/year/week combinations for duplicate cases
weeklyData2 <- ddply(weeklyData, .(name, year, week), numcolwise(mean), na.rm=TRUE)



#Subset Data
weeklyData3 <- weeklyData2[,c("name","year","week","passYds","passTds","passInt","rushYds","rushTds","rec","recYds","recTds")]

#Order data
weeklyDataFinal <- weeklyData3[order(weeklyData3$year, weeklyData3$week, weeklyData3$name),]

#Long to Wide
weeklyDataWide <- reshape(weeklyDataFinal,
                          timevar = c("week"),
                          idvar = c("name","year"),
                          direction = "wide",
                          sep="")

#Order data
weeklyDataWide <- weeklyDataWide[order(weeklyDataWide$name, weeklyDataWide$year),]

#Replace NAs with 0
weeklyDataWide[is.na(weeklyDataWide)] <- 0

#Calculate SD across same player/year combinations
sdPassYds <- apply(weeklyDataWide[,grep("passYds", names(weeklyDataWide))], 1, sd)
sdPassTds <- apply(weeklyDataWide[,grep("passTds", names(weeklyDataWide))], 1, sd)
sdPassInt <- apply(weeklyDataWide[,grep("passInt", names(weeklyDataWide))], 1, sd)
sdRushYds <- apply(weeklyDataWide[,grep("rushYds", names(weeklyDataWide))], 1, sd)
sdRushTds <- apply(weeklyDataWide[,grep("rushTds", names(weeklyDataWide))], 1, sd)
sdRec <- apply(weeklyDataWide[,grep("rec", names(weeklyDataWide))], 1, sd)
sdRecYds <- apply(weeklyDataWide[,grep("recYds", names(weeklyDataWide))], 1, sd)
sdRecTds <- apply(weeklyDataWide[,grep("recTds", names(weeklyDataWide))], 1, sd)

sdVars <- data.frame(sdPassYds, sdPassTds, sdPassInt, sdRushYds, sdRushTds, sdRec, sdRecYds, sdRecTds)

#Convert 0s to NA
sdVars[sdVars == 0] <- NA

#Density Plots
ggplot(sdVars, aes(x=sdPassYds)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Week-to-Week SD of Pass Yards")
ggsave(paste(getwd(),"/Figures/SD Pass Yards.jpg", sep=""), width=10, height=10)
dev.off()

ggplot(sdVars, aes(x=sdPassTds)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Week-to-Week SD of Pass TDs")
ggsave(paste(getwd(),"/Figures/SD Pass TDs.jpg", sep=""), width=10, height=10)
dev.off()

ggplot(sdVars, aes(x=sdPassInt)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Week-to-Week SD of Pass INTs")
ggsave(paste(getwd(),"/Figures/SD Pass INTs.jpg", sep=""), width=10, height=10)
dev.off()

ggplot(sdVars, aes(x=sdRushYds)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Week-to-Week SD of Rush Yards")
ggsave(paste(getwd(),"/Figures/SD Rush Yards.jpg", sep=""), width=10, height=10)
dev.off()

ggplot(sdVars, aes(x=sdRushTds)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Week-to-Week SD of Rush TDs")
ggsave(paste(getwd(),"/Figures/SD Rush TDs.jpg", sep=""), width=10, height=10)
dev.off()

ggplot(sdVars, aes(x=sdRec)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Week-to-Week SD of Rec")
ggsave(paste(getwd(),"/Figures/SD Rec.jpg", sep=""), width=10, height=10)
dev.off()

ggplot(sdVars, aes(x=sdRecYds)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Week-to-Week SD of Rec Yards")
ggsave(paste(getwd(),"/Figures/SD Rec Yards.jpg", sep=""), width=10, height=10)
dev.off()

ggplot(sdVars, aes(x=sdRecTds)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Week-to-Week SD of Rec TDs")
ggsave(paste(getwd(),"/Figures/SD Rec TDs.jpg", sep=""), width=10, height=10)
dev.off()

#Average of SDs
apply(sdVars, 2, function(x) mean(x, na.rm=TRUE))
apply(sdVars, 2, function(x) median(x, na.rm=TRUE))
apply(sdVars, 2, function(x) wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate)

#Robust average
sdAverage <- data.frame(t(apply(sdVars, 2, function(x) wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate)))

#Save file
save(weeklyDataFinal, file = paste(getwd(),"/Data/historicalWeekly.RData", sep=""))
write.csv(weeklyDataFinal, file=paste(getwd(),"/Data/historicalWeekly.csv", sep=""), row.names=FALSE)

save(sdAverage, file = paste(getwd(),"/Data/sdAverage.RData", sep=""))
write.csv(sdAverage, file=paste(getwd(),"/Data/sdAverage.csv", sep=""), row.names=FALSE)
