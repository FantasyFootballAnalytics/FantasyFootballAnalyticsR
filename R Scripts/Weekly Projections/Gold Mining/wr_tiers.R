#install libraries
library("XML")
library("seqinr")
library("stringr")
library(xtable)
library("mclust")
library("ggplot2")

#enter the week
week <- "8"

#grab the data
cbs_nz_url <- paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/", week, "/nathan_zegura/ppr?&print_rows=9999", sep="")
cbs_je_url <- paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/", week, "/jamey_eisenberg/ppr?&print_rows=9999", sep="")
cbs_dr_url <- paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/", week, "/dave_richard/ppr?&print_rows=9999", sep="")

url1_espn <- paste("http://games.espn.go.com/ffl/tools/projections?slotCategoryId=4&scoringPeriodId=", week, "&seasonId=2013", sep="")
url2_espn <- paste("http://games.espn.go.com/ffl/tools/projections?slotCategoryId=4&scoringPeriodId=", week, "&seasonId=2013&startIndex=40", sep="")
url3_espn <- paste("http://games.espn.go.com/ffl/tools/projections?slotCategoryId=4&scoringPeriodId=", week, "&seasonId=2013&startIndex=80", sep="")
url4_espn <- paste("http://games.espn.go.com/ffl/tools/projections?slotCategoryId=4&scoringPeriodId=", week, "&seasonId=2013&startIndex=120", sep="")

url_fftoday1 <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2013&GameWeek=", week, "&PosID=30", sep="")
url_fftoday2 <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2013&GameWeek=", week, "&PosID=30&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1", sep="")

#url_bsports <- "http://www.bsports.com/decisionmakerwr"

url_ffs <- "http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=WR"

#grab rank data
url_fantasy_pro_ranks <- ("http://www.fantasypros.com/nfl/rankings/ppr-wr.php")

partial_espn_wrproj1 <- readHTMLTable(url1_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
partial_espn_wrproj2 <- readHTMLTable(url2_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
partial_espn_wrproj3 <- readHTMLTable(url3_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
partial_espn_wrproj4 <- readHTMLTable(url4_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0

nz_wrproj <- readHTMLTable(cbs_nz_url, as.data.frame=TRUE, stringsAsFactors=FALSE)[7]$`NULL`
je_wrproj <- readHTMLTable(cbs_je_url, as.data.frame=TRUE, stringsAsFactors=FALSE)[7]$`NULL`
dr_wrproj <- readHTMLTable(cbs_dr_url, as.data.frame=TRUE, stringsAsFactors=FALSE)[7]$`NULL`

import_fftoday_wrproj1 <- readHTMLTable(url_fftoday1, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
import_fftoday_wrproj2 <- readHTMLTable(url_fftoday2, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`

#import_bsports_wrproj <- readHTMLTable(url_bsports, as.data.frame=TRUE, stringsAsFactors=FALSE)$`tablepress-174`

import_ffs <- readHTMLTable(url_ffs, as.data.frame=TRUE, stringsAsFactors=FALSE)[3]$`NULL`

#grab rank data
data_ranks <- readHTMLTable(url_fantasy_pro_ranks, as.data.frame=TRUE, stringsAsFactors=FALSE)$data

#trim the data
nz_wrproj <- nz_wrproj[4:(dim(nz_wrproj)[1]-1),]
je_wrproj <- je_wrproj[4:(dim(je_wrproj)[1]-1),]
dr_wrproj <- dr_wrproj[4:(dim(dr_wrproj)[1]-1),]

partial_espn_wrproj1 <- partial_espn_wrproj1[2:(dim(partial_espn_wrproj1)[1]),]
partial_espn_wrproj2 <- partial_espn_wrproj2[2:(dim(partial_espn_wrproj2)[1]),]
partial_espn_wrproj3 <- partial_espn_wrproj3[2:(dim(partial_espn_wrproj3)[1]),]
partial_espn_wrproj4 <- partial_espn_wrproj4[2:(dim(partial_espn_wrproj4)[1]),]

import_fftoday_wrproj1 <- import_fftoday_wrproj1[3:(dim(import_fftoday_wrproj1)[1]-1),]
import_fftoday_wrproj2 <- import_fftoday_wrproj2[3:(dim(import_fftoday_wrproj2)[1]-1),]

import_ffs <- subset(import_ffs, select = c("V2","V3","V4","V5","V6","V7","V8","V9","V10","V11"))
import_ffs <- import_ffs[3:(dim(import_ffs)[1]-1),]
import_ffs <- na.omit(import_ffs)

#no need to trim import_bsports_wrproj

#rbind the necessary data
espn_wrproj <- rbind(partial_espn_wrproj1, partial_espn_wrproj2, partial_espn_wrproj3, partial_espn_wrproj4)
fftoday_wrproj <- rbind(import_fftoday_wrproj1, import_fftoday_wrproj2)

#convert into my naming convention
#bsp_wrproj <- import_bsports_wrproj
ffs_wrproj <- import_ffs

#trim the top 100 wr projections
nz_wrproj <- nz_wrproj[1:100,]
je_wrproj <- je_wrproj[1:100,]
dr_wrproj <- dr_wrproj[1:100,]
espn_wrproj <- espn_wrproj[1:100,]
#less than 100, no need to trim ## fftoday_wrproj <- fftoday_wrproj[1:100,]
#bsp_wrproj <- bsp_wrproj[1:100,]
ffs_wrproj <- ffs_wrproj[1:100,]

#remove an NAs
espn_wrproj <- na.omit(espn_wrproj)
ffs_wrproj <- na.omit(ffs_wrproj)

#get rid of unwanted columns
espn_wrproj <- subset(espn_wrproj, select = c(1,11,12,13,14))
fftoday_wrproj <- subset(fftoday_wrproj, select = c(2,5,6,7,8))
#bsp_wrproj <- subset(bsp_wrproj, select = c(2,8,9,10,11))
ffs_wrproj <- subset(ffs_wrproj, select = c(1,4,5,9,10))

#name the columns
names(nz_wrproj) <- c("player","nz_recpts","nz_yd","nz_avg","nz_td","nz_fl","nz_fpts")
names(je_wrproj) <- c("player","je_recpts","je_yd","je_avg","je_td","je_fl","je_fpts")
names(dr_wrproj) <- c("player","dr_recpts","dr_yd","dr_avg","dr_td","dr_fl","dr_fpts")
names(espn_wrproj) <- c("player","espn_recpts","espn_yd","espn_td","espn_fpts")
names(fftoday_wrproj) <- c("player","fft_recpts","fft_yd","fft_td","fft_fpts")
#names(bsp_wrproj) <- c("player","bsp_recpts","bsp_yd","bsp_td","bsp_fpts")
names(ffs_wrproj) <- c("player","ffs_recpts","ffs_yd","ffs_td","ffs_fpts")

#name rank data
names(data_ranks) <- c("rank","name","best","worst","avg","sd")

#remove unwanted columns
#espn_wrproj <- subset(espn_wrproj, select = c("player","es_fpts","je_fpts","dr_fpts"))

#convert to numeric
nz_wrproj$nz_recpts <- as.numeric(nz_wrproj$nz_recpts)
nz_wrproj$nz_yd <- as.numeric(nz_wrproj$nz_yd)
nz_wrproj$nz_avg <- as.numeric(nz_wrproj$nz_avg)
nz_wrproj$nz_td <- as.numeric(nz_wrproj$nz_td)
nz_wrproj$nz_fl <- as.numeric(nz_wrproj$nz_fl)
nz_wrproj$nz_fpts <- as.numeric(nz_wrproj$nz_fpts)

je_wrproj$je_recpts <- as.numeric(je_wrproj$je_recpts)
je_wrproj$je_yd <- as.numeric(je_wrproj$je_yd)
je_wrproj$je_avg <- as.numeric(je_wrproj$je_avg)
je_wrproj$je_td <- as.numeric(je_wrproj$je_td)
je_wrproj$je_fl <- as.numeric(je_wrproj$je_fl)
je_wrproj$je_fpts <- as.numeric(je_wrproj$je_fpts)

dr_wrproj$dr_recpts <- as.numeric(dr_wrproj$dr_recpts)
dr_wrproj$dr_yd <- as.numeric(dr_wrproj$dr_yd)
dr_wrproj$dr_avg <- as.numeric(dr_wrproj$dr_avg)
dr_wrproj$dr_td <- as.numeric(dr_wrproj$dr_td)
dr_wrproj$dr_fl <- as.numeric(dr_wrproj$dr_fl)
dr_wrproj$dr_fpts <- as.numeric(dr_wrproj$dr_fpts)

espn_wrproj$espn_recpts <- as.numeric(espn_wrproj$espn_recpts)
espn_wrproj$espn_yd <- as.numeric(espn_wrproj$espn_yd)
espn_wrproj$espn_td <- as.numeric(espn_wrproj$espn_td)
espn_wrproj$espn_fpts <- as.numeric(espn_wrproj$espn_fpts)

fftoday_wrproj$fft_recpts <- as.numeric(fftoday_wrproj$fft_recpts)
fftoday_wrproj$fft_yd <- as.numeric(fftoday_wrproj$fft_yd)
fftoday_wrproj$fft_td <- as.numeric(fftoday_wrproj$fft_td)
fftoday_wrproj$fft_fpts <- as.numeric(fftoday_wrproj$fft_fpts)

bsp_wrproj$bsp_recpts <- as.numeric(bsp_wrproj$bsp_recpts)
bsp_wrproj$bsp_yd <- as.numeric(bsp_wrproj$bsp_yd)
bsp_wrproj$bsp_td <- as.numeric(bsp_wrproj$bsp_td)
bsp_wrproj$bsp_fpts <- as.numeric(bsp_wrproj$bsp_fpts)

ffs_wrproj$ffs_recpts <- as.numeric(ffs_wrproj$ffs_recpts)
ffs_wrproj$ffs_yd <- as.numeric(ffs_wrproj$ffs_yd)
ffs_wrproj$ffs_td <- as.numeric(ffs_wrproj$ffs_td)
ffs_wrproj$ffs_fpts <- as.numeric(ffs_wrproj$ffs_fpts)

#convert ranks data
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$avg <- as.numeric(data_ranks$avg)
data_ranks$sd <- as.numeric(data_ranks$sd)
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$best <- as.numeric(data_ranks$best)
data_ranks$worst <- as.numeric(data_ranks$worst)

#round bsp projections
bsp_wrproj$bsp_recpts <- round(bsp_wrproj$bsp_recpts, digits = 0)
bsp_wrproj$bsp_yd <- round(bsp_wrproj$bsp_yd, digits = 0)
bsp_wrproj$bsp_td <- round(bsp_wrproj$bsp_td, digits = 0)
bsp_wrproj$bsp_fpts <- round(bsp_wrproj$bsp_fpts, digits = 0)

#convert espn, fftoday to ppr
espn_wrproj$espn_fpts <- espn_wrproj$espn_recpts + espn_wrproj$espn_fpts
fftoday_wrproj$fft_fpts <- fftoday_wrproj$fft_recpts + fftoday_wrproj$fft_fpts
ffs_wrproj$ffs_fpts <- ffs_wrproj$ffs_recpts + ffs_wrproj$ffs_fpts

#remove team from player names & create name column
nz_wrproj$name <- str_sub(nz_wrproj$player, end=str_locate(string=nz_wrproj$player, ',')[,1]-1)
je_wrproj$name <- str_sub(je_wrproj$player, end=str_locate(string=je_wrproj$player, ',')[,1]-1)
dr_wrproj$name <- str_sub(dr_wrproj$player, end=str_locate(string=dr_wrproj$player, ',')[,1]-1)
espn_wrproj$name <- str_sub(espn_wrproj$player, end=str_locate(string=espn_wrproj$player, ',')[,1]-1)
espn_wrproj$name <- str_replace_all(espn_wrproj$name, "\\*", "")
bsp_wrproj$name <- bsp_wrproj$player
ffs_wrproj$name <- ffs_wrproj$player

#remove symbol from fftoday
fftoday_wrproj$name <- str_replace_all(fftoday_wrproj$player, "Â", "")
fftoday_wrproj$name <- str_replace_all(fftoday_wrproj$name, "^\\s+", "")

#remove formating from ffs
ffs_wrproj$name <- str_replace_all(ffs_wrproj$name, " R", "")
ffs_wrproj$name <- strsplit(ffs_wrproj$name, ",")
ffs_wrproj$name <- unlist(lapply(ffs_wrproj$name, 
                                 function(x) paste(x[1:length(x) %% 2 == 0], 
                                                   x[1:length(x) %% 2 != 0])))

#add team column
nz_wrproj$team <- str_trim(str_sub(nz_wrproj$player, start= -3))
je_wrproj$team <- str_trim(str_sub(je_wrproj$player, start= -3))
dr_wrproj$team <- str_trim(str_sub(dr_wrproj$player, start= -3))

espn_wrproj$team_espn <- str_sub(espn_wrproj$player, start=str_locate(string=espn_wrproj$player, ',')[,1]+2, end = str_locate(string=espn_wrproj$player, ',')[,1]+4)
espn_wrproj$team_espn <- str_trim(espn_wrproj$team_espn, side="right")
espn_wrproj$team_espn <- toupper(espn_wrproj$team_espn)
espn_wrproj$team_espn[espn_wrproj$team_espn=="WSH"] <- "WAS"

#fix ffs' names
#names_ffs <- ffs_wrproj$player

#fix rank names
data_ranks$name <- sapply(strsplit(data_ranks$name," (",fixed = TRUE),"[[",1)

#fix stevie johnson
ffs_wrproj$name[ffs_wrproj$name=="Stevie Johnson"] <- "Steve Johnson"

#fix T.Y. Hilton
data_ranks$name[data_ranks$name=="Ty Hilton"] <- "T.Y. Hilton"

#resort data
nz_wrproj <- subset(nz_wrproj, select = c("name","team","nz_recpts","nz_yd","nz_avg","nz_td","nz_fl","nz_fpts"))
je_wrproj <- subset(je_wrproj, select = c("name","team","je_recpts","je_yd","je_avg","je_td","je_fl","je_fpts"))
dr_wrproj <- subset(dr_wrproj, select = c("name","team","dr_recpts","dr_yd","dr_avg","dr_td","dr_fl","dr_fpts"))

espn_wrproj <- subset(espn_wrproj, select = c("name","team_espn","espn_recpts","espn_yd","espn_td","espn_fpts"))

fftoday_wrproj <- subset(fftoday_wrproj, select = c("name","fft_recpts","fft_yd","fft_td","fft_fpts"))

bsp_wrproj <- subset(bsp_wrproj, select = c("name","bsp_recpts","bsp_yd","bsp_td","bsp_fpts"))

ffs_wrproj <- subset(ffs_wrproj, select = c("name","ffs_recpts","ffs_yd","ffs_td","ffs_fpts"))

#merge data (probably a better way)
projections <- merge(nz_wrproj, je_wrproj, by="name")
projections <- merge(projections, dr_wrproj, by="name")
projections <- merge(projections, espn_wrproj, by="name")
projections <- merge(projections, fftoday_wrproj, by="name")
projections <- merge(projections, bsp_wrproj, by="name")
projections <- merge(projections, ffs_wrproj, by="name")

#storing backup with ranks
data_rank_transfer <- data_ranks

#get rid of unwanted column
data_rank_transfer <- subset(data_rank_transfer, select = c ("name","worst","avg","best","rank"))

#remove duplicate team columns (probably a better way - maybe not send them in the first place?)
projections$team.y <- NULL
projections$team <- NULL
projections$team_espn <- NULL

#subset name and each predicted fpts
fantasyproj <- subset(projections, select = c("name","nz_fpts","espn_fpts","bsp_fpts", "je_fpts", "fft_fpts", "dr_fpts", "ffs_fpts"))

#Hodges-Lehmann estimator
fantasyproj$hodges.lehmann <- apply(fantasyproj[,c(2,3,4,5,6,7,8)],1, function(x) wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate)
fantasyproj$hodges.lehmann <- round(fantasyproj[,'hodges.lehmann'], 1)

#ncol count
columns_fantasyproj <- ncol(fantasyproj)

#find standard deviation of projections
fantasyproj_stdev <- apply(fantasyproj[2:columns_fantasyproj], 1, sd, na.rm = TRUE)

#find mean of projections
fantasyproj_mean <- apply(fantasyproj[2:columns_fantasyproj], 1, mean)

#round mean/stdev
fantasyproj_mean <- round(fantasyproj_mean, digits = 1)
fantasyproj_stdev <- round(fantasyproj_stdev, digits = 1)

#add mean/stdev
fantasyproj$mean <- fantasyproj_mean
fantasyproj$sd <- fantasyproj_stdev

#remove players without projections
#fantasyproj <- fantasyproj[!(fantasyproj$SD==0),]

#sort players by stdev
fantasyproj <- fantasyproj[order(-fantasyproj$sd), , drop = FALSE]

#add in ranks to fantasyprojections
fantasyproj <- merge(fantasyproj, data_rank_transfer, by="name")

#just mean & sd
#fantasyproj <- subset(fantasyproj, select = c("name","mean","sd"))

#training data
training <- subset(fantasyproj, select = c("mean"))

#clustering
cluster <- Mclust(training$mean, G=7)
fantasyproj$Tier <- cluster$classification

#add ceiling / floor columns
fantasyproj$Ceiling <- fantasyproj$mean + fantasyproj$sd
fantasyproj$Floor <- fantasyproj$mean - fantasyproj$sd

#sort by floor
fantasyproj <- fantasyproj[order(-fantasyproj$Floor), , drop = FALSE]

#combine data ranks?
#fantasyproj <- merge(fantasyproj, data_ranks, by="name")

#graphing
p <- ggplot(fantasyproj, aes(y=hodges.lehmann, x=rank, color=factor(Tier*-1)))

#find height
graph.height <- max(fantasyproj$hodges.lehmann + fantasyproj$sd + 5)

p + 
  geom_point(size = I(3)) + 
  geom_errorbar(aes(ymax = hodges.lehmann + sd, ymin = hodges.lehmann - sd, height = .2), alpha = I(0.4)) + 
  geom_text(aes(y=hodges.lehmann + sd, label=name, hjust=(-.08), vjust=(.5), angle=(90), size=1)) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,legend.position = "none"
  ) +
  xlab("Average Rank") + ylab("Average FPTS Projection") + labs(title = paste("Week ", week, " WRs Uncertainty", sep="")) +
  coord_cartesian(ylim = (0:graph.height))

# q <- ggplot(fantasyproj, aes(y=rank, x=mean, color=factor(Tier*-1))) 
# 
# q + 
#   geom_point(aes(x=Ceiling), size = I(0)) + 
#   geom_point(aes(x=mean), size = I(3)) + 
#   geom_text(aes(x=Floor, label=name, hjust=(-.15), vjust=(0), angle=(0), size=1)) +
#   geom_errorbarh(aes(xmax = Ceiling, xmin = Floor, height = .2), alpha = I(0.4)) + 
#   theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#     ,legend.position = "none"
#   ) +
#   scale_x_reverse() +
#   scale_y_reverse()

#put table into html
# htmltable_fantasyproj <- xtable(fantasyproj)
# htmltable_projections <- xtable(projections)

#save the html file
# print.xtable(htmltable_fantasyproj, type="html", file="week11tier.html", include.rownames=FALSE)
# print.xtable(htmltable_projections, type="html", file="week11wrprojections.html", include.rownames=FALSE)
