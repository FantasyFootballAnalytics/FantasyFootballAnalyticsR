require(data.table)
require(XML)
require(stringr)

#Turn NAs to zero
NAtoZero <- function (DT) {
  # Makes all NAs in a data table zero.  Warning: this acts on the data table passed to it.
  for (j in names(DT))
    set(DT,which(is.na(DT[[j]])),j,0)
}

#Upcomming Week
seasonStart<- as.Date("2014-08-27")
next_week <- trunc(as.numeric((as.Date(Sys.time())-seasonStart))/7)

#I'm writting this script for WRs in a PPR league, but we might as well make it flexible.
spos="WR"
league_type="ppr"

# Get CBS WR Data #####
#Build URLs
cbs_base_url = "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections"
#CBS Writers (Nathan Zagura dose not write for them anymore.)
cbs_writers = c("jamey_eisenberg","dave_richard")
cbs_urls=paste(cbs_base_url,"/",spos,"/",next_week,"/",cbs_writers,"/",league_type,"?&print_rows=9999",sep="")
#Read Data
cbs<-lapply(cbs_urls, function(x) {data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)[7]$`NULL`)})
#Do CBS WR specific manipulations (we could use an apply function, but this way we just edit in place.)
for(i in 1:length(cbs)) {
  #Add Writer's name to projection
  cbs[[i]][,writer:= cbs_writers[i]]
  #Remove top 2 and last entry.
  cbs[[i]]<-cbs[[i]][3:(nrow(cbs[[i]])-1)]  
}
cbs<-rbindlist(cbs)
cbs[,c("week","pos","src","scoring"):=list(next_week,spos,"cbs",league_type)]
#Change column names.
setnames(cbs,paste("V",1:7,sep=""),c("player","rec","rec_yd","yd_p_rec","rec_td","fum","fpts"))
cbs[,c("yd_p_rec","fum"):=NULL] #All we really need is player and fpts.
#remove team from player names & create name column
cbs[,name:=str_sub(player, end=str_locate(string=player, ',')[,1]-1)]
#add team name
cbs[,team:=str_trim(str_sub(player, start= -3))]
cbs[,player:=NULL]

# Get ESPN WR Data ####
##Build URL
espn_base_url <- paste("http://games.espn.go.com/ffl/tools/projections?seasonId=",year(seasonStart),"&scoringPeriodId=",next_week,sep="")
espn_pos<- list(QB=0,RB=2,WR=4,TE=6,D=16,K=17,Flex=23)
espn_pages<-c("0","40","80")
espn_urls <- paste0(espn_base_url,"&slotCategoryId=",espn_pos[[spos]],"&startIndex=",espn_pages)
espn<- lapply(espn_urls,function(x) {data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0)})

##Do row removal before binding data set.
for(i in 1:length(espn)){
  ##Remove top entry.
  espn[[i]]<-espn[[i]][2:nrow(espn[[i]])]
}
espn<-rbindlist(espn)
##Add week, position, src, and writer
espn[,c("week","pos","src","writer","scoring"):=list(next_week,spos,"espn","espn","std")]
##Delete Extraneous columns
espn[,paste("V",c(2:10),sep=""):=NULL]
##Change column names.
setnames(espn,paste("V",c(1,11:14),sep=""),c("player","rec","rec_yd","rec_td","fpts"))

##convert to numeric
suppressWarnings(espn[,c("rec","rec_yd","rec_td","fpts"):=lapply(list(rec,rec_yd,rec_td,fpts),as.numeric)])
##remove team from player names & create name column
espn[,name:=str_sub(player, end=str_locate(string=player, ',')[,1]-1)]
espn[,name:=str_replace_all(name, "\\*", "")]
##create team column
espn[,team:=str_sub(player, start=str_locate(string=player, ',')[,1]+2, end = str_locate(string=player, ',')[,1]+4)]
espn[,team:=toupper(str_trim(team, side="right"))]
##Remove free agents
espn<- espn[team!="FA"]
##Rename WSH
espn[team=="WSH",team:="WAS"]
##Set NAs to Zero
NAtoZero(espn)
espn[,player:=NULL]

# Get FF Today WR Data ####
# fft_pos<-list(QB=10,RB=20,WR=30,TE=40,K=80)
# fft_base_url<-paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",
#                     year(seasonStart),"&GameWeek=",next_week,
#                     "&LeagueID=1&order_by=FFPts&sort_order=DESC&PosID=",fft_pos[[spos]],sep="")
# fft_pages<-c("0","1")
# fft_urls<-paste(fft_base_url,"&cur_page=",fft_pages,sep="")
# fft<-lapply(fft_urls,function(x) {data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`)})
# #Do row removal pior to rbind
# for(i in 1:length(fft)) {
#   ##Delete Row 1
#   fft[[i]]<-fft[[i]][2:nrow(fft[[i]])]
# }
# fft<-rbindlist(fft)
# ##Add week, pos, src, and writer.
# fft[,c("week","pos","src","writer","scoring"):=list(next_week,spos,"fft","fft","std")]
# ##Delete extraneous collumns
# fft[,c("V1","V4"):=NULL]
# ##setnames
# setnames(fft,c("V2","V3","V5","V6","V7","V8"),c("player","team","rec","rec_yd","rec_td","fpts"))
# ##Add player name
# fft[,name:=str_replace_all(player, "^Â\\s+", "")]
# ##convert to numeric
# fft[,c("rec","rec_yd","rec_td","fpts"):=lapply(list(rec,rec_yd,rec_td,fpts),as.numeric)]
# fft[,player:=NULL]

# Get FF Sharks Data ####
ffs_pos=list(QB="QB",RB="RB",WR="WR",TE="TE",flex="FLEX",K="PK",DEF="D")
ffs_base_url <- "http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos="
ffs_urls <- paste(ffs_base_url,ffs_pos[[spos]],sep="")
ffs<-data.table(readHTMLTable(ffs_urls, as.data.frame=TRUE, stringsAsFactors=FALSE)[7]$`NULL`)
ffs[1:20]
ffs<-ffs[!is.na(V2)]
ffs[,c("V1","V4","V5"):=NULL]
setnames(ffs,c("player","team","rec","rec_yd","rec_td","fpts"))
ffs<-ffs[!player %in% c("Name","Rec")]
ffs[,c("rec","rec_yd","rec_td","fpts"):=lapply(list(rec,rec_yd,rec_td,fpts),as.numeric)]
ffs[,name:={a<-read.table(text=ffs$player,sep=",",colClasses = "character")
            paste(a$V2,a$V1,sep=" ")}]
ffs[,c("writer","week","pos","src","scoring"):=list("ffs",next_week,spos,"ffs","std")]
ffs[,player:=NULL]

# Get PickingPros Data ####
pp_base_url="http://pickingpros.com/nfl/"
pp_pos <- list(QB="quarterback",RB="running-back",WR="wide-receiver",TE="tight-end")
pp_url <- paste0(pp_base_url,next_week,"-",pp_pos[[spos]],"-","projections.php")
pp <-data.table(readHTMLTable(pp_url, as.data.frame=TRUE, stringsAsFactors=FALSE)$example)
pp[,c("#","Opp","Tar","Avg","1st"):=NULL]
setnames(pp,c("name","rec","rec_yd","rec_td","fpts"))
#No team could complicate matters at some point.
pp[,team:=NA]
pp[,c("writer","week","pos","src","scoring"):=list("pp",next_week,spos,"pp","std")]

# Get Fox Sports Data ####
fx_base_url=paste0("http://www.whatifsports.com/beyondtheboxscore/NFL",substr(year(seasonStart),3,4),"_Projections/Week_",next_week,"/")
fx_pos=list(QB="QB",RB="RB",WR="WR",TE="TE",K="K",DEF="TDEF")
fx_url=paste0(fx_base_url,fx_pos[[spos]],".htm")
fx<-data.table(readHTMLTable(fx_url,data.frame=TRUE,stringsAsFactors=FALSE)$`NULL`)
fx[,V1:=NULL]
setnames(fx,c("name","team","rec","rec_yd","rec_td","fpts"))
fx<-fx[name!="Name"]
fx[,c("writer","week","pos","src","scoring"):=list("fx",next_week,spos,"fx","std")]

# Get Yahoo Data ####
# Using Isaacs leauge: 39345 (mine does not use standard scoring.  I assume this league does.)
yahoo_baseurl <-"http://football.fantasysports.yahoo.com/f1/39345/players?status=ALL&cut_type=9&myteam=0&sort=PTS&sdir=1"
yahoo_pages <- paste("&count=",seq(0,150,by=25),sep="")
yahoo_pos<-list(QB="QB",RB="RB",WR="WR",TE="TE",FLEX="W%2FR%2FT",K="K",DEF="DEF")
yahoo_urls <- paste0(yahoo_baseurl,yahoo_pages,"&pos=",spos,"&stat1=S_PW_",next_week)
yahoo_urls

#Pull from Yahoo
yahoo<-lapply(yahoo_urls,function(x) {data.table(readHTMLTable(x, stringsAsFactors = FALSE)[2]$'NULL')})
yback<-copy(yahoo)
yahoo<-rbindlist(yahoo)

#Variable Names
yahoo<-yahoo[,c(2,5,16,17,18),with=FALSE]
setnames(yahoo,c("player","fpts","rec","rec_yd","rec_td"))
yahoo<-yahoo[,lapply(.SD,function(x) gsub("\\,", "", x))]

yahoo[,player := str_trim(sapply(str_split(player, "\n"), "[[", 2))]
yahoo[,pos := str_trim(str_sub(player, start= -2))]
yahoo[,name := str_trim(str_sub(player, start=0, end=nchar(player)-8))]
yahoo[,team:= toupper(str_trim(str_sub(player, start=str_locate(player, " - ")[,1]-3, end=str_locate(player, " - ")[,1]-1)))]

yahoo[,c("week","src","writer","scoring"):=list(next_week,"yahoo","yahoo","std")]
yahoo[,player:=NULL]
#TODO Get numberFire Data? ####
"https://www.numberfire.com/nfl/fantasy/fantasy-football-projections"

# Get FPro Ranks Data ####
## Read directly from file export.
ppr_ranks<-data.table(read.table("http://www.fantasypros.com/nfl/rankings/ppr-wr.php?export=xls","\t",skip=5,row.names=NULL,header=TRUE))
std_ranks<-data.table(read.table("http://www.fantasypros.com/nfl/rankings/wr.php?export=xls",sep="\t",skip=5,row.names=NULL,header=TRUE))
ranks <- list(ppr_ranks,std_ranks)
scoring<-c("ppr","std")

for(i in 1:length(ranks)){
  ranks[[i]][,c("Team","Std.Dev","X"):=NULL]
  setnames(ranks[[i]],c("rank","name","team","best.rank","worst.rank","avg.rank","std.dev"))
  ranks[[i]][name=="Ty Hilton",name:="T.Y. Hilton"]
  ranks[[i]][,c("scoring","writer","week","pos","src"):=list(scoring[i],"fpro",next_week,spos,"fpro")]
}

# Aggregate ####

#proj<-list(cbs,espn,ffs,fft,fx,pp,yahoo)
proj<-list(cbs,espn,ffs,fx,pp,yahoo)

for(i in 1:length(proj)){
  setcolorder(proj[[i]],c("pos","week","name","team","rec","rec_yd","rec_td","fpts","scoring","src","writer"))
}

ffa<-rbindlist(proj)
#Ideally, we would like to use the team as a second identifier to make sure we have the correct player.
#However, for the sake of expediency we will forgo this step for now.
ffa[,team:=NULL]
#convert to numeric
ffa[,c("rec","rec_yd","rec_td","fpts"):=lapply(list(rec,rec_yd,rec_td,fpts),as.numeric)]
#Add Alternative Scoring
ffa[scoring=="ppr",c("std_fpts","ppr_fpts"):=list(fpts-rec,fpts)]
ffa[scoring=="std",c("std_fpts","ppr_fpts"):=list(fpts,fpts+rec)]
setnames(ffa,"fpts","src_fpts")
#Rank `em
ffa<-ffa[order(-std_fpts)][,std_rank:=1:.N,by=list(scoring,writer)]
ffa<-ffa[order(-ppr_fpts)][,ppr_rank:=1:.N,by=list(scoring,writer)]
#Add Average ranks
ffa[,std_ave_rank:=mean(std_rank),by=name]
ffa[,ppr_ave_rank:=mean(ppr_rank),by=name]

#Check that we have everyone
ffa[,list(max(std_rank)),by=list(scoring,writer)]
#Restrict to the players that have a rank of less than 100 from some source.
ffa[,min_rank:=min(std_rank,ppr_rank),by=name]
ffa<-ffa[min_rank<100]
#Look for messed up names.  (Not fool-proof)
ffa[,list(obs =.N), by=name][order(-obs)][obs<4]
ffa[name=="Cecil Shorts III",name:="Cecil Shorts"]
ffa[name=="Stevie Johnson",name:="Steve Johnson"]
ffa[name=="Steve Smith Sr.",name:="Steve Smith"]
ffa[name=="Louis Murphy Jr.",name:="Louis Murphy"]
ffa[name=="Ted Ginn Jr.",name:="Ted Ginn"]
ffa[name=="Denard Robinson Ja",name:="Denard Robinson"]
ffa[name=="Odell Beckham Jr.",name:="Odell Beckham"]

#Save
save(ffa,next_week,file="./RMarkdown/GoldMining/ffa_wr.RData")

