require(data.table)
require(reshape2)
BreakBuild <- function (df,BCol,IDCols,ValCols) {
  setkeyv(df,BCol)
  NewCols <- unique(df[[BCol]]); #Scan the column for unique entries.  Each entry gets a data table.
  ldt <- list(); #List that will hold a data table for each break.
  ColList <-c(IDCols,ValCols) # List of columns, does not include broken collumn.
  for (bframe in NewCols) {
    ldt[[bframe]] = df[bframe,ColList, with=FALSE] #Create and store a data table with columns from IDCols and VolCols.
    if(length(ValCols)>1){setnames(ldt[[bframe]], ValCols, paste(bframe,ValCols,sep="."))} #Prefix the Value columns with the name of the break.
    else {setnames(ldt[[bframe]], ValCols, bframe)}  #If there is only one Value Column, give it the name of the break.
  }
  Build<-Reduce(function(...) merge(...,by=IDCols,all=T),ldt)
  return(Build)
}

lossFunction <- function(cmin,cmax,proj) {
  x<-rep(NA,length(proj))
  x[proj<=cmax] <- (cmax-proj[proj<=cmax])^2
  x[proj<cmin|proj>cmax] <-0
  return(x)
}

#Get the lineups for yahoo league projection
addLineUps <- function(proj_list,tc=team_config,cw=current_week){
  temp<-copy(proj_list)
  #tc is team_config.
  temp<-temp[order(owner,pos,week,-pts_yahoo)][,POR:=1:.N,by=list(owner,pos,week)]
  temp[week>cw,InLineUp:=FALSE]
  temp[week>cw&(
    pos=="QB"& POR %in% tc$QB|
      pos=="RB"& POR %in% tc$RB|
      pos=="WR"& POR %in% tc$WR|
      pos=="TE"& POR %in% tc$TE|
      pos=="K"& POR %in% tc$K|
      pos=="DEF"& POR %in% tc$DEF),               
    InLineUp:=TRUE]
  temp[week>cw&(InLineUp==FALSE)&(pos %in% tc$FLEX),
       InLineUp:=(1:.N==which.max(pts_yahoo)),
       by=list(week,owner)]
  return(temp)
}

# Step 2. Create a function that selects the best projected Team

ScoreRestofSeason <- function(proj_list) {
  cw=current_week
  proj_list<-addLineUps(proj_list)
  return(proj_list[week>cw&week<=16&InLineUp==TRUE,list(pts_yahoo=sum(pts_yahoo)),by="owner"])
}

changeOwner <-function(proj_list,PlayerName,NewOwner) {
  projcopy <-copy(proj_list)
  setkey(projcopy,name)
  for(i in 1:length(PlayerName)){
    projcopy[J(PlayerName[i]),owner:=NewOwner[i]]
  }
  return(projcopy)
}

GetTrades <- function(Owner1,Owner2,proj_list) {
  #First, one player trades.
  proj_list <- copy(proj_list[owner %in% c(Owner1,Owner2)])
  Original<-ScoreRestofSeason(proj_list)
  Team1 <- proj_list[name!="GENERICFA"][owner==Owner1&week==(current_week+1),list(owner,name)]
  Team2 <- proj_list[name!="GENERICFA"][owner==Owner2&week==(current_week+1),list(owner,name)]
  
  #TradeOptions<-data.table(expand.grid(Team1$name,Team1$name,Team2$name,Team2$name,stringsAsFactors = FALSE))
  T1<-data.table(t(combn(Team1$name,2)),ID=1)
  T1 <- rbindlist(list(T1,data.table(Team1$name,Team1$name,ID=1)))
  T2<-data.table(t(combn(Team2$name,2)),ID=1)
  T2 <- rbindlist(list(T2,data.table(Team2$name,Team2$name,ID=1)))
  TradeOptions<-merge(T1,T2,by="ID",allow.cartesian=TRUE)[,ID:=NULL]
  setnames(TradeOptions,c(paste(Owner2,1:2,sep=" "),paste(Owner1,1:2,sep=" ")))
  TradeOptions[,TradeNum:=1:nrow(TradeOptions)]
  Test<-melt(TradeOptions,id.vars="TradeNum",variable.name="NewOwner",value.name = "name",
             variable.factor =FALSE,value.factor=FALSE)
  Test[,NewOwner:= substr(NewOwner,1,nchar(NewOwner)-2)]
  
  setkey(Test,TradeNum,NewOwner,name)
  Test<-unique(Test)
  HypoSeasons<-Test[,ScoreRestofSeason(changeOwner(proj_list,name,NewOwner)),by=TradeNum]
  Compare<-merge(Original,HypoSeasons,by="owner")
  Compare[,Benefit:=pts_yahoo.y-pts_yahoo.x]
  Compare[,MB:=(all(Benefit>=-5)),by=TradeNum]
  output<-merge(Compare[MB==TRUE][owner %in% c(Owner1,Owner2)],TradeOptions,by="TradeNum")[order(-Benefit)]
  setnames(output,c(paste(Owner2,c(1,2)),paste(Owner1,c(1,2))),c("P1","P2","P3","P4"))
  cat(paste(Owner1, "and",Owner2,"complete.\n"))
  return(output)
}