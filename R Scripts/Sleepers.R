###########################
# File: Sleepers.R
# Description: Identify Sleepers
# Date: 5/24/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net) -- adapted from Drew Conway (http://www.r-bloggers.com/leveraging-the-wisdom-of-crowds-for-fantasy-football/)
# Notes:
# To do:
###########################

#Type of drafts to scrape
num.teams <- 10           #Number of teams in your league
rounds <- 15              #Number of rounds completed, should not change (15 standard num to complete draft for 10 teams)
num.obs <- 20000          #Number of drafts to scrape and parse
humans <- 10    		      #Number of human drafters, in my case I want all humans

#Load libraries
library("XML")
library("ggplot2")

#Functions
base.url <- "http://fantasyfootballcalculator.com/draft/"
seed.url <- paste("http://fantasyfootballcalculator.com/completed_drafts.php?format=standard&teams=",num.teams,sep="")

get.seeds <- function(url, rounds, humans) {
  #Returns url ids for drafts matching num.team and rounds criteria
  seed.drafts <- readHTMLTable(seed.url, header=TRUE, stringsAsFactors=FALSE)[[1]]
  names(seed.drafts) <- c("DraftID", "Date", "Time(EST)", "Format", "TotalTeams", "Humans", "Rounds", "RoundsCompleted", "ViewEntireDraft")
  seed.drafts$RoundsCompleted <- as.numeric(seed.drafts$RoundsCompleted)
  seed.drafts$Humans <- as.numeric(seed.drafts$Humans)
  fit.drafts <- subset(seed.drafts, RoundsCompleted == rounds & Humans >= humans)
  
  return(fit.drafts$DraftID)
}

get.dates <- function(url, rounds, humans) {
  #Returns url ids for drafts matching num.team and rounds criteria
  seed.drafts <- readHTMLTable(url, header=TRUE, stringsAsFactors=FALSE)[[1]]
  names(seed.drafts) <- c("DraftID", "Date", "Time(EST)", "Format", "TotalTeams", "Humans", "Rounds", "RoundsCompleted", "ViewEntireDraft")
  seed.drafts$RoundsCompleted <-as.numeric(seed.drafts$RoundsCompleted)
  seed.drafts$Humans <- as.numeric(seed.drafts$Humans)
  fit.drafts <- subset(seed.drafts, RoundsCompleted == rounds & Humans >= humans)
  
  return(fit.drafts$Date)
}

get.df <- function(draft.id, num.teams, rounds) {
  #Returns draft data as properly formatted data frame
  draft.table <- readHTMLTable(paste(base.url, draft.id, sep=""), header=1:num.teams+1)$draftboard
  draft.order <- t(draft.table)
  draft.order <- draft.order[1:num.teams+1,]
  
  #Melt data into draft order
  raw <- melt(draft.order)
  raw <- gsub(" ","",as.character(raw$value))
  
  #Create seperate columns for player name, position and team
  raw <- strsplit(raw,"[\\(\\)]")
  raw <- do.call(rbind, raw)
  player <- raw[,1]  # Players
  team <- raw[,2]    # Team
  
  #Create vector for player position
  pos <- sapply(player,function(x) substring(x, nchar(x)-1))
  
  #Strip keep just player name
  player <- sapply(player,function(x)substring(x, first=1, last=nchar(x)-2))
  order <- 1:length(player) # Draft order
  df <- cbind(player,pos,team,order)
  row.names(df) <- order
  colnames(df) <- c("Player","Position","Team","Order")
  
  return(df)
}

### Scrape draft position data

#Get seed draft IDs
first.draft <- get.seeds(seed.url, rounds, humans)
draft.data <- lapply(first.draft, function(d) {get.df(d, num.teams, rounds)})

#Build out list of data frames
dates <- vector()
chunk.pos <- 25
pb <- txtProgressBar(min = 0, max = num.obs, style = 3)

while(length(draft.data) < num.obs) {
  setTxtProgressBar(pb, length(draft.data))
  
  dates <- c(dates, get.dates(paste(seed.url, "&list=", chunk.pos, sep=""), rounds, humans))
  dates <- c(dates[1], dates[length(dates)])
  new.seeds <- get.seeds(paste(seed.url, "&list=", chunk.pos, sep=""), rounds, humans)
  seed.data <- lapply(new.seeds, function(d) {get.df(d, num.teams, rounds)})
  # Add new data to full set
  for(f in seed.data) {
    draft.data[[length(draft.data) + 1]] <- f
  }
  chunk.pos <- chunk.pos + 25  # NOTE, the final number of observations may be > num.obs, but will never be less
}
dates

#Take list of draft position data frames and convert to single data frame 
drafts.df <- do.call(rbind, draft.data)
row.names(drafts.df) <- 1:nrow(drafts.df)
drafts.df <- as.data.frame(drafts.df, stringsAsFactors=FALSE)
drafts.df$Order <- as.numeric(drafts.df$Order)

#raw.output <- paste(getwd(), "/Rankings from Mock drafts/", "raw_draft.csv", sep="") #"raw_draft.csv"
#write.csv(drafts.df, raw.output, row.names=FALSE)    # Output raw data

#Compute draft statistics  
drafts.stats <- ddply(drafts.df, .(Player, Position, Team), summarise, mean=mean(Order),
                    sd=sd(Order), freq=length(Order)/length(draft.data), mad=mad(Order), median=median(Order))

#Clean up
drafts.stats$name_ffc <- as.character(drafts.stats$Player)
drafts.stats$name_ffc <- gsub("DefenseD", "", drafts.stats$name_ffc)
drafts.stats$name <- toupper(gsub("[[:punct:]]", "", gsub(" ", "", drafts.stats$name_ffc)))
drafts.stats$pos <- as.character(drafts.stats$Position)
drafts.stats$pos[which(drafts.stats$pos == "EF")] <- "DEF"
drafts.stats$pos[which(drafts.stats$pos == "PK")] <- "K"
drafts.stats$team_ffc <- as.character(drafts.stats$Team)

#Reorder by mean draft position
drafts.stats <- drafts.stats[with(drafts.stats, order(mean)),]

#Subset data
sleepers <- drafts.stats[,c("name","name_ffc","pos","team_ffc","mean","sd","freq","mad","median")]

#Plot
plotTitle <- paste("Sleepers from", dates[2], "to", dates[1], sep=" ")
ex.mad <- quantile(drafts.stats$mad, .95)

value.plot <- ggplot(subset(drafts.stats, drafts.stats$mad >= ex.mad), aes(median, mad)) +
  geom_text(aes(label = name_ffc, alpha=.85, colour="red", size=3.5), position=position_jitter(w=4,h=2)) +
  geom_point(data=subset(drafts.stats, drafts.stats$mad < ex.mad)) +
  stat_smooth(data=drafts.stats, aes(median, mad)) +
  xlab("Median Player Draft Position") +
  ylab("Median Absolute Deviation (MAD) Player Draft Position") +
  ggtitle(plotTitle) +
  annotate("text",label="Only players with MAD in the \n95th percentile are labeled", color="darkred", x=20, y=40) +
  scale_color_manual(values=c("red"="darkred")) +
  theme(legend.position="none")

pdf(paste(getwd(),"/Figures/Sleepers.pdf", sep=""), width=10, height=6)
#jpeg(paste(getwd(),"/Figures/Sleepers.jpg", sep=""))
value.plot
dev.off()

#Save file
save(sleepers, file = paste(getwd(),"/Data/sleepers.RData", sep=""))
write.csv(sleepers, file=paste(getwd(),"/Data/sleepers.csv", sep=""), row.names=FALSE)
