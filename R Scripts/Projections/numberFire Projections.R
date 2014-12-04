projections_nf <- read.csv("F:/Documents/GitHub/FantasyFootballAnalyticsR/Data/numberFire-Projections.csv", stringsAsFactors = FALSE, header=TRUE)

#Pass Comp/Att
projections_nf$passComp_nf <- str_trim(sapply(str_split(projections_nf$compAtt_nf, "/"), "[[", 1))
projections_nf$passAtt_nf <- str_trim(sapply(str_split(projections_nf$compAtt_nf, "/"), "[[", 2))

#Add missing variables
projections_nf$returnTds_nf <- NA
projections_nf$twoPts_nf <- NA
projections_nf$fumbles_nf <- NA

#projections_nf[,c(prefix, paste(varNames, suffix, sep="_"))]

#Convert variables from character strings to numeric
projections_nf[,c("passAtt_nf","passComp_nf","passYds_nf","passTds_nf","passInt_nf","rushAtt_nf","rushYds_nf","rushTds_nf","rec_nf","recYds_nf","recTds_nf","returnTds_nf","twoPts_nf","fumbles_nf","pts_nf")] <- 
  convert.magic(projections_nf[,c("passAtt_nf","passComp_nf","passYds_nf","passTds_nf","passInt_nf","rushAtt_nf","rushYds_nf","rushTds_nf","rec_nf","recYds_nf","recTds_nf","returnTds_nf","twoPts_nf","fumbles_nf","pts_nf")], "numeric")

#Player name, position, and team
projections_nf$name_nf <- str_trim(sapply(str_split(projections_nf$player_nf, "\\("), "[[", 1))
projections_nf$name <- nameMerge(projections_nf$name_nf)
projections_nf$pos <- as.factor(str_trim(sapply(str_split(sapply(str_split(projections_nf$player_nf, "\\("), "[[", 2), "\\,"), "[[", 1)))
projections_nf$team_nf <- str_trim(sapply(str_split(sapply(str_split(projections_nf$player_nf, "\\)"), "[[", 1), "\\,"), "[[", 2))

#Remove duplicate cases
projections_nf[projections_nf$name %in% projections_nf[duplicated(projections_nf$name),"name"],]
#projections_nf <- projections_nf[-which(projections_nf$name_nf=="Dexter McCluster" & projections_nf$pos=="RB"),]

#Calculate overall rank
projections_nf$overallRank_nf <- rank(-projections_nf$pts_nf, ties.method="min")

#Calculate Position Rank
projections_nf$positionRank_nf <- NA
projections_nf[which(projections_nf$pos == "QB"), "positionRank_nf"] <- rank(-projections_nf[which(projections_nf$pos == "QB"), "pts_nf"], ties.method="min")
projections_nf[which(projections_nf$pos == "RB"), "positionRank_nf"] <- rank(-projections_nf[which(projections_nf$pos == "RB"), "pts_nf"], ties.method="min")
projections_nf[which(projections_nf$pos == "WR"), "positionRank_nf"] <- rank(-projections_nf[which(projections_nf$pos == "WR"), "pts_nf"], ties.method="min")
projections_nf[which(projections_nf$pos == "TE"), "positionRank_nf"] <- rank(-projections_nf[which(projections_nf$pos == "TE"), "pts_nf"], ties.method="min")

#Order variables in data set
projections_nf <- projections_nf[,c(prefix, paste(varNames, suffix, sep="_"))]

#Order players by overall rank
projections_nf <- projections_nf[order(projections_nf$overallRank_nf),]
row.names(projections_nf) <- 1:dim(projections_nf)[1]

#Density Plot
ggplot(projections_nf, aes(x=pts_nf)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of numberFire Projected Points")
ggsave(paste(getwd(),"/Figures/numberFire projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_nf, file = paste(getwd(), "/Data/numberFire-Projections.RData", sep=""))
write.csv(projections_nf, file=paste(getwd(), "/Data/numberFire-Projections.csv", sep=""), row.names=FALSE)

save(projections_nf, file = paste(getwd(), "/Data/Historical Projections/numberFire-Projections-", season, ".RData", sep=""))
write.csv(projections_nf, file=paste(getwd(), "/Data/Historical Projections/numberFire-Projections-", season, ".csv", sep=""), row.names=FALSE)