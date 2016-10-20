
# A function that turns c("one","two","three") to "one, two, and three."
p_and<- function(x) {
  paste(paste(x[1:(length(x)-1)],collapse=", "), "and", x[length(x)])
}

bullet1 <-function(pos,lt) {
  paste0(p_and(Models[position==pos&leagueType==lt][pm>3][Status %in% c("","Q")][
    order(-upside)][1:5][order(rank)][,unique(player)]), " are the five players
 with the <b>largest upside</b> (as measured from their (pseudo)medians). For these players,
 some projections are placing much higher valuations than others. 
 If you are projected to lose this week by quite a few points and
 are looking for a risky play that may tip the balance in your favor,
 these are players to consider.")
}
bullet2 <- function(pos,lt) {
  paste0(p_and(Models[position==pos&leagueType==lt][pm>3][Status %in% c("","Q")][
    order(downside)][1:5][order(rank)][,unique(player)])," are the players with the 
    <b> smallest downside</b>, which suggests that while their median projection might
    not be great, there is less uncertainty concerning how poorly they may perform.  So,
    if you are likely to win by a lot and want to reduce your downside risk, these
    players may deserve extra attention.")
}
bullet3 <- function(pos,lt) {
  paste0("On the other hand, ", p_and(Models[position==pos&leagueType==lt][pm>3][Status %in% c("","Q")][
    order(-downside)][1:5][order(rank)][,unique(player)]),
         " are the five players with the <b>largest downside</b> this week.  If you are 
    planning on starting them, it may be prudent to investigate why some projections have 
    such low expectations for these players.")
}

# This a single-purpose graphing function that generates the gold-mining charts we want.
# It makes all sorts assumptions about the data-table we feed it.  It particular, it assumes
# the following columns exist: leagueType, rank, position, floor, ceiling, tier,player.
# The reason we have this function is because each week we make 4 such charts
# Note that, for free, we can generate charts for RB, WR, QBs, Kickers, Defenses, TE, etc. 

roundupGraph <- function(ModelsDT,pos,Scoring,nPlayers,next_week) {
  xlimit<- max(ModelsDT[leagueType==Scoring&rank<nPlayers&position==pos,ceiling],na.rm=TRUE)+3
  ModelsDT[,tier:=factor(tier)]
  ggplot(ModelsDT[leagueType==Scoring&rank<nPlayers&position==pos], aes(x=pm, y=rank, color=tier)) +
    geom_errorbarh(aes(xmin=floor,xmax=ceiling),height=.3)+
    geom_point(size=5,color="white")+
    geom_text(aes(x=pm,label=round(pm,0)),size=3,show.legend = FALSE)+
    geom_text(aes(x=ceiling, label=player),
              hjust=-0.2, angle=(0), size=3,show.legend = FALSE)+
    geom_text(aes(x=floor,label=Status),color="red",fontface="bold",
              hjust=1.2, angle=(0), size=3,show.legend = FALSE)+
    theme_minimal()+
    theme(
      #plot.background = element_blank(),
      #panel.grid.major.x = element_line(color="grey"),
      #panel.grid.minor.y = element_line(color="grey"),
      #panel.border=element_rect(color="grey",fill=NA),
      #panel.background = element_blank(),
      legend.position = c(0.95, 0.1)
    ) + scale_y_reverse()+
    ylab("Rank") + xlab("Median FPTS Projection with Confidence Interval") +
    labs(title = paste("Week ", next_week," ",pos," Projections Roundup ",Scoring, " Leagues",sep=""))+
    coord_cartesian(xlim =c(0,xlimit))+
    scale_color_tableau()
}


# Hodges Lehmann estimator.  The hl estimator chokes when there is a projection of 0.
# In that case, we use median +/- the sd.  Considering alternatives.

h.l <- function(x){tryCatch({
  wilcox.test(x,na.action="na.exclude",conf.int=TRUE,conf.level=.95)},
  error=function(e){return(list(estimate=median(x),
                                conf.int=c(max(median(x)-sd(x),min(x)),
                                           min(median(x)+sd(x),max(x)))))})
}

