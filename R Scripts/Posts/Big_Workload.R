
###########################
# File: Scatter Plot of Fantasy Data 300+ Rushers
# John Newton 
###########################


#Libraries
library("XML")
library("ggplot2")
library("stringr")
library("plyr")
library("gridExtra")

setwd()

lm_eqn = function(df){
  m = lm(y ~ x, df);
  
  if (coef(m)[2] >= 0)  {    
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(abs(coef(m)[2]), digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  }
  else{
  eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(abs(coef(m)[2]), digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 3))) 
    
  }
  as.character(as.expression(eq));                 
}

###################################################
# Importing Data
###################################################

#Density Chart

Rush<-read.csv('Deviations.csv', header=TRUE)
names(Rush)
attach(Rush)

Deviation<-ts(Rush[,23])
Deviation<-jitter(Deviation) #Add jitter term to break ties
x<-Deviation

Games<-ts(Rush[,24])
Carry<-ts(Rush[,25])
Yards<-ts(Rush[,26])
YG<-ts(Rush[,27])
YPC<-ts(Rush[,28])
TD<-ts(Rush[,29])

###################################################
# Histogram of Season PLayed
###################################################

#count player names



###################################################
# Game Model
###################################################

y<-Games

axis.max=max(y)
df <- data.frame(y,x)
model<-lm(y~x)
summary(model)


plot1<-ggplot(df, aes(x=x, y=y)) +
  geom_point(colour = "red", size=10, alpha=0.8) +geom_vline(xintercept = 0) +geom_hline(yintercept = 0)  + geom_text(size=10,aes(x = 5, y = axis.max, label = lm_eqn(df)), parse = TRUE) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, colour="black", size=2)   +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE) + theme(axis.text.y=element_text(size = rel(3), colour = "black"), axis.text.x=element_text(size = rel(3), colour = "black"), axis.title.x=element_text(size = rel(3)), axis.title.y=element_text(size = rel(3)), plot.title=element_text(size=rel(3))) +xlab("Seasons Removed from First 300+ Carry Season")+ylab("Games Played Deviation") +ggtitle("Scatterplot of Games Played Deviation and Deviation \n from First 300+ Carry Season") 
plot1

theplot<-arrangeGrob(plot1, ncol=1)




###################################################
# Carry Model
###################################################

y<-Carry

axis.max=max(y)
df <- data.frame(y,x)
model<-lm(y~x)
summary(model)


plot2<-ggplot(df, aes(x=x, y=y)) +
  geom_point(colour = "blue", size=10, alpha=0.8) +geom_vline(xintercept = 0) +geom_hline(yintercept = 0)  + geom_text(size=10,aes(x = 5, y = axis.max, label = lm_eqn(df)), parse = TRUE) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, colour="black", size=2)   +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE) + theme(axis.text.y=element_text(size = rel(3), colour = "black"), axis.text.x=element_text(size = rel(3), colour = "black"), axis.title.x=element_text(size = rel(3)), axis.title.y=element_text(size = rel(3)), plot.title=element_text(size=rel(3))) +xlab("Seasons Removed from First 300+ Carry Season")+ylab("Carry Deviation") +ggtitle("Scatterplot of Carry Deviation and Deviation \n from First 300+ Carry Season") 

plot2

theplot<-arrangeGrob(plot2, ncol=1)



###################################################
# Yards Model
###################################################

y<-Yards

axis.max=max(y)
df <- data.frame(y,x)
model<-lm(y~x)
summary(model)


plot3<-ggplot(df, aes(x=x, y=y)) +
  geom_point(colour = "orange", size=10, alpha=0.9) +geom_vline(xintercept = 0) +geom_hline(yintercept = 0)  + geom_text(size=10,aes(x = 5, y = axis.max, label = lm_eqn(df)), parse = TRUE) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, colour="black", size=2)   +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE) + theme(axis.text.y=element_text(size = rel(3), colour = "black"), axis.text.x=element_text(size = rel(3), colour = "black"), axis.title.x=element_text(size = rel(3)), axis.title.y=element_text(size = rel(3)), plot.title=element_text(size=rel(3))) +xlab("Seasons Removed from First 300+ Carry Season")+ylab("Yards Deviation") +ggtitle("Scatterplot of Yards Deviation and Deviation \n from First 300+ Carry Season") 

plot3
theplot<-arrangeGrob(plot3, ncol=1)





###################################################
# Yards/Game Model
###################################################

y<-YG

axis.max=max(y)
df <- data.frame(y,x)
model<-lm(y~x)
summary(model)


plot4<-ggplot(df, aes(x=x, y=y)) +
  geom_point(colour = "green", size=10, alpha=0.9) +geom_vline(xintercept = 0) +geom_hline(yintercept = 0)  + geom_text(size=10,aes(x = 5, y = axis.max, label = lm_eqn(df)), parse = TRUE) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, colour="black", size=2)   +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE) + theme(axis.text.y=element_text(size = rel(3), colour = "black"), axis.text.x=element_text(size = rel(3), colour = "black"), axis.title.x=element_text(size = rel(3)), axis.title.y=element_text(size = rel(3)), plot.title=element_text(size=rel(3))) +xlab("Seasons Removed from First 300+ Carry Season")+ylab("Yards Per Game Deviation") +ggtitle("Scatterplot of YPG Deviation and Deviation \n from First 300+ Carry Season") 
plot4
theplot<-arrangeGrob(plot4, ncol=1)




###################################################
# YPC Model
###################################################

y<-YPC

axis.max=max(y)
df <- data.frame(y,x)
model<-lm(y~x)
summary(model)


plot5<-ggplot(df, aes(x=x, y=y)) +
  geom_point(colour = "black", size=10, alpha=0.5) +geom_vline(xintercept = 0) +geom_hline(yintercept = 0)  + geom_text(size=10,aes(x = 5, y = axis.max, label = lm_eqn(df)), parse = TRUE) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, colour="black", size=2)   +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE) + theme(axis.text.y=element_text(size = rel(3), colour = "black"), axis.text.x=element_text(size = rel(3), colour = "black"), axis.title.x=element_text(size = rel(3)), axis.title.y=element_text(size = rel(3)), plot.title=element_text(size=rel(3))) +xlab("Seasons Removed from First 300+ Carry Season")+ylab("Yards Per Carry Deviation") +ggtitle("Scatterplot of YPC Deviation and Deviation \n from First 300+ Carry Season") 

plot5
theplot<-arrangeGrob(plot5, ncol=1)



###################################################
# Game Model
###################################################

y<-TD

axis.max=max(y)
df <- data.frame(y,x)
model<-lm(y~x)
summary(model)


plot6<-ggplot(df, aes(x=x, y=y)) +
  geom_point(colour = "grey50", size=10, alpha=0.8) +geom_vline(xintercept = 0) +geom_hline(yintercept = 0)  + geom_text(size=10,aes(x = 5, y = axis.max, label = lm_eqn(df)), parse = TRUE) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, colour="black", size=2)   +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE) + theme(axis.text.y=element_text(size = rel(3), colour = "black"), axis.text.x=element_text(size = rel(3), colour = "black"), axis.title.x=element_text(size = rel(3)), axis.title.y=element_text(size = rel(3)), plot.title=element_text(size=rel(3))) +xlab("Seasons Removed from First 300+ Carry Season")+ylab("Touchdown Deviation") +ggtitle("Scatterplot of TD Deviation and Deviation \n from First 300+ Carry Season") 
plot6
theplot<-arrangeGrob(plot6, ncol=1)

