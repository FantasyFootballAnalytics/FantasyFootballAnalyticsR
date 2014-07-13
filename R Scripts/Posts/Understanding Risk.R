playerA <- rnorm(1000000, mean=150, sd=5)
playerB <- rnorm(1000000, mean=150, sd=15)
playerC <- rnorm(1000000, mean=150, sd=30)

mydata <- data.frame(playerA, playerB, playerC)

points <- c(mydata$playerA,mydata$playerB,mydata$playerC)
player <- c(rep("Player A",dim(mydata)[1]),rep("Player B",dim(mydata)[1]),rep("Player C",dim(mydata)[1])) #,rep("Latent",dim(projections)[1])
mydata2 <- data.frame(points,player)

ggplot(mydata2, aes(x=points, fill=player)) + geom_density(alpha=.3) + xlab("Players' Projected Points") + ggtitle("Density Plot of Projected Points for 3 Players") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Understanding Risk.jpg", sep=""), width=10, height=10)
dev.off()