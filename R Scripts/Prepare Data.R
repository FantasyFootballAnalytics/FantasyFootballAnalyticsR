#Run fantasy football analysis files in order

###DON'T FORGET TO MANUALLY DOWNLOAD AVG COST FROM YAHOO

#1. Download IDP rankings
source(paste(getwd(),"/R Scripts/Rankings/IDP.R", sep=""), echo=TRUE)

#2. Download K rankings
source(paste(getwd(),"/R Scripts/Rankings/Kickers.R", sep=""), echo=TRUE)

#3. Download Accuscore.com projections
source(paste(getwd(),"/R Scripts/Projections/Accuscore Projections.R", sep=""), echo=TRUE)

#4. Download CBSsports.com projections
source(paste(getwd(),"/R Scripts/Projections/CBS Projections.R", sep=""), echo=TRUE)

#5. Download ESPN.com projections
source(paste(getwd(),"/R Scripts/Projections/ESPN Projections.R", sep=""), echo=TRUE)

#6. Download FantasyPros projections
source(paste(getwd(),"/R Scripts/Projections/FantasyPros Projections.R", sep=""), echo=TRUE)

#7. Download FantasySharks projections
source(paste(getwd(),"/R Scripts/Projections/FantasySharks Projections.R", sep=""), echo=TRUE)

#8. Download FFtoday projections
source(paste(getwd(),"/R Scripts/Projections/FFtoday.R", sep=""), echo=TRUE)

#9. Download NFL.com projections
source(paste(getwd(),"/R Scripts/Projections/NFL Projections.R", sep=""), echo=TRUE)

#10. Download Yahoo.com projections
source(paste(getwd(),"/R Scripts/Projections/Yahoo Projections.R", sep=""), echo=TRUE)

#11. Calculate projections for your league
source(paste(getwd(),"/R Scripts/Calculations/Calculate League Projections.R", sep=""), echo=TRUE)

#12. Wisdom of the Crowd
source(paste(getwd(),"/R Scripts/Calculations/Wisdom of the Crowd.R", sep=""), echo=TRUE)

#13. Calculate players' risk levels
source(paste(getwd(),"/R Scripts/Calculations/Risk.R", sep=""), echo=TRUE)

#14. Calculate VOR for snake draft
source(paste(getwd(),"/R Scripts/Calculations/Value Over Replacement.R", sep=""), echo=TRUE)

#15. Download average auction values
source(paste(getwd(),"/R Scripts/Calculations/Avg Cost.R", sep=""), echo=TRUE)

#16. Calculate optimal team
source(paste(getwd(),"/R Scripts/Calculations/Optimum Roster.R", sep=""), echo=TRUE)

#17. Calculate optimal risk
source(paste(getwd(),"/R Scripts/Calculations/Optimum Risk.R", sep=""), echo=TRUE)

#18. Calculate Bid Up To
source(paste(getwd(),"/R Scripts/Calculations/Bid Up To.R", sep=""), echo=TRUE)

#19. Calculate optimal team via simulation
source(paste(getwd(),"/R Scripts/Calculations/Simulation.R", sep=""), echo=TRUE)

#20. Calculate Bid Up To via simulation
source(paste(getwd(),"/R Scripts/Calculations/Bid Up To Simulation.R", sep=""), echo=TRUE)

#21. Draft Day (remove unavailable/drafted players)
source(paste(getwd(),"/R Scripts/Draft Day/Draft Day.R", sep=""), echo=TRUE)

##########
# Other
##########

#Evaluate league projections from last year
source(paste(getwd(),"/R Scripts/Posts/Evaluate Projections.R", sep=""), echo=TRUE)
