#Run fantasy football analysis files in order

###DON'T FORGET TO MANUALLY DOWNLOAD AVG COST FROM YAHOO

#1. Download IDP projections
source(paste(getwd(),"/R Scripts/IDP.R", sep=""), echo=TRUE)

#2. Download K projections
source(paste(getwd(),"/R Scripts/Kickers.R", sep=""), echo=TRUE)

#3. Download ESPN.com projections
source(paste(getwd(),"/R Scripts/ESPN Projections.R", sep=""), echo=TRUE)

#4. Download CBSsports.com projections
source(paste(getwd(),"/R Scripts/CBS Projections.R", sep=""), echo=TRUE)

#5. Download NFL.com projections
source(paste(getwd(),"/R Scripts/NFL Projections.R", sep=""), echo=TRUE)

#6. Download FantasyPros projections
source(paste(getwd(),"/R Scripts/FantasyPros Projections.R", sep=""), echo=TRUE)

#7. Calculate projections for your league
source(paste(getwd(),"/R Scripts/Calculate League Projections.R", sep=""), echo=TRUE)

#8. Evaluate league projections from last year
source(paste(getwd(),"/R Scripts/Evaluate Projections.R", sep=""), echo=TRUE)

#9. Calculate players' risk levels
source(paste(getwd(),"/R Scripts/Risk.R", sep=""), echo=TRUE)

#10. Calculate VOR for snake draft
source(paste(getwd(),"/R Scripts/Value Over Replacement.R", sep=""), echo=TRUE)

#11. Download average auction values
source(paste(getwd(),"/R Scripts/Avg Cost.R", sep=""), echo=TRUE)

#12. Calculate optimal team
source(paste(getwd(),"/R Scripts/Optimum Roster.R", sep=""), echo=TRUE)

#13. Calculate optimal risk
source(paste(getwd(),"/R Scripts/Optimum Risk.R", sep=""), echo=TRUE)

#14. Calculate Bid Up To
source(paste(getwd(),"/R Scripts/Bid Up To.R", sep=""), echo=TRUE)

#15. Calculate optimal team via simulation
source(paste(getwd(),"/R Scripts/Simulation.R", sep=""), echo=TRUE)

#16. Calculate Bid Up To via simulation
source(paste(getwd(),"/R Scripts/Bid Up To Simulation.R", sep=""), echo=TRUE)

#17. Draft Day (remove unavailable/drafted players)
source(paste(getwd(),"/R Scripts/Draft Day.R", sep=""), echo=TRUE)
