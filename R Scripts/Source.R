#Run fantasy football analysis files in order

#1. Download ESPN.com projections
source(paste(getwd(),"/R Scripts/ESPN Projections.R", sep=""), echo=TRUE)

#2. Download CBSsports.com projections
source(paste(getwd(),"/R Scripts/CBS Projections.R", sep=""), echo=TRUE)

#3. Download NFL.com projections
source(paste(getwd(),"/R Scripts/NFL Projections.R", sep=""), echo=TRUE)

#4. Download FantasyPros projections
source(paste(getwd(),"/R Scripts/FantasyPros Projections.R", sep=""), echo=TRUE)

#5. Calculate projections for your league
source(paste(getwd(),"/R Scripts/Calculate League Projections.R", sep=""), echo=TRUE)

#6. Evaluate league projections from last year
source(paste(getwd(),"/R Scripts/Evaluate Projections.R", sep=""), echo=TRUE)

#7. Calculate players' risk levels
source(paste(getwd(),"/R Scripts/Risk.R", sep=""), echo=TRUE)

#8. Calculate VOR for snake draft
source(paste(getwd(),"/R Scripts/Value Over Replacement.R", sep=""), echo=TRUE)

#9. Download average auction values
source(paste(getwd(),"/R Scripts/Avg Cost.R", sep=""), echo=TRUE)

#10. Calculate optimal team (https://office.microsoft.com/en-us/excel-help/pick-your-fantasy-football-team-with-solver-HA001124603.aspx)
source(paste(getwd(),"/R Scripts/Optimum Roster.R", sep=""), echo=TRUE)

#11. Calculate optimal risk
source(paste(getwd(),"/R Scripts/Optimum Risk.R", sep=""), echo=TRUE)

#12. Calculate Bid Up To
source(paste(getwd(),"/R Scripts/Bid Up To.R", sep=""), echo=TRUE)

#13. Calculate optimal team via Simulation
source(paste(getwd(),"/R Scripts/Optimum Roster.R", sep=""), echo=TRUE)

#14. Calculate Bid Up To via Simulation
source(paste(getwd(),"/R Scripts/Optimum Roster.R", sep=""), echo=TRUE)

#15. Draft Day (remove unavailable/drafted players)
source(paste(getwd(),"/R Scripts/Draft Day.R", sep=""), echo=TRUE)