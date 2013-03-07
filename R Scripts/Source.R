#Run fantasy football analysis files in order

#1. Download ESPN.com projections
source(paste(getwd(),"/R Scripts/ESPN Projections.R", sep=""), echo=TRUE)

#2. Download CBSsports.com projections
source(paste(getwd(),"/R Scripts/CBS Projections.R", sep=""), echo=TRUE)

#3. Download NFL.com projections
source(paste(getwd(),"/R Scripts/NFL Projections.R", sep=""), echo=TRUE)

#4. Calculate projections for your league
source(paste(getwd(),"/R Scripts/Calculate League Projections.R", sep=""), echo=TRUE)

#5. Evaluate league projections from last year
source(paste(getwd(),"/R Scripts/Evaluate Projections.R", sep=""), echo=TRUE)

#6. Calculate players' risk levels (http://www.drewconway.com/zia/?p=2305)
source(paste(getwd(),"/R Scripts/Risk.R", sep=""), echo=TRUE)

#7. Calculate VOR for snake draft (https://harvardsportsanalysis.wordpress.com/2012/04/13/optimizing-draft-strategies-in-fantasy-football-abstract/)

#8. Download average auction values

#9. Calculate optimal team (https://office.microsoft.com/en-us/excel-help/pick-your-fantasy-football-team-with-solver-HA001124603.aspx)

#10. Calculate Bid Up To