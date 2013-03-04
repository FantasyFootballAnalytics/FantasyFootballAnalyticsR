#Run fantasy football analysis files in order

#1. Download ESPN.com projections
source(paste(getwd(),"/R Scripts/ESPN Projections.R", sep=""), echo=TRUE)

#2. Download cbssports.com projections
source(paste(getwd(),"/R Scripts/CBS Projections.R", sep=""), echo=TRUE)

#3. Calculate projections for your league
source(paste(getwd(),"/R Scripts/Calculate League Projections.R", sep=""), echo=TRUE)

#4. Evaluate league projections from last year
source(paste(getwd(),"/R Scripts/Evaluate Projections.R", sep=""), echo=TRUE)

#5. Calculate players' risk levels (http://www.drewconway.com/zia/?p=2305)
source(paste(getwd(),"/R Scripts/Risk.R", sep=""), echo=TRUE)

#6. Calculate VOR for snake draft (https://harvardsportsanalysis.wordpress.com/2012/04/13/optimizing-draft-strategies-in-fantasy-football-abstract/)

#7. Download average auction values

#8. Calculate optimal team (https://office.microsoft.com/en-us/excel-help/pick-your-fantasy-football-team-with-solver-HA001124603.aspx)

#9. Calculate Bid Up To