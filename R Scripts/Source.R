#Run fantasy football analysis files in order

#1. Download ESPN.com projections
source(paste(getwd(),"/R Scripts/ESPN Projections.R", sep=""), echo=TRUE)

#2. Download cbssports.com projections
source(paste(getwd(),"/R Scripts/CBS Projections.R", sep=""), echo=TRUE)

#3. Calculate projections for your league
source(paste(getwd(),"/R Scripts/Calculate League Projections.R", sep=""), echo=TRUE)

#4. Evaluate league projections from last year
source(paste(getwd(),"/R Scripts/Evaluate Projections.R", sep=""), echo=TRUE)

#5. Calculate players' risk levels
source(paste(getwd(),"/R Scripts/Risk.R", sep=""), echo=TRUE)