###########################
# File: ui.R
# Description: UI for Shiny Application to Determine Optimal Draft Team
# Date: 6/10/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
###########################

#setwd("./shinyapp")

#Library
library("shiny")

#Load Data
load("./shinyData.RData")

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Optimal Fantasy Football Draft"),
  
  sidebarPanel(
    numericInput("leagueCap", "League Cap:", 200),
    
    numericInput("numTotalPlayers", "Total Number of Players on Roster:", 20),
    
    sliderInput("maxRisk", 
                "Max Player Risk Tolerance:", 
                min = ceiling(min(shinyData$risk, na.rm=TRUE)), 
                max = ceiling(max(shinyData$risk, na.rm=TRUE)),
                step=.1,
                value = 5),

    sliderInput("numQBs",
                "Number of Quarterbacks:",
                min = 0,
                max = 4,
                value = 1),
    
    sliderInput("numRBs",
                "Number of Running Backs:",
                min = 0,
                max = 4,
                value = 2),
    
    sliderInput("numWRs",
                "Number of Wide Receivers:",
                min = 0,
                max = 4,
                value = 2),
    
    sliderInput("numTEs",
                "Number of Tight Ends:",
                min = 0,
                max = 4,
                value = 1),
    
    sliderInput("numWRTEs",
                "Number of WR/TEs:",
                min = 0,
                max = 4,
                value = 0),
    
    sliderInput("numWRRBs",
                "Number of WR/RBs:",
                min = 0,
                max = 4,
                value = 0),
    
    sliderInput("numWRRBTEs",
                "Number of WR/RB/TEs:",
                min = 0,
                max = 4,
                value = 1),
    
    sliderInput("numQBWRRBTEs",
                "Number of QB/WR/RB/TEs:",
                min = 0,
                max = 4,
                value = 0)
    ),
  
  mainPanel(
    tableOutput("bestTeam"),
    h3(textOutput("totalPoints")),
    h3(textOutput("totalCost")),
    h3(textOutput("maxCost")),
    h3(textOutput("numStarters"))
    )
))

#runApp("~/shinyapp")
#runApp()