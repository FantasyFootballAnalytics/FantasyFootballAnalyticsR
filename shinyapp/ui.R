###########################
# File: ui.R
# Description: UI for Shiny Application to Determine Optimal Draft Team
# Date: 6/10/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
###########################

#Library
library("shiny")

#Load Data
#shinyData <- read.csv("./shinyData.csv")
shinyData <- read.csv("/home/dadrivr/ShinyApps/FantasyFootballDraftOptimizer/shinyData.csv")

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Fantasy Football Draft Optimizer"),
  
  sidebarPanel(
    p("For directions on how to use optimizer, see ",
      a("here.", href="http://fantasyfootballanalyticsr.blogspot.com/")
    ),
    
    numericInput("leagueCap", "League Cap:", 200),
    numericInput("numTotalPlayers", "Total Number of Players on Roster:", 20),
    
    numericInput("passYdsMultiplier", "Passing Yards Per Point:", 25),
    numericInput("passTdsMultiplier", "Points Per Passing TD:", 4),
    numericInput("passIntMultiplier", "Points Per Passing INT:", -2),
    numericInput("rushYdsMultiplier", "Rushing Yards Per Point:", 10),
    numericInput("rushTdsMultiplier", "Points Per Rushing TD:", 6),
    numericInput("recMultiplier", "Points Per Reception:", 0),
    numericInput("recYdsMultiplier", "Receiving Yards Per Point:", 10),
    numericInput("recTdsMultiplier", "Points Per Receiving TD:", 6),
    #numericInput("twoPtsMultiplier", "Points Per 2-Pt Conversions:", 0),
    numericInput("fumbleMulitplier", "Points Per Fumble:", -2),
    
    selectInput("yourDrafted", "Players You Drafted:", sort(shinyData$name), multiple=TRUE),
    numericInput("capSpent", "Cap Spent:", 0),
    selectInput("otherDrafted", "Other Players Drafted:", sort(shinyData$name), multiple=TRUE),
    
    sliderInput("maxRisk", "Max Player Risk Tolerance:", 
                min = ceiling(min(shinyData$risk, na.rm=TRUE)), 
                max = ceiling(max(shinyData$risk, na.rm=TRUE)),
                step=.1, value = 5),
    
    sliderInput("numQBs", "Number of Starting Quarterbacks:", min = 0, max = 4, value = 1),
    sliderInput("numRBs", "Number of Starting Running Backs:", min = 0, max = 4, value = 2),
    sliderInput("numWRs", "Number of Starting Wide Receivers:", min = 0, max = 4, value = 2),
    sliderInput("numTEs", "Number of Starting Tight Ends:", min = 0, max = 4, value = 1),
    sliderInput("numWRTEs", "Number of Starting WR/TEs:", min = 0, max = 4, value = 0),
    sliderInput("numWRRBs", "Number of Starting WR/RBs:", min = 0, max = 4, value = 0),
    sliderInput("numWRRBTEs", "Number of Starting WR/RB/TEs:", min = 0, max = 4, value = 1),
    sliderInput("numQBWRRBTEs", "Number of Starting QB/WR/RB/TEs:", min = 0, max = 4, value = 0),
    
    br(),
    downloadButton("downloadData", "Download Data as .csv")
  ),
  
  mainPanel(
    tableOutput("bestTeam"),
    h3(textOutput("totalPoints")),
    h3(textOutput("totalCost")),
    h3(textOutput("capAvailable")),
    h3(textOutput("numStarters"))
  )
))
