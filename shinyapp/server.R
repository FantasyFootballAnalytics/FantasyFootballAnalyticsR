###########################
# File: ui.R
# Description: Server File for Shiny Application to Determine Optimal Draft Team
# Date: 6/10/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
###########################

#Libraries
library(shiny)
library(Rglpk)

#Load Data
load("./shinyData.RData")

#Create Optimization Function
optimizeTeam <- function(points=shinyData$projections, playerCost=NA, maxRisk=NA, maxCost=NA, numTotalStarters=NA, minQBstarters=NA, maxQBstarters=NA, minRBstarters=NA, maxRBstarters=NA, minWRstarters=NA, maxWRstarters=NA, minTEstarters=NA, maxTEstarters=NA){
#optimizeTeam <- function(points=shinyData$projections, playerCost=shinyData$cost, maxRisk=maxRisk, maxCost=maxCost, numTotalStarters=numTotalStarters){ #can change points, cost, or risk #projectedPtsLatent
  num.players <- length(shinyData$name)
  var.types <- rep("B", num.players)
  
  A <- rbind(as.numeric(shinyData$pos == "QB"),
             as.numeric(shinyData$pos == "QB"),
             as.numeric(shinyData$pos == "RB"),
             as.numeric(shinyData$pos == "RB"),
             as.numeric(shinyData$pos == "WR"),
             as.numeric(shinyData$pos == "WR"),
             as.numeric(shinyData$pos == "TE"),
             as.numeric(shinyData$pos == "TE"),
             diag(shinyData$risk),                 # player's risk
             playerCost,                           # total cost
             rep(1,num.players))                   # num of players in starting lineup     
  
  dir <- c(">=",
           "<=",
           ">=",
           "<=",
           ">=",
           "<=",
           ">=",
           "<=",
           rep("<=", num.players),
           "<=",
           "==")
  
  b <- c(minQBstarters,
         maxQBstarters,
         minRBstarters,
         maxRBstarters,
         minWRstarters,
         maxWRstarters,
         minTEstarters,
         maxTEstarters,
         rep(maxRisk, num.players),
         maxCost,
         numTotalStarters)
  
  sol <- Rglpk_solve_LP(obj = points, mat = A, dir = dir, rhs = b,types = var.types, max = TRUE)
  sol$playerInfo <- as.data.frame(merge(shinyData[shinyData$name %in% shinyData[sol$solution == 1,"name"],c("name","pos","team")], shinyData[sol$solution == 1,c("name","projections","risk","cost")], by="name"))
  sol$playerInfo[,"cost"] <- as.integer(sol$playerInfo[,"cost"])
  #sol$totalCost <- sum(playerCost * sol$solution)
  sol$totalCost <- sum(sol$playerInfo$cost)
  sol$players <- shinyData$name[sol$solution == 1]
  return(sol)
}

# Define server logic
shinyServer(function(input, output) {
  
  projectedCost <- reactive({
    ceiling(shinyData$cost * (input$leagueCap/200))
  })
  
  minQBs <- reactive({
    input$numQBs
  })
  
  maxQBs <- reactive({
    input$numQBs + input$numQBWRRBTEs
  })
  
  minRBs <- reactive({
    input$numRBs
  })
  
  maxRBs <- reactive({
    input$numRBs + input$numWRRBs + input$numWRRBTEs + input$numQBWRRBTEs
  })
  
  minWRs <- reactive({
    input$numWRs
  })
  
  maxWRs <- reactive({
    input$numWRs + input$numWRTEs + input$numWRRBs + input$numWRRBTEs + input$numQBWRRBTEs
  })
  
  minTEs <- reactive({
    input$numWRs
  })
  
  maxTEs <- reactive({
    input$numWRs + input$numWRTEs + input$numWRRBTEs + input$numQBWRRBTEs
  })
  
  numStarters <- reactive({
    input$numQBs + input$numRBs + input$numWRs + input$numTEs + input$numWRTEs + input$numWRRBs + input$numWRRBTEs + input$numQBWRRBTEs
  })
  
  maxAvailable <- reactive({
    input$leagueCap - (input$numTotalPlayers - numStarters())
  })
  
  solutionTeam <- reactive({
    optimizeTeam(maxRisk=input$maxRisk, playerCost=projectedCost(), maxCost=maxAvailable(), numTotalStarters=numStarters(), minQBstarters=minQBs(), maxQBstarters=maxQBs(), minRBstarters=minRBs(), maxRBstarters=maxRBs(), minWRstarters=minWRs(), maxWRstarters=maxWRs(), minTEstarters=minTEs(), maxTEstarters=maxTEs())$playerInfo
  })
  
  output$bestTeam <- renderTable({
    solutionTeam()
    #optimizeTeam(maxRisk=input$maxRisk, playerCost=projectedCost(), maxCost=maxAvailable(), numTotalStarters=numStarters(), minQBstarters=minQBs(), maxQBstarters=maxQBs(), minRBstarters=minRBs(), maxRBstarters=maxRBs(), minWRstarters=minWRs(), maxWRstarters=maxWRs(), minTEstarters=minTEs(), maxTEstarters=maxTEs())$playerInfo
  })
  
  output$totalPoints <- renderText({
    paste("Projected Points: ",round(optimizeTeam(maxRisk=input$maxRisk, playerCost=projectedCost(), maxCost=maxAvailable(), numTotalStarters=numStarters(), minQBstarters=minQBs(), maxQBstarters=maxQBs(), minRBstarters=minRBs(), maxRBstarters=maxRBs(), minWRstarters=minWRs(), maxWRstarters=maxWRs(), minTEstarters=minTEs(), maxTEstarters=maxTEs())$optimum, 2))
  })
  
  output$totalCost <- renderText({
    paste("Total Cost: ",optimizeTeam(maxRisk=input$maxRisk, playerCost=projectedCost(), maxCost=maxAvailable(), numTotalStarters=numStarters(), minQBstarters=minQBs(), maxQBstarters=maxQBs(), minRBstarters=minRBs(), maxRBstarters=maxRBs(), minWRstarters=minWRs(), maxWRstarters=maxWRs(), minTEstarters=minTEs(), maxTEstarters=maxTEs())$totalCost)
  })
  
  output$maxAvailable <- renderText({
    paste("Max Cost: ",maxAvailable())
  })
  
  output$numStarters <- renderText({
    paste("Number of Starters: ",numStarters())
  })

})