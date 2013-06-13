###########################
# File: server.R
# Description: Server File for Shiny Application to Determine Optimal Draft Team
# Date: 6/10/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes:
# Put function within server functon
# Add user inputs for league settings
###########################

#Libraries
library(shiny)
library(Rglpk)

#Load Data
shinyData <- read.csv("./shinyData.csv")

# Define server logic
shinyServer(function(input, output) {
  
  #Calculate inverse multipliers
  passYdsMultiplierInv <- reactive({
    1/input$passYdsMultiplier
  })
  
  rushYdsMultiplierInv <- reactive({
    1/input$rushYdsMultiplier
  })
  
  recYdsMultiplierInv <- reactive({
    1/input$recYdsMultiplier
  })
  
  #Data frame calculations  
  newData <- reactive({
    
    #Calculate projected points for each stat category
    shinyData$passYdsPts <- shinyData$passYds * passYdsMultiplierInv()
    shinyData$passTdsPts <- shinyData$passTds * input$passTdsMultiplier
    shinyData$passIntPts <- shinyData$passInt * input$passIntMultiplier
    shinyData$rushYdsPts <- shinyData$rushYds * rushYdsMultiplierInv()
    shinyData$rushTdsPts <- shinyData$rushTds * input$rushTdsMultiplier
    shinyData$recPts <- shinyData$rec * input$recMultiplier
    shinyData$recYdsPts <- shinyData$recYds * recYdsMultiplierInv()
    shinyData$recTdsPts <- shinyData$recTds * input$recTdsMultiplier
    #shinyData$twoPtsPts <- shinyData$twoPts * input$twoPtsMultiplier
    shinyData$fumblesPts <- shinyData$fumbles * input$fumbleMulitplier
    
    #Calculate projected points
    shinyData$projectedPts <- rowSums(shinyData[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","fumblesPts")], na.rm=TRUE) #,"twoPtsPts"
    
    #Calculate Position Rank
    shinyData$positionRank[shinyData$pos=="QB"] <- rank(-shinyData$projectedPts[shinyData$pos=="QB"], ties.method="min")
    shinyData$positionRank[shinyData$pos=="RB"] <- rank(-shinyData$projectedPts[shinyData$pos=="RB"], ties.method="min")
    shinyData$positionRank[shinyData$pos=="WR"] <- rank(-shinyData$projectedPts[shinyData$pos=="WR"], ties.method="min")
    shinyData$positionRank[shinyData$pos=="TE"] <- rank(-shinyData$projectedPts[shinyData$pos=="TE"], ties.method="min")
    
    #Calculate Overall Rank
    shinyData$overallRank <- rank(-shinyData$projectedPts, ties.method="min")
    
    #Calculate projected cost
    shinyData$projectedCost[shinyData$overallRank <= 33] <- ceiling(shinyData$cost[shinyData$overallRank <= 33] * (input$leagueCap/200) * 1.1)
    shinyData$projectedCost[shinyData$overallRank >= 34 & shinyData$overallRank <= 66] <- ceiling(shinyData$cost[shinyData$overallRank >= 34 & shinyData$overallRank <= 66] * (input$leagueCap/200) * 1.0)
    shinyData$projectedCost[shinyData$overallRank >= 67] <- ceiling(shinyData$cost[shinyData$overallRank >= 67] * (input$leagueCap/200) * 0.9)
    shinyData$projectedCost[is.na(shinyData$projectedCost)==TRUE] <- 1
    shinyData$projectedCost[shinyData$projectedCost==0] <- 1
    
    return(shinyData)
  })
  
  #Determine number of starters at each position
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
    input$numTEs
  })
  
  maxTEs <- reactive({
    input$numTEs + input$numWRTEs + input$numWRRBTEs + input$numQBWRRBTEs
  })
  
  #Calculate total number of starters
  numStarters <- reactive({
    input$numQBs + input$numRBs + input$numWRs + input$numTEs + input$numWRTEs + input$numWRRBs + input$numWRRBTEs + input$numQBWRRBTEs
  })
  
  #Calculate cap available for bidding on starters (assumes $1 for each bench player)
  capAvailable <- reactive({
    input$leagueCap - (input$numTotalPlayers - numStarters())
  })
  
  #Determine maximum risk of individual players
  maxRisk <- reactive({
    input$maxRisk
  })
  
  #Calculate best team
  
  num.players <- reactive({
    length(newData()$name)
  })
  
  var.types <- reactive({
    rep("B", num.players())
  })
  
  A <- reactive({
    rbind(as.numeric(newData()$pos == "QB"),
          as.numeric(newData()$pos == "QB"),
          as.numeric(newData()$pos == "RB"),
          as.numeric(newData()$pos == "RB"),
          as.numeric(newData()$pos == "WR"),
          as.numeric(newData()$pos == "WR"),
          as.numeric(newData()$pos == "TE"),
          as.numeric(newData()$pos == "TE"),
          diag(newData()$risk),                 # player's risk
          newData()$projectedCost,              # total cost
          rep(1,num.players()))                 # num of players in starting lineup   
  })
  
  dir <- reactive({
    c(">=",
      "<=",
      ">=",
      "<=",
      ">=",
      "<=",
      ">=",
      "<=",
      rep("<=", num.players()),
      "<=",
      "==")
  })
  
  b <- reactive({
    c(minQBs(),
      maxQBs(),
      minRBs(),
      maxRBs(),
      minWRs(),
      maxWRs(),
      minTEs(),
      maxTEs(),
      rep(maxRisk(), num.players()),
      capAvailable(),
      numStarters())
  })
  
  bestTeam <- reactive({
    sol <- Rglpk_solve_LP(obj = newData()$projectedPts, mat = A(), dir = dir(), rhs = b(),types = var.types(), max = TRUE)
    sol$playerInfo <- as.data.frame(merge(newData()[newData()$name %in% newData()[sol$solution == 1,"name"],c("name","pos","team")], newData()[sol$solution == 1,c("name","projectedPts","risk","projectedCost")], by="name"))
    sol$playerInfo[,"projectedCost"] <- as.integer(sol$playerInfo[,"projectedCost"])
    sol$totalCost <- sum(newData()$projectedCost * sol$solution)
    sol$players <- as.character(newData()$name[sol$solution == 1])
    
    return(sol)
  })
  
  #Output: best team
  output$bestTeam <- renderTable({
    bestTeam()$playerInfo
  })
  
  #Output: sum of projected points
  output$totalPoints <- renderText({
    paste("Projected Points: ", bestTeam()$optimum)
  })
  
  #Output: sum of projected cost
  output$totalCost <- renderText({
    paste("Total Cost: ", bestTeam()$totalCost)
  })
  
  #Output: cap available (max cost) for starters
  output$capAvailable <- renderText({
    paste("Max Cost: ", capAvailable())
  })
  
  #Output: Number of starters
  output$numStarters <- renderText({
    paste("Number of Starters: ", numStarters())
  })
  
  #Output: Download data
  output$downloadData <- downloadHandler(
    filename = function() { "projections.csv" },
    content = function(file) {
      write.csv(newData(), file)
    }
  )
  
  #Output: Drafted
  output$Drafted <- renderText({
    paste("Drafted: ", input$drafted)
  })
  
})