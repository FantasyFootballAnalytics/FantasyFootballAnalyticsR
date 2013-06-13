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
  
  #Removed drafted players
  drafted <- reactive({
    c(input$yourDrafted, input$otherDrafted)
  })
  
  removedPlayers <- reactive({
    newData()[! newData()$name %in% drafted(),]
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
  
  #Determine drafted team
  draftedTeam <- reactive({
    newData()[newData()$name %in% input$yourDrafted,]
  })
  
  #Calculate how many players to draft at each position
  minQBsToDraft <- reactive({
    minQBs() - sum(draftedTeam()$pos == "QB")
  })
  
  maxQBsToDraft <- reactive({
    maxQBs() - sum(draftedTeam()$pos == "QB")
  })
  
  minRBsToDraft <- reactive({
    minRBs() - sum(draftedTeam()$pos == "RB")
  })
  
  maxRBsToDraft <- reactive({
    maxRBs() - sum(draftedTeam()$pos == "RB")
  })
  
  minWRsToDraft <- reactive({
    minWRs() - sum(draftedTeam()$pos == "WR")
  })
  
  maxWRsToDraft <- reactive({
    maxWRs() - sum(draftedTeam()$pos == "WR")
  })
  
  minTEsToDraft <- reactive({
    minTEs() - sum(draftedTeam()$pos == "TE")
  })
  
  maxTEsToDraft <- reactive({
    maxTEs() - sum(draftedTeam()$pos == "TE")
  })
  
  #Calculate total number of players to draft
  numToDraft <- reactive({
    numStarters() - length(input$yourDrafted)
  })
  
  #Calculate cap available for bidding on starters (assumes $1 for each bench player)
  maxCost <- reactive({
    input$leagueCap - (input$numTotalPlayers - numStarters())
  })
  
  #Calculate remaining cost
  capAvailable <- reactive({
    maxCost() - input$capSpent
  })
  
  #Determine maximum risk of individual players
  maxRisk <- reactive({
    input$maxRisk
  })
  
  #Calculate best team  
  num.players <- reactive({
    length(removedPlayers()$name)
  })
  
  var.types <- reactive({
    rep("B", num.players())
  })
  
  A <- reactive({
    rbind(as.numeric(removedPlayers()$pos == "QB"),
          as.numeric(removedPlayers()$pos == "QB"),
          as.numeric(removedPlayers()$pos == "RB"),
          as.numeric(removedPlayers()$pos == "RB"),
          as.numeric(removedPlayers()$pos == "WR"),
          as.numeric(removedPlayers()$pos == "WR"),
          as.numeric(removedPlayers()$pos == "TE"),
          as.numeric(removedPlayers()$pos == "TE"),
          diag(removedPlayers()$risk),                 # player's risk
          removedPlayers()$projectedCost,              # total cost
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
    c(minQBsToDraft(),
      maxQBsToDraft(),
      minRBsToDraft(),
      maxRBsToDraft(),
      minWRsToDraft(),
      maxWRsToDraft(),
      minTEsToDraft(),
      maxTEsToDraft(),
      rep(maxRisk(), num.players()),
      capAvailable(),
      numToDraft())
  })
  
  bestTeam <- reactive({
    sol <- Rglpk_solve_LP(obj = removedPlayers()$projectedPts, mat = A(), dir = dir(), rhs = b(),types = var.types(), max = TRUE)
    sol$playerInfo <- as.data.frame(merge(removedPlayers()[removedPlayers()$name %in% removedPlayers()[sol$solution == 1,"name"],c("name","pos","team")], removedPlayers()[sol$solution == 1,c("name","projectedPts","risk","projectedCost")], by="name"))
    sol$playerInfo[,"projectedCost"] <- as.integer(sol$playerInfo[,"projectedCost"])
    sol$totalCost <- sum(removedPlayers()$projectedCost * sol$solution)
    sol$players <- as.character(removedPlayers()$name[sol$solution == 1])
    
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
})
