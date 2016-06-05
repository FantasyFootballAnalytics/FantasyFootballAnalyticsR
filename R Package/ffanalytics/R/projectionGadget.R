#' @export Run_Projection
Run_Projection <- function(){
  curYear <- as.POSIXlt(Sys.Date())$year + 1900
  weekList <- 0:17
  names(weekList) <- c("Season", paste("Week", 1:17))
  fbgs <- analysts[siteId == sites[siteName == "Footballguys"]$siteId]$analystId
  ui <-miniPage(
    gadgetTitleBar("Get Projections"),
    miniTabstripPanel(
      miniTabPanel("Scrape Settings", icon = icon("bars"),
                   miniContentPanel(
                     fillCol(flex = c(1,9),
                             fillRow(
                               selectInput("scrapeSeason", "Season", 2008:curYear,
                                           selected = curYear, width = "90%"),
                               selectInput("scrapeWeek", "Week",weekList, selected = 0, width = "90%"),
                               "",""),

                             fillRow(
                               fillCol(flex = c(1,10),
                                       miniButtonBlock(actionButton("allAnalyst", "All"),
                                                       actionButton("nonSubs","Free"),
                                                       actionButton("noAnalyst", "None")),
                                       uiOutput("avail_analysts")),
                               fillCol(flex = c(1,5,5),
                                       miniButtonBlock(actionButton("allPosition", "All"),
                                                       actionButton("offPosition", "Offense"),
                                                       actionButton("nonIdpPosition", "Non-IDP"),
                                                       actionButton("noPosition", "None")),
                                       checkboxGroupInput("selectPositions", "Select Positions",
                                                          position.name),
                                       uiOutput("fbg_cred"))))
                   )
      ),
      miniTabPanel("Scoring Settings",  icon = icon("sliders"),
                   miniContentPanel(
                     fillRow(uiOutput("scoring")))
      ),
      miniTabPanel("Calculation Settings",  icon = icon("cogs"),
                   miniContentPanel(
                     fillCol(flex = c(1,1,8),
                             fillRow(flex = c(2,2,4),
                                     numericInput("numTeams", "Teams", 12,
                                                  min = 8, max = 20, step = 1,
                                                  width = "70%"),
                                     selectInput("leagueType", "Format",
                                                 choices = c("Standard", "PPR"),
                                                 width = "80%"),
                                     checkboxGroupInput("adp", "ADP sources",
                                                        c("CBS", "ESPN", "FFC", "MFL", "NFL", "Yahoo", "All"),
                                                        inline = TRUE)),
                             fillRow(selectInput("averageType", "Average",
                                                 choices = c("Average", "Robust", "Weighted"),
                                                 width = "95%"),
                                     selectInput("mockMFL", "MFL Draft Types",
                                                 choices = c(All = -1, "Real Drafts" = 0,
                                                             "Mock Drafts" = 1), width = "95%"),
                                     selectInput("leagueMFL", "MFL League Types",
                                                 choices = c(All = -1, "Redraft Leagues" = 0,
                                                             "Keeper League" = 1, "Rookie League" = 2,
                                                             "Public Leagues" = 3), width = "95%")
                             ),
                             fillRow(uiOutput("vorData"))
                             ))
      )
    )
  )

  server <- function(input, output, session){

    scrapePeriod <- reactive(dataPeriod(weekNo = as.numeric(input$scrapeWeek),
                                        season = as.numeric(input$scrapeSeason)))

    output$avail_analysts <- renderUI({
      analyst_list <- analystOptions(scrapePeriod())
      checkboxGroupInput("selectAnalyst", "Select Analysts", analyst_list,
                         selected = NULL)
    })
    output$fbg_cred <- renderUI({
      req(input$selectAnalyst)
      selectedAnalysts <- input$selectAnalyst
      if(any(fbgs %in% selectedAnalysts)){
        inp <- tags$div(
          textInput("fbgUser", "Footballguys User Name"),
          passwordInput("fbgPwd","Footballguys Password")
        )
        return(inp)
      }
    })


    availPositions <- reactive({
      analystCheck <- input$selectAnalyst
      week <- input$scrapeWeek
      analystPos <- analystPositions[analystId %in% analystCheck]
      if(week == 0){
        analystPos <- analystPos[season == 1]
      } else {
        analystPos <- analystPos[weekly == 1]
      }

      posList <- intersect(position.name, unique(analystPos$position))
      posList
    })

    observeEvent(input$selectAnalyst,{
      updateCheckboxGroupInput(session, "selectPositions", choices = availPositions())
    })

    observeEvent(input$scrapeWeek,{
      updateCheckboxGroupInput(session, "selectPositions", choices = availPositions())
    })

    output$scoring <- renderUI(scoringUI(input$selectPositions))
    output$vorData <- renderUI(vorUI(input$selectPositions))

    observeEvent(input$allAnalyst, {
      allAnalysts <-analystOptions(scrapePeriod())
      updateCheckboxGroupInput(session, "selectAnalyst",
                               selected = as.character(allAnalysts))
    })

    observeEvent(input$nonSubs, {
      allAnalysts <-analystOptions(scrapePeriod())
      subSites <- sites[subscription == 1]
      freeAnalysts <- analysts[!(siteId %in% subSites$siteId)]
      freeAnalysts <- intersect(freeAnalysts$analystId, allAnalysts)
      updateCheckboxGroupInput(session, "selectAnalyst",
                               selected = as.character(freeAnalysts))
    })
    observeEvent(input$noAnalyst, {
      updateCheckboxGroupInput(session, "selectAnalyst", selected = character(0))
    })

    observeEvent(input$allPosition, {
      updateCheckboxGroupInput(session, "selectPositions", selected = position.name)

    })

    observeEvent(input$offPosition, {
      updateCheckboxGroupInput(session, "selectPositions", selected = c("QB", "RB", "WR", "TE"))
    })

    observeEvent(input$nonIdpPosition, {
      updateCheckboxGroupInput(session, "selectPositions", selected = c("QB", "RB", "WR", "TE", "K", "DST"))
    })

    observeEvent(input$noPosition, {
      updateCheckboxGroupInput(session, "selectPositions", selected = "")
    })



    observeEvent(input$adp, {
      if(any(input$adp == "All")){
        updateCheckboxGroupInput(session, "adp", selected =  c("CBS", "ESPN", "FFC", "MFL", "NFL", "Yahoo", "All"))
      }
    })

    getScoringRules <- function(positions){
      scoringTables <- lapply(positions, function(p){
        scoringVars <- names(defaultScoring[[p]])
        multipliers <- lapply(scoringVars, function(sv){
          multVar <- paste0(p, "_", sv)
          return(as.numeric(input[[multVar]]))
        })

        scoreTable <- data.table::data.table(dataCol = scoringVars,
                                             multiplier = multipliers)
        return(scoreTable)
      })
      names(scoringTables) <- positions

      if(any(positions == "DST")){
        dstBracket <- ptsBracket
        for(r in 1:nrow(dstBracket)){
          if(!is.na(input[[paste0("limit", r)]])){
            dstBracket[r, c("threshold", "points") := list(as.numeric(input[[paste0("limit", r)]]),
                                                           as.numeric(input[[paste0("points", r)]]))]
          }
        }
      scoringTables$ptsBracket <- dstBracket[!is.na(threshold)]
      }
      return(scoringTables)
    }

    getVORbaseline <- function(positions){
      vorPos <- intersect(position.name, positions)
      vorValues <- unlist(lapply(vorPos, function(p)as.numeric(input[[paste0(p, "_vor")]])))
      names(vorValues) <- vorPos
      return(vorValues)
    }

    getVORtypes <- function(positions){
      vorPos <- intersect(position.name, positions)
      vorTypes <- unlist(lapply(vorPos, function(p)input[[paste0(p, "_vorType")]]))
      names(vorTypes) <- vorPos
      return(vorTypes)
    }


    getTierD <- function(positions){
      tierPos <- intersect(position.name, positions)
      tierD <- unlist(lapply(tierPos, function(p)as.numeric(input[[paste0(p, "_dval")]])))
      names(tierD) <- tierPos
      return(tierD)
    }

    observeEvent(input$done,{
      analystVector <- "NULL"
      positionVector <- "NULL"
      adpVector <- "NULL"
      fbg.user <- "NULL"
      fbg.pwd <- "NULL"

      if(!is.null(input$selectAnalyst))
        analystVector <- paste0("c(", paste(input$selectAnalyst, collapse = ", "), ")")
      if(!is.null(input$selectPositions))
        positionVector <- paste0("c(\"", paste(input$selectPositions, collapse = "\", \""), "\")")
      if(!is.null(input$adp))
        adpVector <- paste0("c(\"", paste(input$adp[input$adp != "All"], collapse = "\", \""), "\")")

      scrapeCode <- paste0("runScrape(week = ", input$scrapeWeek,
                           ", season = ", input$scrapeSeason,
                           ", analysts = ", analystVector,
                           ", positions = ", positionVector)
      if(length(input$fbgUser) == 0){
        scrapeCode <- paste0(scrapeCode, ")")
      } else {
        if(nchar(input$fbgUser) > 0){
          scrapeCode <- paste0(scrapeCode,
                               ", fbgUser = \"", input$fbgUser,
                               "\", fbgPwd = \"", input$fbgPwd, "\")")
        } else {
          scrapeCode <- paste0(scrapeCode, ")")
        }
      }

      if(!is.null(input$fbgUser))
        fbg.user <- paste0("\"", input$fbgUser, "\"")

      if(!is.null(input$fbgUser))
        fbg.pwd <- paste0("\"", input$fbgPwd, "\"")

      userScoring <<- getScoringRules(input$selectPositions)
      vorBaseline <<- getVORbaseline(input$selectPositions)
      vorType <<- getVORtypes(input$selectPositions)
      tierDValues <<- getTierD(input$selectPositions)
      rCode <- paste0("getProjections(scrapeData=", scrapeCode ,
                      ", avgMethod = \"", tolower(input$averageType),
                      "\", leagueScoring = userScoring, vorBaseline, vorType",
                      ", teams = ", input$numTeams,
                      ", format = \"", tolower(input$leagueType),
                      "\", mflMocks = ", input$mockMFL,
                      ", mflLeagues = ", input$leagueMFL,
                      ", adpSources = ", adpVector,
                      ", fbgUser = ", fbg.user,
                      ", fbgPwd = ", fbg.pwd,
                      ")"

      )
      rstudioapi::insertText(rCode, id = "#console")
      stopApp()
    }
    )

  }
  runGadget(ui, server, viewer = dialogViewer("Calculate Projections", height = 1100, width = 800))
}

