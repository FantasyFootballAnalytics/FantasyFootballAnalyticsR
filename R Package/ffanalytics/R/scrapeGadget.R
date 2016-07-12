#' Scrape gadget. Will be used as addin.
#' @export Run_Scrape
Run_Scrape <- function(){
  curYear <- as.POSIXlt(Sys.Date())$year + 1900
  weekList <- 0:17
  names(weekList) <- c("Season", paste("Week", 1:17))
  fbgs <- analysts[siteId == sites[siteName == "Footballguys"]$siteId]$analystId
  ui <- miniPage(
    gadgetTitleBar("Run Data Scrape"),
    miniContentPanel(
      fillCol(flex = c(1,9),
              fillRow(
                selectInput("scrapeSeason", "Season", 2008:curYear,
                            selected = curYear, width = "90%"),
                selectInput("scrapeWeek", "Week",weekList, selected = 0, width = "90%"),
                "", ""),

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
  )

  server <- function(input, output, session){
    scrapePeriod <- reactive(dataPeriod(weekNo = as.numeric(input$scrapeWeek),
                                        season = as.numeric(input$scrapeSeason)))

    output$avail_analysts <- shiny::renderUI({

      scrapePeriod <- dataPeriod(weekNo = as.numeric(input$scrapeWeek),
                                 season = as.numeric(input$scrapeSeason))
      analyst_list <- analystOptions(scrapePeriod)
      analyst_list <- analyst_list[analyst_list != 27]
      if(input$scrapeWeek == 0)
        analyst_list <- analyst_list[analyst_list != 8]
      checkboxGroupInput("selectAnalyst", "Select Analysts", analyst_list)
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
      updateCheckboxGroupInput(session, "selectPositions",
                               selected = c("QB", "RB", "WR", "TE"))
    })

    observeEvent(input$nonIdpPosition, {
      updateCheckboxGroupInput(session, "selectPositions",
                               selected = c("QB", "RB", "WR", "TE", "K", "DST"))
    })

    observeEvent(input$noPosition, {
      updateCheckboxGroupInput(session, "selectPositions", selected = character(0))
    })

    observeEvent(input$done,{
      analystVector <- "NULL"
      positionVector <- "NULL"
      fbg.user <- "NULL"
      fbg.pwd <- "NULL"
      if(!is.null(input$selectAnalyst))
        analystVector <- paste0("c(", paste(input$selectAnalyst, collapse = ", "), ")")
      if(!is.null(input$selectPositions))
        positionVector <- paste0("c(\"", paste(input$selectPositions,
                                               collapse = "\", \""), "\")")
      if(!is.null(input$fbgUser))
        fbg.user <- paste0("\"", input$fbgUser, "\"")

      if(!is.null(input$fbgUser))
        fbg.pwd <- paste0("\"", input$fbgPwd, "\"")

      rCode <- paste0("myScrapeData <- ",
                      "runScrape(week = ", input$scrapeWeek,
                      ", season = ", input$scrapeSeason,
                      ", analysts = ", analystVector,
                      ", positions = ", positionVector,
                      ", fbgUser = ", fbg.user,
                      ", fbgPwd = ", fbg.pwd,
                      ")")

      rstudioapi::insertText(rCode, id = "#console")
      stopApp()
    }
    )
  }
  runGadget(ui, server, viewer = shiny::dialogViewer("Run a scrape", height = 725,
                                                     width = 800))
}

