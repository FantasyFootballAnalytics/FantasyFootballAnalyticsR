#' Gadget used to display results of scrape calculations
#' @export dataGadget
dataGadget <- function(inputData){

  ui <- miniPage(
    miniContentPanel(
    selectInput("dataPositions", "Positions:", choices = c("All", intersect(position.name,unique(inputData$position))), selected = "All"),
    DT::dataTableOutput("showData")
    )
  )

  server <- function(input, output, session){
   output$showData <- DT::renderDataTable({
     data <- data.table::copy(inputData)
     if(input$dataPositions == "All"){
       data[, Name := paste0(player, ", ", position, positionRank, " - ", team)]
     } else {
       data <- data[position == input$dataPositions]
       data[, Name := paste0(player, ", ", team)]
     }

     colNames <- c("Name", names(data)[which(names(data) != "Name")])
      numericCols <- c("points",  "lower", "upper", "sdPts", "dropoff", "vor",
                       "adp", "adpDiff" , "sdRank", "risk")
      otherCols <- names(data)[!(which(names(data) %in% numericCols))]

      data <- data[, (numericCols) := lapply(.SD, function(x)ifelse(!is.nan(x) & is.numeric(x), round(x,2),x)),
                   by = otherCols, .SDcols = numericCols]

     data[, colNames, with = FALSE]
     }
     , rownames = FALSE, extensions = "Buttons", selection = "none"
     , colnames = c("Player", "playerName", "position", "team", "Birth Date", "Points", "Lower", "Upper", "Std. Dev. Pts",
                   "Position Rank", "DropOff", "VOR", "Overall Rank", "ADP", "ADP diff", "Experience", "ECR Position",
                    "Std. Dev. Rank", "ECR Overall", "Risk", "Tier"),
     options = list(dom = 'Bfrtip', pageLength = 15,
                    buttons = list(list(extend = 'colvis', columns = c(5:7 ,10, 11, 13:16, 18:20))),
                    columnDefs = list(list(targets = c(1,2,3, 8, 9, 12, 17), visible = FALSE),
                                      list(targets = c(0), width = "20%"))))

  }
  runGadget(ui, server, viewer = dialogViewer("Projected Points", width = 1500, height = 1500))
}
