# Create Scoring UI for the projections gadget
defaultScoring = list(
  QB = c(passYds = 0.04, passAtt = 0, passComp  = 0, passInc = 0, passTd = 4,
         passInt = -3, pass40 = 0, pass300 = 0, pass350 = 0, pass400 = 0,
         rushYds = 0.1, sacks = 0, rushAtt = 0, rush40 = 0, rushTds = 6,
         twoPts = 2, fumbles = -3),

  RB = c(rushYds = 0.1, rushAtt = 0, rushTds = 6, rush40 = 0, rush100 = 0,
         rush150 = 0, rush200 = 0, rec = 0, recYds = 0.1, recTds = 6, rec40 = 0,
         returnYds = 0, returnTds = 6, twoPts=2 , fumbles = -3),

  WR = c(rushYds = 0.1, rushAtt = 0, rushTds = 6, rush40 = 0, rush100 = 0,
         rush150 = 0, rush200 = 0, rec = 0, recYds = 0.1, recTds = 6, rec40 = 0,
         returnYds = 0, returnTds = 6, twoPts=2 , fumbles = -3),

  TE = c(rushYds = 0.1, rushAtt = 0, rushTds = 6, rush40 = 0, rush100 = 0,
         rush150 = 0, rush200 = 0, rec = 0, recYds = 0.1, recTds = 6, rec40 = 0,
         returnYds = 0, returnTds = 6, twoPts=2 , fumbles = -3),

  K = c(xp = 1, fg0019 = 3, fg2029 = 3, fg3039 = 3, fg4049 = 4, fg50 = 5, fgMiss = 0),
  DST = c(dstFumRec = 2, dstInt = 2, dstSafety = 2, dstSack = 1, dstTd = 6,
          dstBlk = 1.5, dstReturnYds = 0, dstPtsAllow = 0),
  DL = c(idpSolo = 1, idpAst = 0.5, idpSack = 2, idpInt = 3, idpForceFum = 3,
         idpFumRec = 2 , idpPD = 1, idpTd =6, idpSafety = 2),
  LB = c(idpSolo = 1, idpAst = 0.5, idpSack = 2, idpInt = 3, idpForceFum = 3,
         idpFumRec = 2 , idpPD = 1, idpTd =6, idpSafety = 2),
  DB = c(idpSolo = 1, idpAst = 0.5, idpSack = 2, idpInt = 3, idpForceFum = 3,
         idpFumRec = 2 , idpPD = 1, idpTd =6, idpSafety = 2)
)

ptsBracket <- data.table::data.table(threshold = c(0, 6, 20, 34, NA, NA, NA, NA, NA, 99),
                                     points = c(10, 7, 4, 0, NA, NA, NA, NA, NA,-4))

scoringUI <- function(positions){

  scoringPositions <- positions
  numPanels <- length(positions)
  numRows <- ceiling(numPanels / 4)
  lapply(1:numRows, function(r){
    fluidRow(tagList(
      lapply(scoringPositions[c((1+ (r-1)*4):min(4*r,numPanels))], function(sp){
        scoringVars <- names(defaultScoring[[sp]])
        column(3, tags$div(class = "panel panel-default",
                           tags$div(sp, class = "panel-heading"),
                           tags$div(class = "panel-body",
                                    lapply(scoringVars, function(sv){

                                      textInput(paste0(sp, "_", sv), label = sv,
                                                value = defaultScoring[[sp]][[sv]])

                                    }))))
      }),
      if(any(scoringPositions == "DST" & r == numRows)){
        column(4,
               tags$div(class = "panel panel-default",
                        tags$div("DST bracket", class = "panel-heading"),
                        tags$div(class = "panel-body",
                                 lapply(1:nrow(ptsBracket), function(br){
                                   fluidRow(column(6, textInput(paste0("limit", br),
                                                                label = "Lower Than ",
                                                                value = ptsBracket[br]$threshold)),
                                            column(6, textInput(paste0("points", br),
                                                                label = "Points ",
                                                                value = ptsBracket[br]$points)))

                                 })
                        ))

        )
      }
    ))})

}


vorUI <- function(positions){
  numPanels <- length(positions)
  numRows <- ceiling(numPanels / 4)
  lapply(1:numRows, function(r){
    fluidRow(tagList(
  lapply(positions[c((1+ (r-1)*4):min(4*r,numPanels))], function(p){
    column(3,
           tags$div(class = "panel panel-default",
                    tags$div(paste(p, "Value"), class = "panel-heading"),
                    tags$div(class = "panel-body",
                             textInput(paste0(p, "_vor"), label = "Baseline",
                                       value = ffa.vorBaseline[[p]]),
                             radioButtons(paste0(p, "_vorType"),label = "Type",
                                                 choices = c("Rank", "Points"),
                                                 selected = "Rank", inline = TRUE ))

                    ))
  })))
  })
}



