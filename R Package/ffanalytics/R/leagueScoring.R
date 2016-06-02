#' Default scoring rules
#' @export scoringRules
scoringRules <- list(
  QB = data.table::data.table(dataCol = c("passYds", "passTds", "passInt", "rushYds", "rushTds", "twoPts", "fumbles"),
                  multiplier = c(1/25, 4, -3, 1/10, 6, 2, -3 )),
  RB = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                  multiplier = c(1/10, 6, 0, 1/8, 6, 6, 2, -3)),
  WR = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                  multiplier = c(1/10, 6, 0, 1/8, 6, 6, 2, -3)),
  TE = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                  multiplier = c(1/10, 6, 0, 1/8, 6, 6, 2, -3)),
  K = data.table::data.table(dataCol = c("xp", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"),
                 multiplier = c(1,  3, 3, 3, 4, 5)),
  DST = data.table::data.table(dataCol = c("dstFumlRec", "dstInt", "dstSafety", "dstSack", "dstTd", "dstBlk"),
                   multiplier = c(2, 2, 2, 1, 6, 1.5)),
  DL = data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                              multiplier = c(1, 0.5, 2, 3, 3, 2, 1, 6, 2)),
  LB =  data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                                     multiplier = c(1, 0.5, 2, 3, 3, 2, 1, 6, 2)),
  DB = data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                              multiplier = c(1, 0.5, 2, 3, 3, 2, 1, 6, 2)),
  ptsBracket = data.table::data.table(threshold = c(0, 6, 20, 34, 99),
                          points = c(10, 7, 4, 0, -4))
)

#' Default VOR Baseline
#' @export ffa.vorBaseline
ffa.vorBaseline <- c(QB = 13, RB = 35, WR = 36, TE = 10, K= 8, DST = 3, DL = 10, LB = 10, DB = 10)

#' Default VOR Adjustments
#' @export ffa.vorAdjustment
ffa.vorAdjustment <- c(QB = 0, RB = 0, WR = 0, TE = 0, K = 18, DST = 6, DL = 0, B = 0, DB = 0)

#' Default Scoring threshold for tiers
#' @export ffa.scoreThreshold
ffa.scoreThreshold <- c(QB = 20, RB =20, WR = 20, TE = 20, K=10, DST = 10, DL =10, LB = 10, DB = 10)

#' Default number of tiers for clusters
#' @export ffa.tierGroups
ffa.tierGroups <- c(QB = 10, RB = 10, WR = 10, TE = 7, K = 7, DST =5, DL =10, LB = 10, DB =10)

#' @export ffa.vorType
ffa.vorType <- c(QB = "Rank", RB = "Rank", WR = "Rank", TE = "Rank", K = "Rank",
                 DST = "Rank", DL = "Rank", LB = "Rank", DB = "Rank")
