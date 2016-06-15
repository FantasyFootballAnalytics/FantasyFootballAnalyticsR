#' Constants defining result columns
#'
#' The results in the \linkS4class{dataResult} table are determined by the values in
#' these constants. See definitiions in details.
#' \describe{
#'  \item{staticColumns}{Names of columns in all result tables.}
#'  \item{resultColums}{A list of character vectors (one per position), indicating
#' the columns in the results table for that position.}
#' }
#' @name tableColumns
#' @format NULL
#' @export staticColumns
#' @export resultColumns
staticColumns <- c("playerId", "player", "analyst", "position", "analystId")
#' @rdname tableColumns
#' @format NULL
resultColumns <- list(
  "QB" = c("passAtt", "passComp", "passInc", "passCompPct", "passYds", "passTds",
           "passInt", "rushAtt", "rushYds", "rushTds", "fumbles", "twoPts",
           "pass300", "pass350", "pass400", "regTds", "returnTds",  "pass40",
           "rush40", "sacks", "games"),
  "RB" = c("rushAtt", "rushYds", "rushTds", "rec", "recYds", "recTds", "fumbles",
           "twoPts", "rush100", "rush150", "rush200", "returnYds", "regTds",
           "returnTds", "rush40", "rec40", "games"),
  "WR" = c("rec", "recYds", "recTds", "rushAtt", "rushYds", "rushTds", "fumbles",
           "twoPts", "rec100", "rec150", "rec200", "returnYds",
           "regTds", "returnTds", "rush40", "rec40", "games"),
  "TE" = c("rec", "recYds", "recTds", "rushAtt", "rushYds", "rushTds", "fumbles",
           "twoPts", "rec100", "rec150", "rec200", "returnYds", "regTds", "returnTds",
           "rush40", "rec40", "games"),
  "K"  = c("fgAtt", "fgMiss", "fg", "fg0019", "fg2029", "fg3039", "fg4049",
           "fg50", "fg0039", "xp", "games"),
  "DST" = c("dstInt", "dstSack", "dstFumlRec", "dstSafety", "dstTd", "dstBlk",
            "dstRetTd", "dstPtsAllow", "returnYds", "games"),
  "DL" = c("idpSolo", "idpAst", "idpSack", "idpFumlRec", "idpFumlForce", "idpInt",
           "idpPD", "idpTd", "idpTFL", "games"),
  "LB" = c("idpSolo", "idpAst", "idpSack", "idpFumlRec", "idpFumlForce", "idpInt",
           "idpPD", "idpTd", "idpTFL", "games"),
  "DB" = c("idpSolo", "idpAst", "idpSack", "idpFumlRec", "idpFumlForce", "idpInt",
           "idpPD", "idpTd", "idpTFL", "games")
)

