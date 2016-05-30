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
           "rush40", "sacks"),
  "RB" = c("rushAtt", "rushYds", "rushTds", "rec", "recYds", "recTds", "fumbles",
           "twoPts", "rush100", "rush150", "rush200", "returnYds", "regTds",
           "returnTds", "rush40", "rec40"),
  "WR" = c("rec", "recYds", "recTds", "rushAtt", "rushYds", "rushTds", "fumbles",
           "twoPts", "rec100", "rec150", "rec200", "returnYds",
           "regTds", "returnTds", "rush40", "rec40"),
  "TE" = c("rec", "recYds", "recTds", "rushAtt", "rushYds", "rushTds", "fumbles",
           "twoPts", "rec100", "rec150", "rec200", "returnYds", "regTds", "returnTds",
           "rush40", "rec40"),
  "K"  = c("fgAtt", "fgMiss", "fg", "fg0019", "fg2029", "fg3039", "fg4049",
           "fg50", "fg0039", "xp"),
  "DST" = c("dstInt", "dstSack", "dstFumlRec", "dstSafety", "dstTd", "dstBlk",
            "dstRetTd", "dstPtsAllow", "returnYds"),
  "DL" = c("idpSolo", "idpAst", "idpSack", "idpFumlRec", "idpFumlForce", "idpInt",
           "idpPD", "idpTd", "idpTFL"),
  "LB" = c("idpSolo", "idpAst", "idpSack", "idpFumlRec", "idpFumlForce", "idpInt",
           "idpPD", "idpTd", "idpTFL"),
  "DB" = c("idpSolo", "idpAst", "idpSack", "idpFumlRec", "idpFumlForce", "idpInt",
           "idpPD", "idpTd", "idpTFL")
)

