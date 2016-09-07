#' @export tableMerge
tableMerge <- function(playerTable, dataTable){
  mergeData_1 <- merge(playerTable[, c("playerId", "player", "position", "team"), with = FALSE],
                     dataTable, by = c("player", "position", "team"), all.y = TRUE)

  missingId <- data.table::copy(mergeData_1[is.na(playerId)])
  missingId[, playerId := NULL]

  mergeData_2 <- merge(playerTable[, c("playerId", "player", "position"), with = FALSE],
                     missingId,
                     by = c("player", "position"), all.y = TRUE)

  missingId <- data.table::copy(mergeData_2[is.na(playerId)])
  missingId[, playerId := NULL]

  mergeData_3 <- merge(playerTable[, c("playerId", "player"), with = FALSE],
                       missingId,
                       by = c("player"), all.y = TRUE)


  mergeData <- data.table::rbindlist(
    list(
      mergeData_1[!is.na(playerId)],
      mergeData_2[!is.na(playerId)],
      mergeData_3
    ), fill = TRUE
  )

  return(mergeData)

}
