source("R/externalConstants.R")
analysts <- data.table::data.table(read.csv("data-raw/analysts.csv", stringsAsFactors = FALSE, na.strings = c("NA", "NULL")))
data.table::setnames(analysts,  c("analystQryId", "siteID"), c("sourceId", "siteId"))
analysts[, analystCode := NULL]

analystPositions <- data.table::data.table(read.csv("data-raw/analystPositions.csv", stringsAsFactors = FALSE, na.strings = c("NA", "NULL")))
analystPositions[, position := names(position.Id)[which(position.Id == positionId)], by = "analystPosId"]
analystPositions[, positionId := NULL]

sites <- data.table::data.table(read.csv("data-raw/sites.csv", stringsAsFactors = FALSE, na.strings = c("NA", "NULL")))
data.table::setnames(sites, c("siteID", "playerIdCol"), c("siteId", "playerId"))
sites[, siteCode := NULL]

siteTables <- data.table::data.table(read.csv("data-raw/siteTables.csv", stringsAsFactors = FALSE, na.strings = c("NA", "NULL")))
data.table::setnames(siteTables, c("siteTableId",  "positionQryId"), c("tableId", "positionAlias"))
siteTables[, position := names(position.Id)[which(position.Id == positionId)], by = "tableId"]
siteTables[, positionId := NULL]

siteUrls <- data.table::data.table(read.csv("data-raw/siteUrls.csv", stringsAsFactors = FALSE, na.strings = c("NA", "NULL")))
data.table::setnames(siteUrls, c("urlData", "whichTable", "playerLinkString", "siteID"), c("urlType", "urlTable", "playerLink", "siteId"))
siteUrls[, urlID := NULL]

dataColumns <- data.table::data.table(read.csv("data-raw/dataColumns.csv", stringsAsFactors = FALSE, na.strings = c("NA", "NULL")))
tableColumns <- data.table::data.table(read.csv("data-raw/tableColumns.csv", stringsAsFactors = FALSE, na.strings = c("NA", "NULL")))
data.table::setnames(tableColumns, c("dataColID", "siteTableID"), c("dataColId", "tableId"))
tableColumns <- merge(tableColumns, dataColumns, by = "dataColId")
tableColumns <- tableColumns[, c("tableId", "columnName", "columnType", "columnOrder", "columnPeriod", "removeColumn"), with = FALSE]

tableRowRemove <- data.table::data.table(read.csv("data-raw/tableRowRemove.csv", stringsAsFactors = FALSE, na.strings = c("NA", "NULL")))
data.table::setnames(tableRowRemove, "siteTableId", "tableId")
tableRowRemove[, rowRemoveId := NULL]


devtools::use_data(analysts, analystPositions, sites, siteTables,
                   siteUrls, tableColumns, tableRowRemove, overwrite = TRUE)

nflstats <- data.table::data.table(read.csv("data-raw/nflstats.csv", stringsAsFactors = FALSE))
devtools::use_data(nflstats, overwrite = TRUE, internal = )



