#' Create objects from list
#'
#' Allows creation of class objects based on a list.
#' @param objName Name of object to be created
#' @param targetValues List of values representing slots in the object
#' @export createObject
createObject <- function(objName = as.character(), targetValues = list()){
  targetSlots <- intersect(slotNames(objName), names(targetValues))

  argList <- list(Class = objName)
  argList[targetSlots] <- targetValues[targetSlots]
  return(do.call("new", argList))
}



