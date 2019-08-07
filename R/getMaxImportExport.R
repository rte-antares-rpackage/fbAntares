#' @title Return a data.table with the maximum values of import and export
#' 
#' @description 
#' This function takes the object domainesFB, generated with \link{computeFB}
#' and return a data.table with the maximum values of import and export by
#' period and typical day.
#'
#' @param domainesFB \code{data.table} The output domainesFB obtained with \link{computeFB}
#' 
#' @examples
#' \dontrun{
#' domainesFB <- readRDS(system.file("testdata/antaresInput/domainesFB.rds", package = "fbAntares"))
#' dtImportExport <- getMaxImportExport(domainesFB)
#' }
#' @export

getMaxImportExport <- function(domainesFB) {
  
  namesVert <- colnames(domainesFB[, VERTDetails][[1]])
  dtmaxModel <- rbindlist(lapply(1:nrow(domainesFB), function(X) {
    domainesFB[, VERTDetails][[X]][, lapply(.SD, max), .SDcols = namesVert[!grepl(
      "idDayType|Period|Date", namesVert)]]
    
  }))
  dtminModel <- rbindlist(lapply(1:nrow(domainesFB), function(X) {
    domainesFB[, VERTDetails][[X]][, lapply(.SD, min), .SDcols = namesVert[!grepl(
      "idDayType|Period|Date", namesVert)]]
    
  }))
  
  dtmaxReal <- rbindlist(lapply(1:nrow(domainesFB), function(X) {
    domainesFB[, VERTRawDetails][[X]][, lapply(.SD, max), .SDcols = namesVert[!grepl(
      "idDayType|Period|Date", namesVert)]]
    
  }))
  dtminReal <- rbindlist(lapply(1:nrow(domainesFB), function(X) {
    domainesFB[, VERTRawDetails][[X]][, lapply(.SD, min), .SDcols = namesVert[!grepl(
      "idDayType|Period|Date", namesVert)]]
    
  }))
  
  colnames(dtmaxModel) <- paste0("ValueMaxModel", colnames(dtmaxModel))
  colnames(dtminModel) <- paste0("ValueMinModel", colnames(dtminModel))
  colnames(dtmaxReal) <- paste0("ValueMaxReal", colnames(dtmaxReal))
  colnames(dtminReal) <- paste0("ValueMinReal", colnames(dtminReal))
  
  dtmaxDiff <- abs(dtmaxModel-dtmaxReal)
  dtminDiff <- abs(dtminModel-dtminReal)
  
  colnames(dtmaxDiff) <- paste0("DiffMaxModelReal", colnames(dtmaxDiff))
  colnames(dtminDiff) <- paste0("DiffMinModelReal", colnames(dtminDiff))
  
  dtAll <- cbind(domainesFB[, .SD, .SDcols = c("idDayType", "Period")],
                 dtmaxModel, dtmaxReal, dtmaxDiff, dtminModel, dtminReal, dtminDiff)
  dtAll <- dtAll[, lapply(.SD, round, digits = 0)]
  return(dtAll)
}
