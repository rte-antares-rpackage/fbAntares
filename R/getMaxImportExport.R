#' @title Return a data.table with the maximum values of import and export
#' 
#' @description 
#' This function takes the object domainesFB, generated with \link{computeFB}
#' and return a data.table with the maximum values of import and export by
#' period and typical day.
#'
#' @param domainesFB \code{data.table} The output domainesFB obtained 
#' with \link{computeFB}
#' @param writecsv \code{logical} TRUE if you want to save your results in a csv
#' (default FALSE)
#' 
#' @examples
#' \dontrun{
#' domainesFB <- readRDS(system.file("testdata/antaresInput/domainesFB.rds", package = "fbAntares"))
#' dtImportExport <- getMaxImportExport(domainesFB)
#' }
#' @export

getMaxImportExport <- function(domainesFB, writecsv = F) {
  
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
  
  colnames(dtmaxModel) <- paste0("ExportMaxModel", colnames(dtmaxModel))
  colnames(dtminModel) <- paste0("ImportMinModel", colnames(dtminModel))
  colnames(dtmaxReal) <- paste0("ExportMaxReal", colnames(dtmaxReal))
  colnames(dtminReal) <- paste0("ImportMinReal", colnames(dtminReal))
  
  dtmaxDiff <- dtmaxModel-dtmaxReal
  dtminDiff <- dtminReal-dtminModel
  
  colnames(dtmaxDiff) <- paste0("DiffExportModelReal", colnames(dtmaxDiff))
  colnames(dtminDiff) <- paste0("DiffImportModelReal", colnames(dtminDiff))
  
  dtAll <- cbind(domainesFB[, .SD, .SDcols = c("idDayType", "Period")],
                 dtmaxModel, dtmaxReal, dtmaxDiff, dtminModel, dtminReal, dtminDiff)
  dtAll <- dtAll[, lapply(.SD, round, digits = 0)]
  if (writecsv) {
    fwrite(paste0(Sys.Date(), "_maxImportExport.csv"))
  }
  return(dtAll)
}
