#' @title Add a flow-based typical day id column
#' 
#' @description Add to an Antares output table a column indicating the id of 
#' the used flow-based typical day for each timestep.
#' This information is read in the "user" directory file of the Antares study: 
#' it must be made sure that the data of the study is still
#' consistent with the output.
#' 
#' @param data \code{antaresdata} output data load by \link{readAntares}
#' @param fb_opts \code{list} of simulation parameters returned by the function 
#' \link{setSimulationPath} or fb model 
#' localisation obtain with \link{setFlowbasedPath}. 
#' The default value is indicated by \code{antaresRead::simOptions()}.
#' 
#' 
#' @examples
#'
#' \dontrun{
#' # Change the study path for the path of a study you have on your computer
#' study <- "../../Pour Julien/blop/MT_base_nucM2_2023"
#' antaresRead::setSimulationPath(study, 17)
#' data <- antaresRead::readAntares(mcYears = 1:2)
#' data <- addTypicalDayId(data)
#' 
#' 
#' data <- antaresRead::readAntares(areas = "all", links = "all", clusters = "all" ,mcYears = 1:2)
#' data <- addTypicalDayId(data)
#' 
#' #' #### Example with the virtual area zz_flowbased
#' ## If you gave virtualFBarea = TRUE in the function \link{computeFB},
#' ## the areas and links are built with a virtual area on the center.
#' 
#' # Change the study path for the path of a study you have on your computer
#' study <- "../Etude Antares/BP19_costs18_FB18_2023Virtual/"
#'
#' opts <- antaresRead::setSimulationPath(study, 5)
#' 
#' data <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl", "at", "zz_flowbased"),
#'                                 links = c("be - zz_flowbased", 
#'                                 "fr - zz_flowbased", "nl - zz_flowbased",
#'                                 "de - zz_flowbased", "at - zz_flowbased"), mcYears = 1:2,
#'                                 select = c("LOLD", "UNSP. ENRG",
#'                                 "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#'                                 opts = opts)           
#' 
#' data <- addTypicalDayId(data)
#' }
#' 
#' @export
addTypicalDayId <- function(data, fb_opts = antaresRead::simOptions()){
  
  # .ctrlUserHour(opts)

  
  simulation <- Date <- time <- NULL
  if(!"antaresData" %in%class(data)){
    warning(paste0("Your data are not antaresData object, antaresData objetc are object load by readAntares. If you have 
                   write your data in a csv file and you reload them after this is break the sytem of antaresData class.
                   You can write them in a .rds file, this system conserve antaresData class. For this straitement we will try to merge your object but 
                   this may cause a bug "))
  }
  
  
  if(attributes(data)$synthesis){
    stop("You can merge typical day with mcAll")
  }
  
  foldPath <- .mergeFlowBasedPath(fb_opts)
  if(!file.exists(paste0(foldPath, "scenario.txt"))){
    stop(paste0("The file scenario.txt is missing. Please either: add it to your flow-based model directory and use setFlowBasedPath(path = 'pathToDirectory') or
                use setFlowBasedPath(path = 'pathToAntaresStudy/user/flowbased')"))
  }
  scenario <- fread(paste0(foldPath, "scenario.txt"))
  ts <- fread(paste0(foldPath, "ts.txt"), header = TRUE)

  
  tsTransform <- rbindlist(sapply(2:ncol(ts), function(X){
    oo <- ts[, .SD, .SDcols = c(1, X)]
    oo$simulation <- X - 1
    colnames(oo)[2] <- "typicalDay"
    oo
  }, simplify = FALSE))
  
  scenario$mcYear <- 1:nrow(scenario)
  out <- merge(tsTransform, scenario, by = "simulation", allow.cartesian = TRUE)
  
  out[,simulation := NULL]

  if("antaresDataList" %in% class(data))
  {
    key <- unique(data[[1]][, .SD, .SDcols = c("time", "mcYear")])
  }else{
    key <- unique(data[, .SD, .SDcols = c("time", "mcYear")])
    
  }
  
  key[,Date := as.Date(time)]
  
  key
  key$Date <- substr(key$Date, 6, 10)
  out$Date <- substr(out$Date, 6, 10)
  out2 <- merge(key, out, by = c("Date", "mcYear"))
  # setnames(out2, "1", "typicalDay")
  out2[,Date := NULL]
  
  if("antaresDataList" %in% class(data)){
    for(i in 1:length(data)){
      data[[i]] <- merge(data[[i]], out2, by = c("time", "mcYear"))
    }
  }else{
    data <- merge(data, out2, by = c("time", "mcYear"))
    
  }
  data
  
}