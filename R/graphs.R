#' Plot 2D for flowbased areas
#'
#' @param flowbased \code{list}, flowbased outout obtain which computeFB function
#' @param ctry1 \code{character}, country in X
#' @param ctry2 \code{character}, country in Y
#' @param hour \code{numeric}, hour default is NULL, if NULL the function takes the first
#' line of flowbased, have to be of length 1
#' @param dayType \code{numeric}, dayType default is NULL, if NULL the function takes the first
#' line of flowbased, have to be of length 1
#' @param xlim \code{numeric}, limits of x-axis
#' @param ylim \code{numeric}, limits of y-axis
#'
#' @import rAmCharts
#' @importFrom grDevices chull
#'
#' @noRd
graphFlowBased2D <- function(flowbased, ctry1, ctry2, hour = NULL, dayType = NULL, 
                             xlim = c(-10000, 10000), ylim = c(-10000, 10000))
{
  
  Period <- idDayType <- VERTDetails <- VERTRawDetails <- NULL
  
  if(is.null(hour)){
    hour <- flowbased$Period[1]
  }
  # browser()


  if(is.null(dayType)){
    dayType <- flowbased$idDayType[1]
  }
  
  if (grepl("ptdf", ctry1)) {
    ctry1 <- gsub("ptdf", "", ctry1)
  } else {
    ctry1 <- as.character(ctry1)
  }
  if (grepl("ptdf", ctry2)) {
    ctry2 <- gsub("ptdf", "", ctry2)
  } else {
    ctry2 <- as.character(ctry2)
  }
  if(ctry1 == ctry2) {
    stop("The hubs should be distinct")
  }
  
  hubnames <- colnames(flowbased$PTDFRawDetails[[1]])
  hubnames <- gsub("ptdf", "", hubnames[grepl("ptdf", hubnames)])
  if (!(ctry1 %in% hubnames) |
      !(ctry2 %in% hubnames)) {
    stop(paste("country1 or country 2 has wrong format. Format should be",
               "XX (where XX is the abreviation of the hub (ex : FR, DE, BE))"))
  }
  hubnames_vert <- colnames(flowbased$PTDFDetails[[1]])
  hubnames_vert <- gsub("ptdf", "", hubnames_vert[grepl("ptdf", hubnames_vert)])
  hubnameDiff <- hubnames[!(hubnames %in% hubnames_vert)]
  
  # data_stud <- copy(flowbased)
  # if(!is.null(hour)){
  #   data_stud <- data_stud[Period == hour]
  # }
  # if(!is.null(dayType)){
  #   data_stud <- data_stud[idDayType == dayType]
  # }
  
  
  dtModel <- .getChull(flowbased[Period == hour & idDayType == dayType, VERTDetails][[1]], 
                       ctry1, ctry2, hubnameDiff)
  dtReal <- .getChull(flowbased[Period == hour & idDayType == dayType, VERTRawDetails][[1]], 
                      ctry1, ctry2, hubnameDiff)

  max_r <- max(nrow(dtModel), nrow(dtReal))
  if(nrow(dtModel) < max_r){
    dtModel <- rbind(dtModel, data.frame(ctry1 = rep(NA, max_r-nrow(dtModel)),
                                 ctry2 = rep(NA, max_r-nrow(dtModel))))
  }
  if(nrow(dtReal) < max_r){
    dtReal <- rbind(dtReal, data.frame(ctry1 = rep(NA,max_r- nrow(dtReal)),
                                   ctry2 = rep(NA, max_r-nrow(dtReal))))
  }
  
  out <- cbind.data.frame(dtModel, dtReal)
  colnames(out) <- c(paste0("Model", ctry1), paste0("Model", ctry2),  paste0("Real", ctry1),  paste0("Real", ctry2))
  
  out <- round(out, 2)
  
  hour <- paste0(" Hour ", hour)

  dayType <- paste0(" Typical day ", dayType)

  pipeR::pipeline(
    amXYChart(dataProvider = out),
    addTitle(text = paste0("Flow-based ", ctry1, "/", ctry2, hour, dayType)),
    addGraph(title = "Model", balloonText =
               paste0('<b>Model<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),
             
             bullet = 'circle', xField = names(out)[1],yField = names(out)[2],
             lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#FF0000",
             lineThickness = 1),
    addGraph(title = "Real",balloonText =
               paste0('<b>Real<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),
             bullet = 'circle', xField = names(out)[3],yField = names(out)[4],
             lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#0000FF",
             lineThickness = 1,  dashLength = 7),
    setChartCursor(),
    addValueAxes(title = paste(ctry1, "(MW)"), position = "bottom", minimum = xlim[1], maximum = xlim[2]),
    addValueAxes(title =  paste(ctry2, "(MW)"), minimum = ylim[1], maximum = ylim[2]),
    setExport(enabled = TRUE),
    setLegend(enabled = TRUE)
  )
  
}



.getChull <- function(data, country1, country2, hubnameDiff){
  
  # remove NOTE data.table
  chull <- NULL

  data <- data.frame(data)
  if(country1 == hubnameDiff){
    ctry1 <- -rowSums(data[!grepl("Date|Period|N|nbsign|sign", colnames(data))])
  }else{
    ctry1 <- data[[country1]]
  }
  if(country2 == hubnameDiff){
    ctry2 <- -rowSums(data[!grepl("Date|Period|N|nbsign|sign", colnames(data))])
  }else{
    ctry2 <- data[[country2]]
  }
  res <- cbind(ctry1, ctry2)
  res <- res[chull(res),]
  res <- rbind(res, res[1,])
  res
}


#' @title Generate html report on a typical flow-based day
#' 
#' @description This function generates an html report on one or several typical days, 
#' comparing the real and modelled domains. It hence can follow the use of the 
#' function \link{computeFB}. The report (one per day) is composed of several tabs: a 
#' summary of the volumetric errors (called inf and sup, representing real points 
#' forgotten in the model and modelled pointsmissing from the real domain) and 
#' plots for each hour of the real and modelled domains.
#'
#' @param fb_opts \code{list} of flowbased parameters (directory of the flow-based input) 
#' returned by the function \link{setFlowbasedPath}. By default, the value is 
#' indicated by \code{antaresFlowbased::fbOptions()}
#' @param output_file \code{character}, output directory of the html reports. 
#' By default, the value is \code{NULL}, the reports will be written in the current directory.
#' @param dayType \code{numeric}, numerical id of the chosen typical flow-based days
#' @param allFB \code{data.table}, table of flow-based domains (real and modelled) 
#' returned by the function \link{computeFB}. By default, the value is \code{NULL}: 
#' in this case, the flow-based data is directly read in the model designated by 
#' the parameter fb_opts.
#'
#' @import rmarkdown flexdashboard rAmCharts manipulateWidget
#'
#' @examples
#'
#' \dontrun{
#' #Generate report for the typical day 7 of a model (already designated by setFlowBasedPath)
#' generateReportFb(dayType = 7, fb_opts = antaresFlowbased::fbOptions())
#' 
#' #Generate a report for the typical day 7 of a PTDF file
#' allFB <- computeFB(PTDF = "/path/PTDF_file.csv",reports = FALSE, dayType = 7)
#' generateReportFb(dayType = 7, fb_opts = antaresFlowbased::fbOptions(), allFB = allFB)
#' }
#' @export
generateReportFb <- function(dayType, output_file = NULL,
                             fb_opts =NULL,
                             allFB = NULL){
  
  fb_opts <- Period <- idDayType <- VERTDetails <- VERTRawDetails <- NULL
  if(is.null(allFB)){
    allFB <- readRDS(paste0(fb_opts$path, "/domainesFB.RDS"))
  }
  
  dayType2 <- dayType
  if(is.null(output_file)){
    output_file <- getwd()
  }
  output_Dir <- output_file
  output_file <- paste0(output_file, "/", "FlowBased_TD",dayType, "_", Sys.Date(), ".html")
  e <- environment()
  e$dayType <- dayType
  e$dta <- allFB[dayType == dayType2]
  
  rmarkdown::render(system.file("/report/resumeFBflex.Rmd", package = "fbAntares"),
                    output_file = output_file,
                    params = list(set_title = paste0(
                      "Typical Day ", dayType, " (generated on ", Sys.Date(), ")")),
                    intermediates_dir = output_Dir, envir = e,
                    quiet = TRUE)
}
