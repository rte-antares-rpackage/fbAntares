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
#' @param width \code{character}, for rAmCharts only. Default to "420px" 
#' (set to "100/100" for dynamic resize)
#' @param height \code{character}, for rAmCharts only. Default to "410px" 
#' (set to "100/100" for dynamic resize)
#' @param export \code{logical} If you want the possibility to export your graphic
#' if true in rmarkdown, can return a blank html
#'
#' @import rAmCharts
#' @importFrom grDevices chull
#'
#' @noRd
graphFlowBased2D <- function(flowbased, ctry1, ctry2, hour = NULL, dayType = NULL, 
                             xlim = c(-12000, 12000), ylim = c(-12000, 12000),
                             width = "420px", height = "410px", export = T)
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
    addGraph(title = "Real", balloonText =
               paste0('<b>Real<br>', ctry1, '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]'),
             bullet = 'circle', xField = names(out)[3],yField = names(out)[4],
             lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#0000FF",
             lineThickness = 1,  dashLength = 7),
    setChartCursor(),
    addValueAxes(title = paste(ctry1, "(MW)"), position = "bottom", minimum = xlim[1], maximum = xlim[2]),
    addValueAxes(title =  paste(ctry2, "(MW)"), minimum = ylim[1], maximum = ylim[2]),
    setExport(enabled = export),
    setLegend(enabled = TRUE),
    plot(width = width, height = height)
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


#' @title Plot typical flow-based domains
#' 
#' @description 
#' This function enables to plot one or several typical flow-based domains, in 2 dimensions (the axis being 2 countries).
#'
#' @param hour \code{numeric}, hour(s) (can be from 0 to 23 or from 1 to 24 depending on the data of the flow-based model)
#' @param dayType \code{numeric}, numerical id of the typical day(s)
#' @param country1 \code{character}, name of the country (axis X)
#' @param country2 \code{character}, name of the country (axis Y)
#' @param fb_opts \code{list} of flowbased parameters returned by the function \link{setFlowbasedPath} : directory of the flow-based
#' model. By default, the value is indicated by \code{fbAntares::fbOptions()}
#' @param areaName \code{character} The name of the area of your study, possible values are
#' cwe_at (default), cwe and other. If you choose other, you have to give a csv file
#' which explains how your area work.
#' @param xlim \code{numeric}, limits of x-axis
#' @param ylim \code{numeric}, limits of y-axis
#' @param export \code{logical} If you want the possibility to export your graphic
#' (if true in rmarkdown, can return a blank html)
#' @examples
#'
#' \dontrun{
#'  fb_opts <- setFlowbasedPath(path = system.file("input/model/antaresInput/", package = "fbAntares"))
#'  plotFB(dayType = 1, hour = 1, country1 = "FR", country2 = "NL", 
#'  fb_opts = fb_opts, areaName = "cwe_at")
#'  plotFB(dayType = 1, hour = 1:4,country1 = "FR",country2 = "NL", 
#'  fb_opts = fb_opts, areaName = "cwe")
#'  plotFB(dayType = 1, hour = 1:2, country1 = "DE", country2 = "AT", 
#'  fb_opts = fb_opts, areaName = "cwe_at")
#'  plotFB(dayType = 1, hour = 1, country1 = c("FR", "DE"), 
#'      country2 = c("NL", "FR"), fb_opts = fb_opts, areaName = "cwe_at")
#' }
#'
#'
#' @export
plotFB <- function(dayType, hour, country1, country2, 
                   fb_opts = fbAntares::fbOptions(), areaName = "cwe_at",
                   xlim = c(-12000, 12000), ylim = c(-12000, 12000), export = T){
  hoursel <- hour
  dayTypesel <- dayType
  
  dta <- readRDS(paste0(fb_opts$path, "/domainesFB.RDS"))
  
  if(!all(hour%in%(dta$Period - 1))) {
    stop(paste0("Some hour are not in data : ",paste0(hour[!hour%in%(dta$hour - 1)])))
  }
  
  
  if(!all(dayType%in%dta$idDayType)){
    stop(paste0("Some typical day are not in data : ",
                paste0(dayType[!dayType%in%dta$dayType])))
  }
  
  
  ##New version
  areaConf <- .getAreaName(areaName)
  
  nameCheck <- c(areaConf$country[[1]])
  if(!all(country1 %in% nameCheck)){
    stop(paste0("country1 must be", paste(nameCheck, collapse = " ")))
  } 
  if(!all(country2 %in% nameCheck)){
    stop(paste0("country2 must be", paste(nameCheck, collapse = " ")))
  } 
  
  if(length(country1) != length(country2)){
    stop("country1 must be same length to country2")
  }
  
  allCtry <- data.frame(country1 = country1, country2 = country2)

  graphList <- sapply(hour, function(hoursel){
    sapply(dayType, function(dayTypesel){
      apply(allCtry, 1, function(countsel){

        ctsel <- data.frame(t(countsel))
        tempData <- dta[Period == (hoursel) & idDayType == dayTypesel]
        if(length(tempData)==0) {
          stop(paste0("Not available data for typical day ",
                      dayTypesel, " hour ", hoursel))
        }
        graphFlowBased2D(tempData,
                         as.character(ctsel$country1), as.character(ctsel$country2)
                         , dayType = dayTypesel, hour = hoursel, xlim = xlim,
                         ylim = ylim)
      })
    })
  })
  combineWidgets(list = graphList)
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
#' @param countries \code{list, character} a list of couples of countries to choose the axises for the projection
#' of the flowbased domains (ex : list(c("BE", "FR"), c("BE", "NL"))) or an array of countries
#' (ex : c("FR", "NL", "AT")) to project the domains on all the countries combination 
#' (here FR+NL, FR+AT and NL+AT)
#' @param dayType \code{numeric}, numerical id of the chosen typical flow-based days
#' @param allFB \code{data.table}, table of flow-based domains (real and modelled) 
#' returned by the function \link{computeFB}. By default, the value is \code{NULL}: 
#' in this case, the flow-based data is directly read in the model designated by 
#' the parameter fb_opts.
#' @param xlim \code{numeric}, limits of x-axis
#' @param ylim \code{numeric}, limits of y-axis
#' @import rmarkdown flexdashboard rAmCharts manipulateWidget
#'
#' @examples
#'
#' \dontrun{
#' #Generate report for the typical day 7 of a model (already designated by setFlowBasedPath)
#' generateReportFb(dayType = 7, fb_opts = fbAntares::fbOptions())
#' 
#' #Generate a report for the typical day 2 of a PTDF file
#' computeFB(PTDF = "/input/ptdfraw.csv", dayType = 1)
#' domainesFB <- readRDS("antaresInput/domainesFB.rds")
#' generateReportFb(dayType = 7, allFB = domainesFB, 
#'   countries = list(c("BE", "FR"), c("BE", "NL"), c("DE", "FR"), c("DE", "AT")))
#' }
#' @export
generateReportFb <- function(
  dayType, output_file = NULL,
  countries = list(c("BE", "FR"), c("BE", "NL"), c("DE", "FR"), c("DE", "AT")),
  fb_opts = NULL, allFB = NULL, xlim = c(-12000, 12000), ylim = c(-12000, 12000)){
  Period <- idDayType <- VERTDetails <- VERTRawDetails <- NULL
  if(is.null(allFB)){
    allFB <- readRDS(paste0(fb_opts$path, "/domainesFB.RDS"))
    
  }
  combi <- .crtlCountriesCombn(countries)
  
  dayType2 <- dayType
  if(is.null(output_file)){
    output_file <- getwd()
  }
  output_Dir <- output_file
  output_file <- paste0(output_file, "/", "FlowBased_TD",dayType, "_", Sys.Date(), ".html")
  e <- environment()
  e$dayType <- dayType
  e$dta <- allFB[idDayType == dayType2]
  e$countries <- countries
  e$combi <- combi

  e$xlim <- xlim
  e$ylim <- ylim
  rmarkdown::render(system.file("/report/resumeFBflex.Rmd", package = "fbAntares"),
                    output_file = output_file,
                    params = list(set_title = paste0(
                      "Typical Day ", dayType, " (generated on ", Sys.Date(), ")")),
                    intermediates_dir = output_Dir, envir = e,
                    quiet = TRUE)
  
  print(paste("You can find your report here :", output_file))
}



#' @title Run a shiny application to visualize the real and modelled flow-based domains
#' 
#' @description 
#' Run a shiny application displaying the results of the conversion of real domains into Antares models. It will display for
#' each typical day and each hour the volumetric errors of conversion (inf_error: forgotten points in the 
#' model, sup_error: modelled points missing in the real domain) and dynamic plots of the real and modelled domains. Html reports
#' for each day can also be exported.
#'
#' 
#' @param fb_opts \code{list} of flow-based parameters returned by the function 
#' \link{setFlowbasedPath}: directory of the flow-based model to study.
#' By default, the value is indicated by \code{fbAntares::fbOptions()}
#' @param countries \code{list, character} a list of couples of countries to choose the axises for the projection
#' of the flowbased domains (ex : list(c("BE", "FR"), c("BE", "NL"))) or an array of countries
#' (ex : c("FR", "NL", "AT")) to project the domains on all the countries combination 
#' (here FR+NL, FR+AT and NL+AT)
#' @import shiny manipulateWidget
#'
#'
#' @examples
#'
#' \dontrun{
#'  fb_opts = fbAntares::fbOptions()
#'  runAppError(fb_opts, 
#'  countries = list(c("BE", "FR"), c("BE", "NL"), c("DE", "FR"), c("DE", "AT")))
#' }
#' @export
runAppError <- function(
  fb_opts = fbAntares::fbOptions(),
  countries = list(c("BE", "FR"), c("BE", "NL"), c("DE", "FR"), c("DE", "AT"))){
  
  dta <- readRDS(paste0(fb_opts$path, "/domainesFB.RDS"))
  print(dta)
  combi <- .crtlCountriesCombn(countries)
  print(combi)
  G <- .GlobalEnv
  stopifnot(all(c(
    "Period", "idDayType", "PTDFDetails", "PTDFRawDetails", "VERTDetails",
    "VERTRawDetails", "volIntraInter", "error1", "error2") %in% colnames(dta)))
  assign("dtaUseByShiny", dta, envir = G)
  assign("combi", combi, envir = G)
  shiny::runApp(system.file("shinyError", package = "fbAntares"),
                launch.browser = TRUE)
}


#' @title Run a shiny application to visualise the flow-based typical domains and the Net Positions reached within 
#' in an Antares simulation
#' 
#' @description
#' Run a shiny application displaying, after running an Antares simulation, 
#' how the domains have been used by the optimizer to fix the exchanges in the 
#' CWE area. The user can in the application select a period of time on a 
#' calendar, choose to only display some typical days and hours, change the 
#' colors palette, filter unused domains... 
#' (all the parameters of the function \link{plotNetPositionFB}). 
#' It is possible to filter the output data beforehand to only display situations 
#' with loss of load (see examples).
#' 
#'
#' @param dta \code{antaresDataList} Antares output data, imported with \link{readAntares}. 
#' It can be filtered (see examples).
#' @param fb_opts \code{list} of simulation parameters returned by the function
#'   \link{setSimulationPath} or flow-based model directory obtained with
#'   \link{setFlowbasedPath}. By default, the value will be indicated by \code{antaresRead::simOptions()}
#' @param country_list \code{character} Names of the countries used in the study
#' @examples
#'
#' \dontrun{
#' ## Select a study and import the data
#' study <- "../../Pour Julien/blop/MT_base_nucM2_2023"
#' opts <- antaresRead::setSimulationPath(study, 18)
#' dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl", "at"),
#'          links = c("be - de","be - fr","be - nl","de - fr","de - nl", "at - de"), 
#'          select = c("LOLD", "UNSP. ENRG", "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."), 
#'          mcYears = 1, opts = opts)
#' 
#' ## Run the application
#' runAppPosition(dta)
#' 
#' ## Filter the data on situations with unsupplied energy 
#' # If you want to keep only timeId with LOLD!=0 you can't use : 
#' # dta$areas <- dta$areas[LOLD!=0] otherwise some areas
#' # Otherwise some areas are forgotten: the data must be filtered to keep 
#' # all areas at specific timesteps.
#'  
#'  ## An exemple of authorized filter :
#'  idC <- c(antaresRead::getIdCols(dta$areas))
#'  idC <- idC[idC!="area"]
#'  LOLD <- dta$areas[,lapply(.SD, sum), by = idC, .SDcols = "LOLD"]
#'  LOLD <- LOLD[LOLD!=0]
#'  LOLD[,LOLD := NULL]
#'  
#'  # Merge to filter data
#'  dta$areas <- merge(dta$areas, LOLD, by =  idC)
#'  ## End filter
#'
#'  runAppPosition(dta)
#' }
#'
#' @import shiny manipulateWidget
#'
#' @export
runAppPosition <- function(dta, fb_opts = antaresRead::simOptions(),
                           country_list = c("fr", "be", "de", "nl", "at")){
  
  #.ctrlUserHour(opts)
  
  foldPath <- .mergeFlowBasedPath(fb_opts)
  
  countTryList <- toupper(country_list)
  dayTyList <- unique(readRDS(paste0(foldPath,"domainesFB.RDS"))$idDayType)
  rangeDate <- range(dta$areas$time)
  rangeDate <- round(rangeDate, "day")
  
  G <- .GlobalEnv
  assign("dta", dta, envir = G)
  assign("countTryList", countTryList, envir = G)
  assign("dayTyList", dayTyList, envir = G)
  assign("rangeDate", rangeDate, envir = G)
  assign("fb_opts", fb_opts, envir = G)
  
  shiny::runApp(system.file("shinyPosition", package = "fbAntares"),
                launch.browser = TRUE)
}


#' @title Plot a flow-based typical domain and the Net Positions reached within in an Antares simulation
#' 
#' @description 
#' This function is used after running an Antares simulation to visualize how the domains have been used by the optimizer to
#' fix the exchanges in the CWE area. The user chooses one (or several) domain (typical day + hour) and the function will then
#' filter the given Antares output data to only keep the times when this domain has been used and calculate the Net Position, 
#' inside the CWE area, of the 2 countries chosen as axis. A plot gathers then the domain(s) and the points representing the 
#' Net Positions in the same color. The user can choose whether to plot Net Positions before and/or after applying the adequacy 
#' patch.
#' 
#' @details 
#' The Antares output data must respect the \code{antaresDataList} format (imported with \link{readAntares}). But it can be
#' filtered (see examples) to only keep days presenting unsupplied energy or a specific timeline.
#' In that case, choose the value \code{'all'} for the parameters \code{dayType} and/or \code{hour} and set the parameter
#' \code{filteringEmptyDomains} as TRUE. The function will then filter by itself the domains which are not used.
#' 
#'
#' @param fb_opts \code{list} of simulation parameters returned by the function
#'   \link{setSimulationPath} or flow-based model directory obtained with
#'   \link{setFlowbasedPath}. By default, the value will be indicated by \code{antaresRead::simOptions()}
#' @param data \code{antaresDataList} Antares output data, imported with \link{readAntares}. It can be a filtered (time or loss
#' of load) antaresDataList.
#' @param dayType \code{numeric}: typical day to plot. The value can also be 'all', for example if the \code{data} has been 
#' filtered.
#' @param hour \code{numeric} : hour to plot(format : 0:23, in accordance with Antares output). The value can also be 'all'.
#' @param country1 \code{character} : first country, axis X
#' @param country2 \code{character} : second country, axis Y
#' @param areaName \code{character} The name of the area of your study, possible values are
#' cwe_at (default), cwe and other. If you choose other, you have to give a csv file
#' which explains how your area work.
#' @param filteringEmptyDomains \code{boolean}, if TRUE, the function will only plot the domains for which it can find values
#' in \code{data} in accordance with \code{dayType} and \code{hour}. By default, it is FALSE.
#' @param nbMaxPt \code{numeric} : maximum number of points plotted on the graph. It can be increased (which increases computation
#' time). By default, the value is 10000.
#' @param palette \code{character} color range, by default the palette is "rainbow". Are vailable : 
#' "cm.colors", "topo.colors", "terrain.colors", "heat.colors", "rainbow".
#' @param xlim \code{numeric}, limits of x-axis
#' @param ylim \code{numeric}, limits of y-axis
#' @examples
#' \dontrun{
#' # Choose an ANtares study and import its output
#' study <- "../../Pour Julien/blop/MT_base_nucM2_2023"
#'
#' opts <- antaresRead::setSimulationPath(study, 17)
#' dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl", "at"),
#'                                 links = c("be - de","be - fr","be - nl",
#'                                 "de - fr","de - nl", "at - de"), mcYears = 1:2,
#'                                 select = c("LOLD", "UNSP. ENRG",
#'                                 "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#'                                 opts = opts)
#'
#' # plot the typical domain 1 at 00:00 and 19:00 and the matching output 
#' # points of the study on the axis France-Belgium
#' plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = 3, hour = c(0, 19),
#'          country1 = "BE", country2 = "FR", areaName = "cwe_at",
#'          xlim = c(-10000, 10000), ylim = c(-10000, 10000))
#'          
#' dta2 <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl"),
#'                                 links = c("be - de","be - fr","be - nl",
#'                                 "de - fr","de - nl"), mcYears = 1:2,
#'                                 select = c("LOLD", "UNSP. ENRG",
#'                                 "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#'                                 opts = opts)
#'                                 
#'          
#' # Change color palette and areaName
#' plotNetPositionFB(fb_opts = opts,
#'                  data = dta2,
#'                  dayType = 3, hour = c(0, 19),
#'                  country1 = "BE", country2 = "FR", palette = "topo.colors",
#'                  areaName = "cwe")
#'
#' # plot a single idTime : the data is filtered beforehand
#' # in this case all domains are plotted, also all are empty except one
#' # change also country1 -> DE
#' dta$areas <- dta$areas[timeId == 1]
#' plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = "all", hour = 0,
#'          country1 = "DE", country2 = "FR",
#'          areaName = "cwe_at")
#'
#' # Filtering empty domains : only the matching domain of timeid 1 will be drawn
#'
#' plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = "all", hour = 0,
#'          country1 = "BE", country2 = "FR", 
#'          filteringEmptyDomains = TRUE)
#'
#' # Plot with DE AT and changing of axes limits
#' 
#' plotNetPositionFB(fb_opts = opts,
#'          data = dta,
#'          dayType = "all", hour = 0,
#'          country1 = "AT", country2 = "DE", areaName = "cwe_at",
#'          filteringEmptyDomains = TRUE,
#'          xlim = c(-12000, 12000), ylim = c(-12000, 12000))
#' }
#'
#' @importFrom grDevices topo.colors
#' @export
plotNetPositionFB <- function(data, dayType,
                              hour, country1, country2, areaName = "cwe_at",
                              fb_opts = antaresRead::simOptions(),
                              filteringEmptyDomains = FALSE,
                              nbMaxPt = 10000, palette = "rainbow",
                              xlim = c(-8000, 8000), ylim = c(-8000, 8000)){
  
  
  if(!palette[1]%in%c("cm.colors", "topo.colors", "terrain.colors", "heat.colors", "rainbow")){
    stop('Palette must be in : "cm.colors", "topo.colors", "terrain.colors", "heat.colors", "rainbow"')
  }
  
  
  if(!all(c("areas", "links") %in% names(data))){
    stop("your data object must contain areas and links tables")
  }
  
  if(!"antaresDataList"%in%class(data)){
    warning(paste("data object should be an antaresDataList, the best way to load",
                  "data it's to use antaresRead. If straitment bug it's probably", 
                  "due to your data object"))
  }
  
  
  
  
  ##New version
  areaConf <- .getAreaName(areaName)
  
  nameCheck <- c(areaConf$country[[1]])
  if(!country1 %in% nameCheck){
    stop(paste0("country1 must be", paste(nameCheck, collapse = " ")))
  } 
  if(!country2 %in% nameCheck){
    stop(paste0("country2 must be", paste(nameCheck, collapse = " ")))
  } 
  
  if(!all(c("BALANCE", "UNSP. ENRG", "LOLD", "DTG MRG")%in%names(data$areas))){
    stop("This type of positions does not appear in the simulation data.")
  }
  
  idS <- antaresRead::getIdCols(data$areas)
  ##Test if no-adq are present
  idSNoAr <- idS[idS!="area"]
  
  if(length(unique(data$areas[, length(get("area")), by = idSNoAr]$V1)) != 1){
    stop(paste("All data by timeId-mcYear must have same length. If you have filtered your data", 
               ", you must keep all areas by goup of timeId-mcYear"))
  }
  
  namesToTest <- names(data$areas)[!names(data$areas)%in%idS]
  
  ctry1 <- country1
  ctry2 <- country2
  #.ctrlUserHour(opts)
  
  foldPath <- .mergeFlowBasedPath(fb_opts)
  secondM <- fread(paste0(foldPath, "second_member.txt"))
  if(!file.exists(paste0(foldPath, "scenario.txt"))){
    stop(paste0("The file scenario.txt is missing. Please either: add it to your flow-based model directory and use setFlowBasedPath(path = 'pathToDirectory') or
                use setFlowBasedPath(path = 'pathToAntaresStudy/user/flowbased')"))
  }
  scenario <- fread(paste0(foldPath, "scenario.txt"))
  ts <- fread(paste0(foldPath, "ts.txt"), header = TRUE)
  domaines <- readRDS(paste0(foldPath, "domainesFB.RDS"))
  if("dayType" %in% names(domaines)){
    setnames(domaines, "dayType", "idDayType")
  }
  if("hour" %in% names(domaines)){
    setnames(domaines, "hour", "Period")
  }
  if("outFlowBased" %in% names(domaines)){
    setnames(domaines, "outFlowBased", "VERTDetails")
    for (i in 1:length(domaines$VERTDetails)) {
      domaines$VERTDetails[[i]] <- domaines$VERTDetails[[i]]$pointX
    }
    
  }
  
  
  
  if(dayType[1] == "all") dayType <- unique(domaines$idDayType)
  if(hour[1] == "all") hour <- 0:23
  
  if(!all(hour%in%0:23)){
    stop("All hour elements must be between 0 and 23 (included)")
  }
  
  if(!all(dayType %in% unique(domaines$idDayType))){
    stop("Somes elements specify in dayType are not included in domainesFB.RDS file")
  }
  
  
  
  mcYears <- unique(data$areas$mcYear)
  out <- out2 <- NULL
  
  ipn <- .giveIpn(data, areaName = areaConf$country[[1]])
  
  
  
  
  out <- .constructDataForGraph(hour = hour,
                                dayType = dayType,
                                mcYears = mcYears,
                                ipn = ipn,
                                ctry1 = ctry1,
                                ctry2 = ctry2,
                                ts = ts, 
                                domaines = domaines)
  
  
  ##Remove domains on out
  
  if((!is.null(out)) & (!is.null(out2))){
    out <- sapply(1:length(out), function(inc){
      cbind(out[[inc]], out2[[inc]][,3:4])
    }, simplify = FALSE, USE.NAMES = FALSE)
  }else{
    if(is.null(out)){
      out <- out2
    }
  }
  
  if(filteringEmptyDomains){
    out <- lapply(out, function(X){
      if(sum(is.na(X[,3]))==nrow(X)){
        return(NULL)
      }else{
        return(X)
      }
    })
  }
  
  
  if(is.null(unlist(lapply(out, function(X){ncol(X)})))){
    stop('This selection of typical days/hours does not appear in the simulation data.', 
         'Try using dayType = "all" or/and hour = "all"')
  }
  nCurvByTyD <- max(unlist(lapply(out, function(X){ncol(X)})))
  
  out <- Reduce(c, out)
  
  nbpt <- sum(unlist(lapply(out, function(X){length(X[!is.na(X)])})))
  if(nbpt > nbMaxPt){
    stop(paste0("You try to draw ", nbpt, " points but you can't draw more than ", 
                nbMaxPt, " points. You can change this limit with nbMaxPt argument", 
                "but be carefull, your graph can be impossible to draw if you have soo much points."))
  }
  
  
  Mlength <- max(unlist(lapply(out, length)))
  out <- lapply(out, function(X){
    if(length(X)<Mlength){
      X <- c(X, rep(NA, Mlength-length(X)))
    }
    X
  })
  
  
  cleanNam <- gsub("_ADQ","", names(out))
  cleanNam <- cleanNam[!grepl("mcYear", cleanNam)]
  cleanNam <- cleanNam[!grepl("time", cleanNam)]
  
  stayH <- sapply(cleanNam, function(X){
    strsplit(X, "_")[[1]][3]
  })
  
  stayH <- unique(gsub("H", "", stayH))
  
  stayD <- sapply(cleanNam, function(X){
    strsplit(X, "_")[[1]][4]
  })
  
  stayD <- unique(gsub("D", "", stayD))
  
  outF <- Reduce(cbind.data.frame, out)
  outF <- data.frame(outF)
  names(outF) <- names(out)
  out <- outF
  
  
  oneOnNbC <- which(1:ncol(out)%%nCurvByTyD==1)
  allGraph <- list()
  CC <- 0
  colors <- substr(do.call(palette, args = list(n = length(oneOnNbC))), 1,7)
  for(X in oneOnNbC){
    curvInThisLoop <- X:(X+nCurvByTyD-1)
    curvInThisLoopnoModel <- curvInThisLoop[3:length(curvInThisLoop)]
    
    CC <- CC + 1
    
    ##Compute title
    titleS <- substr(names(out)[X], nchar(names(out)[X])-5, nchar(names(out)[X]))
    titleS <- gsub( "_H", "H",titleS)
    titleS <- gsub( "H", "0",titleS)
    titleS <- strsplit(titleS, "_")[[1]]
    titleS[1] <- ifelse(nchar(titleS[1] ) == 2, titleS[1], substr(titleS[1], 2,3))
    titleS <- paste0(titleS[1], "_", titleS[2])
    
    titleS <- paste0("Hour ", titleS)
    titleS <- gsub( "_D", " Day ",titleS)
    allGraph <- c(allGraph,
                  amGraph(title = titleS, balloonText =paste0(
                    '<b>Model<br>', ctry1, '</b> : [[x]] <br><b>',ctry2, '</b> : [[y]]'),
                    bullet = 'circle', xField = names(out)[X],yField = names(out)[X+1],
                    lineAlpha = 1, bulletSize = 0, lineColor = colors[CC],
                    lineThickness = 1, bulletAlpha = 0) )
    
    nameCurve <- names(out)[curvInThisLoopnoModel]
    adqc <- curvInThisLoopnoModel[grep("Position_ADQ", nameCurve)]
    noadqc <- curvInThisLoopnoModel[!curvInThisLoopnoModel%in%adqc]
    
    
    witchTipe <- nameCurve[grep("time_", nameCurve)]
    witchMc <- nameCurve[grep("mcYear_", nameCurve)]
    
    if(length(noadqc)>0){
      
      allGraph <- c(allGraph,
                    amGraph(balloonText =
                              paste0('<b>Position<br>', ctry1, 
                                     '</b> : [[x]] <br><b>',ctry2, '</b> : [[y]]
                                     <b>time : </b>[[', witchTipe, ']]
                                     <b>mcYear : </b>[[',witchMc, ']]
                                     '),
                            xField = names(out)[noadqc[1]],yField = names(out)[noadqc[2]],
                            lineAlpha = 0, bullet = "bubble", bulletSize = 4, 
                            lineColor = colors[CC],
                            lineThickness = 1, visibleInLegend  = FALSE))
    }
    if(length(adqc)>0){
      allGraph <- c(allGraph,
                    amGraph(balloonText =
                              paste0('<b>Position_ADQ<br>', ctry1, 
                                     '</b> :[[x]] <br><b>',ctry2, '</b> :[[y]]
                                     <b>time : </b>[[', witchTipe, ']]
                                     <b>mcYear : </b>[[',witchMc, ']]
                                     '),
                            bullet = 'triangleDown', xField = names(out)[adqc[1]],
                            yField = names(out)[adqc[2]],
                            lineAlpha = 0, bulletSize = 4, lineColor = colors[CC],
                            lineThickness = 1, visibleInLegend  = FALSE,
                            bulletBorderColor = colors[CC], bulletAlpha = 0, 
                            bulletBorderAlpha = 1))
      
    }
  }
  
  
  g <- pipeR::pipeline(
    amXYChart(dataProvider = out),
    addTitle(text = paste0("Flow-based ", ctry1, "/", ctry2, ', hour : ', 
                           paste0(stayH, collapse = ";"), ', typical day : ', 
                           paste0(stayD, collapse = ";"))),
    setGraphs(allGraph),
    setChartCursor(),
    addValueAxes(title = paste(ctry1, "(MW)"), position = "bottom", 
                 minimum = xlim[1], maximum = xlim[2]),
    addValueAxes(title =  paste(ctry2, "(MW)"), minimum = ylim[1], maximum = ylim[2]),
    setExport(enabled = TRUE),
    setLegend(enabled = TRUE, switchable = FALSE),
    plot()
  )
  combineWidgets(list = list(g))
}

.giveIpn <- function(dta, areaName){
  lole <- value <-NULL
  `UNSP. ENRG` <- `DTG MRG` <- NULL
  `UNSP. ENRG_ADQPatch` <- `DTG MRG_ADQPatch` <- NULL
  links <- dcast(dta$links, time + mcYear~link, value.var = "FLOW LIN.")
  allCt <- names(links)
  allCt <- allCt[-which(allCt == "time")]
  allCt <- allCt[-which(allCt == "mcYear")]
  # ct <- unique(unlist(sapply(allCt, function(X)strsplit(X, ' - '))))
  ct <- tolower(areaName)
  sapply(ct, function(ctCons){
    ct1 <- grep(ctCons, allCt)
    ct1 <- allCt[ct1]
    ctPlus <- which(substr(ct1, 1, 2) == ctCons)
    pluS <- paste0(paste0("+", paste0("`", ct1[ctPlus], "`")), collapse = "")
    ctMoins <- which(substr(ct1, 1, 2) != ctCons)
    moins <- paste0(paste0("-", paste0("`", ct1[ctMoins], "`")), collapse = "")
    if(length(ctPlus) == 0){
      expEnd <- moins
    }else if(length(ctMoins) == 0){
      expEnd <- pluS
      
    }else{
      expEnd <- paste0(pluS, moins)
    }
    links[, tp := eval(parse(text = expEnd))]
    setnames(links, "tp", ctCons)
  })
  
  links <- links[, .SD, .SDcols = c("time", "mcYear",ct)]
  links <- melt(links, id = 1:2)
  setnames(links, "variable", "area")
  dta$areas <- merge(dta$areas, links, by = c("time", "mcYear", "area"))
  dta$areas[, lole :=`UNSP. ENRG` - `DTG MRG` - value]
  dta$areas[, ipn := value]
  ipn <- dcast(dta$areas, time + mcYear~area, value.var = c("ipn"))
  
  ipn
}

.constructDataForGraph <- function(hour, dayType, mcYears, ipn, ctry1, ctry2, ts, domaines){
  out <-  sapply(hour, function(HH){
    sapply(dayType, function(DD){
      ######
      
      ipnout <- sapply(mcYears, function(mcy){
        
        ipntp <- ipn[which(hour(ipn$time)  == HH  & ipn$mcYear == mcy)]
        
        dateSel <- unlist(ts[,.SD, .SDcols = as.character(mcy)] == DD)
        dateSel <- ts$Date[dateSel]
        daysel <- substr(dateSel, 6, 10)
        
        ipntp <- ipntp[substr(ipntp$time, 6, 10) %in% daysel]
        ipntp
      }, simplify = FALSE)
      ipnO <- rbindlist(ipnout)
      # if(HH == 0)HH <- 24
      dSel <- domaines[which(idDayType == DD & Period ==( HH  + 1))]
      
      ##TODO vérifier que c'est bien pointX!)
      points <- dSel$VERTDetails[[1]]
      
      col_ptdfraw <- colnames(dSel$PTDFRawDetails[[1]])[
        grepl("ptdf", colnames(dSel$PTDFRawDetails[[1]]))]
      col_vert <- colnames(dSel$VERTDetails[[1]])[
        !grepl("idDayType|Period", colnames(dSel$VERTDetails[[1]]))]
      col_ptdfraw <- gsub("ptdf", "", col_ptdfraw)
      coldiff <- col_ptdfraw[!(col_ptdfraw %in% col_vert)]
      
      resdiff <- -dSel$VERTDetails[[1]][[col_vert[1]]]
      for (i in 2:length(col_vert)) {
        resdiff <- resdiff - dSel$VERTDetails[[1]][[col_vert[i]]]
      }
      points[[coldiff]] <- resdiff
      
      res <- data.frame("ctry1" = points[[ctry1]],
                        "ctry2" = points[[ctry2]])
      
      
      res2 <- data.frame("ctry11" = unlist(ipnO[, .SD, .SDcols = tolower(ctry1)]),
                         "ctry22" = unlist(ipnO[, .SD, .SDcols = tolower(ctry2)]),
                         "time" = c(ipnO[, .SD, .SDcols = "time"]),
                         "mcYear" = c(ipnO[, .SD, .SDcols = "mcYear"]))
      res2$time <- as.character(res2$time +1)
      
      ##Supress year :
      res2$time <- substr(res2$time, 6,nchar(res2$time ) -3)
      
      # primaryKey <-  data.frame("time" = unlist(ipnO[, .SD, .SDcols = "time"]),
      #                           "mcYear" = unlist(ipnO[, .SD, .SDcols = "mcYear"]))
      
      
      res <- res[chull(res),]
      res <- rbind(res, res[1,])
      res <- round(res, 2)
      
      max_r <- max(nrow(res), nrow(res2))
      if(nrow(res)<max_r){
        res <- rbind(res, data.frame(ctry1 = rep(NA, max_r-nrow(res)),
                                     ctry2 = rep(NA, max_r-nrow(res))))
      }
      if(nrow(res2)<max_r){
        res2 <- rbind(res2, data.frame(ctry11 = rep(NA,max_r- nrow(res2)),
                                       ctry22 = rep(NA, max_r-nrow(res2)),
                                       time = rep(NA, max_r-nrow(res2)),
                                       mcYear = rep(NA, max_r-nrow(res2))))
      }
      
      out2 <- cbind(res, res2)
      names(out2) <- c(paste0(c("Model", "Model", "Position", "Position"),"_", 
                              c(ctry1, ctry2), "_H", HH, "_D", DD), 
                       paste0('time',  "_H", HH, "_D", DD), 
                       paste0('mcYear',  "_H", HH, "_D", DD))
      
      data.frame(out2)
      
    }, simplify = FALSE)
  }, simplify = FALSE)
  
  
  out <- unlist(out, recursive = FALSE)
  out
}

.mergeFlowBasedPath <- function(fb_opts){
  
  if("simOptions" %in% class(fb_opts)){
    foldPath <- paste0(fb_opts$studyPath, "/user/flowbased/")
  }else if("flowBasedPath" %in% class(fb_opts)){
    foldPath <- paste0(fb_opts$path, "/")
  }else{
    stop("fb_opts must be obtain with setSimulationPath or setFlowbasedPath function")
  }
  if(!file.exists(paste0(foldPath, "second_member.txt"))){
    stop("Impossible to found second_member.txt file, you can specify fb_opts with setSimulationPath or setFlowbasedPath function")
  }
  
  foldPath
}
