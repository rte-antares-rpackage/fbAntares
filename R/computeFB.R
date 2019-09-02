#' @title Conversion of flow-based domains into an ANtares model with fixed PTDF
#' 
#' @description 
#' This function follows two steps: based on the list of flow-based domains 
#' given in input, it will calculate a standard shape
#' (selection of sides, k-medoid method) and will then project the real domains 
#' on this standard shape. The projection is 
#' computed using an optimisation algorithm aiming at minimising the 
#' volumetric error between the real domain and its projection.
#' The function will then write in an output directory the standard shape 
#' (weights.txt), the projection result for each domain 
#' (second_member.txt) and an RDS object storing information on the projection 
#' and the errors. Reports can be written represening the real and
#' modelled domains and the volumetric error between them.
#'
#' @param PTDF \code{character}, path leading to the flow-based 
#' domains (PTDF description) list. 
#' By default, this leads to an example file (\code{"PTDF.csv"}, saved in the package).
#'  This must be a csv file containing the following columns (and column names): 
#' \itemize{
#'  \item Id_day : numeric, name of each day. Default in example id_day between 1 and 12.
#'  \item Date : The date of the typical day 
#'  \item Period : numeric, hour of the day. Default in example period 
#'  between 1 and 24 (1 is then between 00:00 and 01:00).
#'  \item ptdfBE : numeric, PTDF coefficient of Belgium.
#'  \item ptdfDE : numeric, PTDF coefficient of Germany. 
#'  \item ptdfFR : numeric, PTDF coefficient of France. 
#'  \item ptdfNL : numeric, PTDF coefficient of the Netherlands. 
#'  \item ptdfAT : numeric, PTDF coefficient of the Austria.
#'  \item more ptdf if needed 
#'  \item ram : numeric, remaining available capacity in the critical branch (MW). 
#'  \item Class : character, class of typical day (ex WinterSe) (not necessary)
#'  \item idDayType : numeric, name of each day. Default in example id_day between 1 and 12.
#' }
#' @param outputName \code{character}, path/name of the output directory. 
#' By default, the value is a created directory named 
#' "antaresInput" in the current directory
#' @param reports \code{boolean}, if TRUE, the function will write html reports 
#' (one per typical day). By default, the value is TRUE.
#' @param dayType \code{numeric}, by default, the value is All. 
#' (optional) Vector of id_days to compute.
#' @param hour \code{numeric}, by default, the value is All. 
#' (optional) vector of hours/periods to compute.
#' @param hubDrop \code{list}, list of hubs in the ptdf, with the ones which should
#' sustracted to the others as the names of the arrays which themself 
#' contain the ones which be sustracted
#' @param clusteringDayType \code{numeric}, by default, the value is All. (optional) 
#' Typical days you want to choose for the faces selection.
#' @param clusteringHours \code{numeric}, by default, the value is All. (optional) 
#' Hours you want to choose for the faces selection.
#' @param  fixFaces \code{data.table} data.table if you want to use fix faces for 
#' the creation of the flowbased models. If you want to do it, 
#' the data.table has the following form :
#' data.table(func = c("min", "min", "max", "min"), zone = c("BE", "FR", "DE", "DE")).
#' func is the direction of the fix faces and zone is the area of this direction.
#' If you give for example min and DE, there will be a fix face at the minimum import
#' value of Germany.
#' @param areaName \code{character} The name of the area of your study, possible values are
#' cwe_at (default), cwe and other. If you choose other, you have to modify the csv file
#' of the package and use the examples to write how your area work.
#' The actual csv file is in the folder areaName of the package if you want to
#' modify it or understand how it is written.
#' @param virtualFBarea \code{logical} If you want to use the new area format or not
#' default is FALSE
#' @param nbFaces \code{numeric}, standard shape parameters: 
#' number of sides to select. By default, the value is 75
#' @param nbLines \code{numeric}, number of halflines drawn 
#' for the distance computation, default 100 000
#' @param maxiter \code{numeric}, maximum number of iteration on 
#' the optimization problem, default 15
#' @param thresholdIndic \code{numeric}, minimum value of the validation 
#' indicator to stop, default 95
#' the optimization problem
#' @param quad \code{logical}, quadratic problem or linear, default FALSE
#' @param seed \code{numeric}, value of the seed, default 123456
#' @param verbose \code{numeric}, shows the logs in console. By default, the value is 1.
#' \itemize{
#'  \item 0 : No log
#'  \item 1 : Short log
#'  \item 2 : Medium log
#' }
#' @examples
#' \dontrun{
#' # Compute models for all days and hours of a PTDF file, with no reports 
#' # automatically generated at the same time
#' computeFB(PTDF = system.file("testdata/2019-07-18ptdfraw.csv", package = "fbAntares"), 
#' reports = FALSE, areaName = "cwe_at", hubDrop = list(NL = c("BE", "DE", "FR", "AT"))
#' 
#' # Example using more arguments like the fixFaces
#' computeFB(PTDF = system.file("testdata/2019-07-18ptdfraw.csv", package = "fbAntares"), 
#' reports = FALSE, areaName = "cwe_at", hubDrop = list(NL = c("BE", "DE", "FR", "AT")),
#' nbFaces = 75, dayType = 1, clusteringHours = c(7:10, 17:19), nbLines = 50000, 
#' maxiter = 20, thresholdIndic = 95, fixFaces = data.table(func = "min", zone = "BE"))
#' 
#' # Example with the virtualFBarea
#' computeFB(PTDF = system.file("testdata/2019-07-18ptdfraw.csv", package = "fbAntares"), 
#' reports = FALSE, areaName = "cwe_at", hubDrop = list(NL = c("BE", "DE", "FR", "AT"),
#' virtualFBarea = TRUE)
#' 
#' }
#' @importFrom stats cutree dist hclust
#' @importFrom utils combn write.table
#' @export
computeFB <- function(PTDF = system.file("testdata/2019-07-18ptdfraw.csv", package = "fbAntares"),
                      outputName =  paste0(getwd(), "/antaresInput"),
                      reports = TRUE,
                      areaName = "cwe_at",
                      dayType = "All", hour = "All", 
                      clusteringDayType = "All", clusteringHours = "All",
                      nbFaces = 75, verbose = 1,
                      nbLines = 100000, maxiter = 15, thresholdIndic = 95, quad = F,
                      hubDrop = list(NL = c("BE", "DE", "FR", "AT")), 
                      fixFaces = NULL, virtualFBarea = F,
                      seed = 123456)
{
  browser()
  if (!is.null(seed)) {
    set.seed(seed)
  }
  PTDFDetails <- Face <- ram <- outFlowbased <- generateReportFb <- idDayType <- Period <- NULL
  # pb <- txtProgressBar(style = 3)
  # browser()
  
  ######### OK
  PTDF <- .readPTDF(PTDF)
  
  # In order to keep the raw ptdf in the output
  PTDFRaw <- copy(PTDF)
  .ctrlHubDrop(hubDrop = hubDrop, PTDF = PTDF)
  # generate transformed ptf in order o get the vertices
  PTDF <- setDiffNotWantedPtdf(PTDF = PTDF, hubDrop = hubDrop)
  
  col_ptdf <- colnames(PTDF)[
    grep("ptdf", colnames(PTDF))]
  col_ptdfraw <- colnames(PTDFRaw)[
    grep("ptdf", colnames(PTDFRaw))]

  
  if (!is.null(fixFaces)) { 
    if (nrow(fixFaces) > 0) {
      # check if the argument fixFaces is well written
      .crtlFixFaces(fixFaces = fixFaces, col_ptdf = col_ptdf)
      
    }
    # to have a total of faces equal to nbFaces
    nbCl <- nbFaces-nrow(fixFaces)
  } else {
    nbCl <- nbFaces
  }

  ## Clustering on the ptdf lines to obtain the faces of the 
  
  ###### changement
  face <- giveBClassif(
    PTDFRaw, nbClust = nbCl, fixFaces = fixFaces, col_ptdf = col_ptdfraw,
    clusteringDayType = clusteringDayType, clusteringHours = clusteringHours)
  
  face <- round(face, 2)
  faceraw <- copy(face)
  face <- setDiffNotWantedPtdf(PTDF = face, hubDrop = hubDrop)
  # keep only the hours and daytype you want to return
  if(length(dayType) == 1) {
    if(dayType == "All"){
      dayType <- unique(PTDF$idDayType)
    }
  }
  if(length(hour) == 1) {
    if(hour == "All"){
      # reports <- FALSE
      hour <- unique(PTDF$Period)
    }
  }
  ##From B to antares
  # if cwe, cwe-at or other area
  areaConf <- .getAreaName(areaName)
  if (virtualFBarea) {
    antaresFace <- .fromBtoAntaresvirtualFBarea(
      face = faceraw, col_ptdf = col_ptdfraw)
    
  } else {
    antaresFace <- .fromBtoAntares(face, col_ptdf, areaConf = areaConf)
  }
  
  combi <- data.table(expand.grid(hour, dayType))
  names(combi) <- c("hour", "dayType")
  
  flowbased <- rbindlist(sapply(1:nrow(combi), function(X) {
    if(verbose>0){
      cat(paste0("\n", "Optim for hour : ", combi[X, hour],
                 " and typical day : ", combi[X, dayType], "\n"))
    }
    A <- PTDF[Period == combi[X, hour] &
                idDayType == combi[X, dayType], .SD,
              .SDcols = c("idDayType", "Period", col_ptdf, "ram")]
    
    VERTRawDetails <- getVertices(A)
    VERTRawDetails[, c("Date", "Period") := NULL]
    VERTRawDetails[, c("idDayType", "Period") := list(combi[X, dayType], combi[X, hour])]
    setcolorder(VERTRawDetails, c("idDayType", "Period"))
    
    # Creation of the modelized domain
    B <- copy(face)

    ## Initialization of the rams to 1000 (arbitrary initialization)
    B[, c("ram", "idDayType", "Period") := list(1000, unique(A$idDayType), unique(A$Period))]

    # Adding of fix faces if they exist
    if (!is.null(fixFaces)) {
      if (nrow(fixFaces) > 0) {
        dtFixRam <- .getFixRams(fixFaces, VERTRawDetails)
        B[(nrow(B)-nrow(dtFixRam)+1):nrow(B), ram := abs(dtFixRam$ram)]
      }
    }
    
    setcolorder(B, colnames(A))
    ## Finalization of modelized domain
    res <- getBestPolyhedron(
      A = A, B = B, nbLines = nbLines, maxiter = maxiter, 
      thresholdIndic = thresholdIndic, quad = quad, verbose = verbose, 
      fixFaces = fixFaces, VERTRawDetails = VERTRawDetails)
    
    res[, Face := NULL]
    error <- evalInter(A, res)
    if(verbose >= 2) {
      print(error)
    }
    ## Creation of the output, we use all objects we need
    PTDFRawDetails <- PTDFRaw[Period == combi[X, hour] & idDayType == combi[X, dayType],
                              .SD, .SDcols = c("idDayType", "Period", col_ptdfraw, "ram")]
    VERTDetails <- getVertices(res)
    VERTDetails[, c("Date", "Period") := NULL]
    VERTDetails[, c("idDayType", "Period") := list(combi[X, dayType], combi[X, hour])]
    setcolorder(VERTDetails, c("idDayType", "Period"))
    
    ## Final data.table
    out <- data.table(Period = combi[X, hour], idDayType = combi[X, dayType],
                      PTDFDetails = list(res), PTDFRawDetails = list(PTDFRawDetails),
                      VERTDetails = list(VERTDetails), VERTRawDetails = list(VERTRawDetails),
                      volIntraInter = error[1, 1],
                      error1 = error[1, 2], error2 = error[1, 3])
  }, simplify = F))
  

  

  ##Output
  # Writting of the second member
  allFaces <- rbindlist(sapply(1:nrow(combi), function(X){
    
    nam <- 1:nrow(antaresFace)
    maxnchar <- max(nchar(nam))
    nam <- ifelse(nchar(nam)==1, paste0(0, nam), nam)
    if(maxnchar == 3) {
      nam <- ifelse(nchar(nam)==2, paste0(0, nam), nam)
    }
    data.table(Id_day = combi[X, dayType], Id_hour = combi[X, hour],
               vect_b = flowbased[idDayType == combi[X, dayType] &
                                    Period == combi[X, hour],
                                  PTDFDetails][[1]][, ram], Name = paste0("FB", nam))
  }, simplify = F))
  
  
  allFaces$vect_b <- round(allFaces$vect_b, 0)
  ## Writting of all the objects needed in the antares model
  dir.create(outputName)
  write.table(antaresFace, paste0(outputName, "/weight.txt"), row.names = FALSE, sep = "\t", dec = ".")
  saveRDS(flowbased, paste0(outputName, "/domainesFB.RDS"))
  write.table(allFaces, paste0(outputName, "/second_member.txt"), row.names = FALSE, sep = "\t", dec = ".")
  if(reports){
    # If you want to generate a report
    outputNameReports <- paste0(outputName, "/reports")
    dir.create(outputNameReports)
    sapply(unique(flowbased$idDayType), function(X){
      print(outputNameReports)
      generateReportFb(allFB = flowbased, dayType = X, output_file = outputNameReports)
    })
  }
  
  outputName
}

#' @title read PTDF file
#'
#' @description read PTDF file
#' @param PTDF \code{character} PTDF path.
#'
#' @return \code{data.table}
#'
#' @noRd
#'
.readPTDF <- function(PTDF){
  if(grepl("csv$", PTDF)) {
    PTDF <- try(fread(PTDF))
  } else {
    PTDF <- try(readRDS(PTDF))
  }
  if (!all(c("Date", "Period", "ram", "idDayType") %in% names(PTDF)) |
      length(grep("ptdf", colnames(PTDF))) < 2) {
    stop("Your columns should contain at least Date, Period, ram and idDayType 
         and at least two ptdf name (ex ptdfFR)")
  }
  PTDF
}
