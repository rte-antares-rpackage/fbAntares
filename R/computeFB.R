#' @title Conversion of flow-based domains into an ANtares model with fixed PTDF
#' 
#' @description 
#' This function follows two steps: based on the list of flow-based domains given in input, it will calculate a standard shape
#'  (selection of sides, k-medoid method) and will then project the real domains on this standard shape. The projection is 
#' computed using an optimisation algorithm aiming at minimising the volumetric error between the real domain and its projection.
#'  The function will then write in an output directory the standard shape (weights.txt), the projection result for each domain 
#'  (second_member.txt) and an RDS object storing information on the projection and the errors. Reports can be written represening the real and
#' modelled domains and the volumetric error between them.
#'
#' @param PTDF \code{character}, path leading to the flow-based domains (PTDF description) list. 
#' By default, this leads to an example file (\code{"PTDF.csv"}, saved in the package).
#'  This must be a csv file containing the following columns (and column names): 
#' \itemize{
#'  \item Id_day : numeric, name of each day. Default in example id_day between 1 and 12.
#'  \item Date : The date of the typical day 
#'  \item Period : numeric, hour of the day. Default in example period between 1 and 24 (1 is then between 00:00 and 01:00).
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
#' @param outputName \code{character}, path/name of the output directory. By default, the value is a created directory named 
#' "antaresInput" in the current directory
#' @param reports \code{boolean}, if TRUE, the function will write html reports (one per typical day). By default, the value is 
#' TRUE.
#' @param dayType \code{numeric}, by default, the value is All. (optional) Vector of id_days to compute.
#' @param hour \code{numeric}, by default, the value is All. (optional) vector of hours/periods to compute.
#' @param hubDrop \code{list}, list of hubs in the ptdf, with the ones which should
#' sustracted to the others as the names of the arrays which themself contain the ones which
#' be sustracted
#' @param nbFaces \code{numeric}, standard shape parameters: number of sides to select. By default, the value is 36.
#' @param nbLines \code{numeric}, number of halflines drawn for the distance computation, default 10 000
#' @param maxiter \code{numeric}, maximum number of iteration on the optimization problem, default 10
#' @param thresholdIndic \code{numeric}, minimum value of the validation indicator to stop, default 0.9
#' the optimization problem
#' @param quad \code{logical}, quadratic problem or linear, default FALSE
#' @param seed \code{numeric}, value of the seed, default NULL
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
#' computeFB(PTDF = system.file("testdata/2019-07-17ptdf.csv", package = "fbAntares"), reports = FALSE)
#' 
#' }
#' @importFrom stats cutree dist hclust
#' @importFrom utils combn write.table
#' @export
computeFB <- function(PTDF = system.file("testdata/2019-07-18ptdfraw.csv", package = "fbAntares"),
                      outputName =  paste0(getwd(), "/antaresInput"),
                      reports = TRUE,
                      dayType = "All", hour = "All", nbFaces = 36,
                      verbose = 1,
                      nbLines = 10000, maxiter = 10, thresholdIndic = 90, quad = F,
                      hubDrop = list(NL = c("BE", "DE", "FR", "AT")), seed = NULL)
{
  
  PTDFDetails <- Face <- ram <- outFlowbased <- generateReportFb <- idDayType <- Period <- NULL
  # pb <- txtProgressBar(style = 3)
  
  
  
  ######### OK
  PTDF <- .readPTDF(PTDF)
  
  PTDFRaw <- copy(PTDF)
  .ctrlHubDrop(hubDrop = hubDrop, PTDF = PTDF)
  PTDF <- setDiffNotWantedPtdf(PTDF = PTDF, hubDrop = hubDrop)
  
  col_ptdf <- colnames(PTDF)[
    grep("ptdf", colnames(PTDF))]
  # univ <- .univ(nb = 500000, bInf = -10000, bSup = 10000, 
  #               col_ptdf = col_ptdf, seed = seed)
  
  face <- giveBClassif(PTDF, nbClust = nbFaces)
  face <- round(face, 2)
  if(dayType == "All"){
    dayType <- unique(PTDF$idDayType)
  }
  
  if(hour == "All"){
    # reports <- FALSE
    hour <- unique(PTDF$Period)
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
    B <- copy(face)
    B[, c("ram", "idDayType", "Period") := list(100, unique(A$idDayType), unique(A$Period))]
    setcolorder(B, colnames(A))
    res <- getBestPolyhedron(
      A = A, B = B, nbLines = nbLines, maxiter = maxiter, 
      thresholdIndic = thresholdIndic, quad = quad, verbose = verbose)
    res[, Face := NULL]
    error <- evalInter(A, res)
    # out <- data.table(hour = combi[X, hour], idDayType = combi[X, dayType],
    #                   outFlowbased = list(res), volIntraInter = error[1, 1],
    #                   error1 = error[1, 2], error2 = error[1, 3])
    # out <- data.table(hour = combi[X, hour], idDayType = combi[X, dayType],
    #                   outFlowbased = list(data.table(
    #                     idDayType = unique(res$idDayType), Period = unique(res$Period),
    #                     PTDFDetails = list(res))), 
    #                   volIntraInter = error[1, 1],
    #                   error1 = error[1, 2], error2 = error[1, 3])
    PTDFRawDetails <- PTDFRaw[Period == combi[X, hour] & idDayType == combi[X, dayType],
                              .SD, .SDcols = c("idDayType", "Period", col_ptdf, "ram")]
    VERTDetails <- getVertices(res)
    VERTDetails[, c("Date", "Period") := NULL]
    VERTDetails[, c("idDayType", "Period") := list(combi[X, hour], combi[X, dayType])]
    setcolorder(VERTDetails, c("idDayType", "Period"))
    
    out <- data.table(hour = combi[X, hour], idDayType = combi[X, dayType],
                        PTDFDetails = list(res), PTDFRawDetails = list(PTDFRawDetails),
                      VERTDetails = list(VERTDetails), volIntraInter = error[1, 1],
                      error1 = error[1, 2], error2 = error[1, 3])
  }, simplify = F))
  
  ######### OK
  
  
  
  ##From B to antares
  
  antaresFace <- .fromBtoAntares(face, col_ptdf)
  
  ######### OK
  
  ##Output
  allFaces <- rbindlist(sapply(1:nrow(combi), function(X){
    
    nam <- 1:nrow(antaresFace)
    nam <- ifelse(nchar(nam) == 1, paste0("0", nam), nam)
    data.table(Id_day = combi[X, dayType], Id_hour = combi[X, hour],
               vect_b = flowbased[idDayType == combi[X, dayType] &
                                    hour == combi[X, hour],
                                  PTDFDetails][[1]][, ram], Name = paste0("FB", nam))
  }, simplify = F))
  
  
  allFaces$vect_b <- round(allFaces$vect_b, 0)
  dir.create(outputName)
  write.table(antaresFace, paste0(outputName, "/weight.txt"), row.names = FALSE, sep = "\t", dec = ".")
  saveRDS(flowbased, paste0(outputName, "/domainesFB.RDS"))
  write.table(allFaces, paste0(outputName, "/second_member.txt"), row.names = FALSE, sep = "\t", dec = ".")
  if(reports){
    outputNameReports <- paste0(outputName, "/reports")
    dir.create(outputNameReports)
    sapply(unique(flowbased$dayType), function(X){
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
    stop("Your columns chould contain at least Date, Period, ram and idDayType 
         and at least two ptdf name (ex ptdfFR)")
  }
  PTDF
}
