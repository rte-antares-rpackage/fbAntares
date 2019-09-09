#' @title addNetPosition
#' 
#' @description This function calculates the Net Position (the balance)
#' of areas within a bigger group of areas, 
#' based on the exchanged flows in the indicated perimeter.
#' By default, the function calculates the Net Position within 
#' the CWE area and creates a new column called Balance_CWE. 
#'
#' @param data \code{antaresDataList} read with readAntares, containing both 
#' areas and links tables.
#' @param opts \code{list} of simulation parameters returned by the 
#' function \link{setSimulationPath}: this represents the 
#' directory of the Antares study. The default path is indicated by 
#' \code{antaresRead::simOptions()}.
#' @param inAreas \code{character} lists of areas belonging to the perimeter, 
#' whose exchanges are going to be taken into account in the balance
#' calculation. All links connecting two areas in this list are taken 
#' into account, the links connected to only one area (or none) are ignored.
#' By default, the list is c("be", "de", "fr", "nl", "at").
#' @param newName \code{character} Added suffix to the calculated column's name. 
#' By default, the value is "_CWEAt".
#' 
#' 
#' @examples
#' \dontrun{
#' # Change the study path for the path of a study you have on your computer
#' study <- "../../Pour Julien/blop/MT_base_nucM2_2023"
#' opts <- antaresRead::setSimulationPath(study, 17)
#' data <- antaresRead::readAntares(area = c("fr", "be", "de", "nl"), 
#'   links = c("be - de","be - fr","be - nl", "de - fr","de - nl"), 
#'   mcYears = 1)
#' 
#' ##Add the net positions in the CWE area
#' data <- addNetPosition(data, opts, inAreas = c("be", "de", "fr", "nl"),
#'   newName = "_CWE")
#' 
#' ##Add the net positions in an area containing CWE+AT
#' data <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl", "at"),
#'  links = c("be - de","be - fr","be - nl",
#'  "de - fr","de - nl", "at - de"), mcYears = 1)
#' data <- addNetPosition(data, opts,
#'  inAreas = c("be", "de", "fr", "nl", "at"), newName = "_CWEAt")
#' 
#' ## Example with the virtual area
#' # Change the study path for the path of a study you have on your computer
#' study <- "../Etude Antares/BP19_costs18_FB18_2023Virtual/"
#' opts <- antaresRead::setSimulationPath(study, 5)
#' 
#' dta <- antaresRead::readAntares(
#'   areas = c("fr", "be", "de", "nl", "at", "zz_flowbased"),
#'   links = c("be - zz_flowbased",
#'             "fr - zz_flowbased", "nl - zz_flowbased",
#'             "de - zz_flowbased", "at - zz_flowbased"), mcYears = 1:2,
#'   select = c("LOLD", "UNSP. ENRG",
#'              "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#'   opts = opts)
#' 
#' dta <- addNetPosition(
#' dta, opts, inAreas = c("be", "de", "fr", "nl", "at", "zz_flowbased"), 
#' newName = "_CWEAtVirtual")
#' 
#' }
#' 
#' @export
addNetPosition <- function(data, opts = antaresRead::simOptions(), 
                           inAreas = c("be", "de", "fr", "nl", "at"), 
                           newName = "_CWEAt"){
  allAreas <- antaresRead::getAreas(opts = opts)
  allLinks <- antaresRead::getLinks(opts = opts)
  allLinksSave <- allLinks
  allLinks <- strsplit(allLinks, " - ")
  
  
  
  ##Test if all nedded links are in data
  linkNedded <- unlist(lapply(allLinks, function(X){
    X[1]%in%inAreas&X[2]%in%inAreas
  }))
  
  if(!all(allLinksSave[linkNedded]%in%unique(data$links$link))){
    lkN <- allLinksSave[linkNedded]
    missLk <- lkN[!lkN%in%unique(data$links$link)]
    missLk <- paste0(missLk, collapse = ";")
    stop(paste0("Somes links are missing in your data : ",missLk))
  }
  
  
  allAreas <- allAreas[allAreas%in%inAreas]
  
  areaLinkTable <- sapply(allAreas, function(X){
    paste0(unlist(lapply(allLinks, function(Y){
      if(X == Y[1] & Y[2]%in% inAreas)
      {
        return(paste0("+`", Y[1], " - ", Y[2], "`"))
      }
      if(X == Y[2]& Y[1]%in% inAreas)
      {
        return(paste0("-`", Y[1], " - ", Y[2], "`"))
      }
      
      NULL
    })), collapse = "")
  }, simplify = FALSE)
  class(areaLinkTable)
  fl <- "FLOW LIN."
  
  
  ##Suppress no-compete area
  links <- dcast(data$links, time + mcYear~link, value.var = c(fl))
  areaLinkTable <- areaLinkTable[unlist(lapply(areaLinkTable, function(X){
    spl <- strsplit(X,"`")[[1]]
    aR <- spl[1:length(spl)%%2==0]
    all(aR %in% names(links))
  }))]
  
  
  suppressWarnings(
    for(i in 1:length(areaLinkTable)){
      links[,names(areaLinkTable)[i] := eval(parse(text =areaLinkTable[[i]] ))]
    }
  )
  links <- links[,.SD, .SDcols = c("time", "mcYear", names(areaLinkTable)[names(areaLinkTable)%in%names(links)])]
  links <- melt(links, id = 1:2)
  setnames(links, "variable", "area")
  newColumnName <- paste0("Balance", newName)
  
  
  
  
  data$areas <- merge(data$areas, links, by = c("time", "mcYear", "area"), all.x = TRUE)
  
  data$areas$value[is.na(data$areas$value)] <- 0
  setnames(data$areas, "value", newColumnName)
  
  setorderv(data$areas, c("area", "mcYear", "timeId"))
  data
}


