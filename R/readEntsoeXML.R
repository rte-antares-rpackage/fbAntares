request <- "https://transparency.entsoe.eu/api?DocumentType=A69&In_Domain=10YDE-VE-------2&securityToken=3ea2c90d-8ecb-4452-898a-263ea835f498&periodStart=201807200000&periodEnd=201907230000&processType=A01&psrType=B16"

#' @title Download data from entsoe and convert them to data.table and csv.
#' @description This function allows you to download data from entsoe and convert 
#' it in data.frame and/or a csv file. If you need more help to kwon how to do 
#' the requests, don't hesitate to use the help of the entsoe website :
#' https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_available_parameters
#' Some of the following arguments can be optional according to the data you 
#' want to load.
#' 
#' This function works for all wind and solar data but can meet some issues 
#' with other data
#' @param request \code{character} If you are familiar with entsoe data, you can
#' put here your entire request instead of use the decomposition proposed with
#' the other parameters
#' @param bz \code{character} build zone, transparency.entsoe.eu API guide
#' @param outDomain \code{character} outDomain (if there is one), an area
#' linked to the build zone
#' @param docType \code{character} docType, A80 for generation unit, A77 for 
#' produciton unit, A69 for solar and wind forecast, 
#' @param docStatus \code{character} docStatus "A05" : Active, "A09" : Cancel
#' @param timeStart \code{character} start date, format YYYYMMDDhhmm (ex : 201807200000)
#' @param timeEnd \code{character} end date, format YYYYMMDDhhmm (ex : 201907200000)
#' Maximum of documents load by request is 200 so if data are not correctly try 
#' an other timestep.
#' @param processType \code{character} type of processed data you want, ex : 
#' A16 = realised, A33 = year ahead
#' @param psrType \code{character} type of energy you want, ex : B16 = Solar,
#' B19 = Wind Onshore
#' @param token \code{character}, ENTSOE token
#' @param entsoeHttps \code{character}, ENTSOE adress, defalut https://transparency.entsoe.eu/api?
#' @param writeDataDirectory \code{character} Write the data in a csv or rds file in
#' the wanted directory
#' @param fileName \code{character} Name of the file you want to write 
#' (either csv or rds). If NULL, the file is writtend in your working directory
#' 
#' @examples
#' \dontrun{
#' 
#' 
#'  #PROD : "A77" Gene : "A80"
#'  token <- "Mytoken" 
#'  
#'  #BiddingZone_Domain = "10YFR-RTE------C"
#'  #BiddingZone_Domain = "10YBE----------2"
#'  #BiddingZone_Domain = "10YNL----------L"
#'  #BiddingZone_Domain = "10Y1001A1001A63L"
#'
#'  #BiddingZone_Domain = "10YDE-VE-------2"    #GRT Allemand 50Hertz
#'  #BiddingZone_Domain = "10YDE-RWENET---I"    #GRT Allemand Amprion
#'  #BiddingZone_Domain = "10YDE-EON------1"    #GRT Allemand TennetDE
#'  #BiddingZone_Domain = "10YDE-ENBW-----N"    #GRT Allemand TransnetBW
#'  #BiddingZone_Domain = "10YAT-APG------L"    #Autriche#' 
#' 
#'  #NL Wind + Solar
#'  readEntsoeXML(token = token, bz = "10YNL----------L", docType = "A69",
#' start = "201807200000", end = "201907230000", processType = "A01")
#'
#'  #BE Wind + Solar
#'  readEntsoeXML(token = token, bz = "10YBE----------2", docType = "A69",
#' start = "201807200000", end = "201907230000", processType = "A01")
#' 
#'
#'  #FR Wind + Solar
#'  readEntsoeXML(token = token, bz = "10YFR-RTE------C", docType = "A69",
#' start = "201807200000", end = "201907230000", processType = "A01")
#'  
#'  
#'  readEntsoeXML(token = token, bz = "10YFR-RTE------C", docType = "A80",
#'    start = "2015-01-01", end = "2018-12-31", fileToMerge = "FRindispo.csv")
#'  readEntsoeXML(token = token, "10YFR-RTE------C", docType = "A77",
#'    start = "2015-01-01", end = "2018-12-31", fileToMerge = "FRindispo.csv")
#'
#'  #DE wind solar some examples
#'  
#'  dtDESolarWind1 <- readEntsoeXML(bz = "10YDE-VE-------2", token = token,
#' docType = "A69", start = "201807200000", end = "201907230000",
#' processType = "A01")
#' 
#'  dtDESolarWind2 <- readEntsoeXML(bz = "10YDE-RWENET---I", token = token,
#' docType = "A69", start = "201807200000", end = "201907230000",
#' processType = "A01",  writeDataDirectory = getwd())
#' 
#'  dtDESolarWind3 <- readEntsoeXML(bz = "10YDE-EON------1", token = token,
#' docType = "A69", start = "201807200000", end = "201907230000",
#' processType = "A01")
#' 
#'  dtDESolarWind4 <- readEntsoeXML(bz = "10YDE-ENBW-----N", token = token,
#' docType = "A69", start = "201807200000", end = "201907230000",
#' processType = "A01", writeDataDirectory = getwd(),
#' fileName = "solarwindDEpart4.rds")
#' 
#'  dtDEWind1 <- readEntsoeXML(bz = "10YDE-VE-------2", token = token,
#' docType = "A69", start = "201807200000", end = "201907230000",
#' processType = "A01", psrType = "B19",  writeDataDirectory = tempdir(),
#' fileName = "windDEpart1.csv") 
#'    
#'  dtDESolar2 <- readEntsoeXML(bz = "10YDE-RWENET---I", token = token,
#' docType = "A69", start = "201807200000", end = "201907230000",
#' processType = "A01", psrType = "B16",  writeDataDirectory = getwd())
#'    
#'    
#' }
#' 
#' @import XML magrittr
#' @export



readEntsoeXML <- function(request = NULL, bz = NULL, outDomain = NULL, 
                          docType = NULL, docStatus = NULL, timeStart = NULL,
                          timeEnd = NULL, processType = NULL, psrType = NULL,
                          token = NULL, entsoeHttps = "https://transparency.entsoe.eu/api?", 
                          writeDataDirectory = NULL,
                          fileName = NULL) {
  tempdir <- tempdir()
  
  ## Request construction part
  if(is.null(request)) {
    if(!is.null(bz)) {
      bzpaste <- paste0("In_Domain=", bz)
    } else {
      stop("The Bidding zone can't be null")
    }
    if(!is.null(outDomain)) {
      outDomain <- paste0("Out_Domain=", outDomain)
    } 
    if(!is.null(docType)) {
      docType <- paste0("DocumentType=", docType)
    } else {
      stop("The document type can't be null")
    }
    if(!is.null(timeStart)) {
      timeStart <- paste0("periodStart=", timeStart)
    } else {
      stop("The starting time can't be null")
    }
    if(!is.null(timeEnd)) {
      timeEnd <- paste0("periodEnd=", timeEnd)
    } else {
      stop("The ending time can't be null")
    }
    if(!is.null(processType)) {
      processType <- paste0("processType=", processType)
    } 
    if(!is.null(psrType)) {
      psrType <- paste0("psrType=", psrType)
    } 
    if(!is.null(token) & token != "Mytoken") {
      token <- paste0("securityToken=", token)
    } else {
      stop(paste("The token can't be null and Mytoken is not a token.", 
                 "You can get a token by subscribing to the entsoe website"))
    }
    if(is.null(entsoeHttps) & token != "Mytoken") {
      stop(paste("The entsoe https link can't be NULL, by default :", 
                 "https://transparency.entsoe.eu/api?"))
    }
    # creation of the request
    request <- paste0(entsoeHttps, docType, "&", token, "&", bzpaste, "&", 
                      outDomain, "&", timeStart,
                      "&", timeEnd, "&", processType, "&", psrType)
    request <- gsub("&{2,}", "&", request)
    request <- gsub("&$", "", request)
  }
  # downloading of the xml file
  download.file(request, destfile = paste0(tempdir, "/request.xml"))
  xmlParsed <- xmlParse(paste0(tempdir, "/request.xml"))
  
  # if (class(request) == "character") {
  #   if(grepl("^https", request)) {
  #     download.file(request, destfile = paste0(tempdir, "/res.xml"))
  #     xmlParsed <- xmlParse(paste0(tempdir, "/res.xml"))
  #   }
  # } else {
  #   xmlParsed <- xmlParse(request)
  # }
  
  # recuperation of the node
  ns <- c("a" = xmlNamespaceDefinitions(xmlParsed, simplify = TRUE))
  # Recuperation of the children nodes Period and Mkt
  outtsPeriod <- getNodeSet(xmlParsed, '/a:GL_MarketDocument/a:TimeSeries/a:Period', ns,
                            addFinalizer = FALSE)
  
  outtsMkt <- getNodeSet(xmlParsed, '/a:GL_MarketDocument/a:TimeSeries/a:MktPSRType', ns,
                         addFinalizer = FALSE)
  # Command to check if there are one or more type of data (wind and solar for ex)
  Mkt <- unname(unlist(lapply(outtsMkt, function(Mkt) {
    xmlElementsByTagName(Mkt, "psrType")$psrType %>%
      xmlValue %>%
      data.table
  })))

  ### Because of Mkt, it is needed to use the lapply on the indices instead of
  ### the elements
  dtres <- rbindlist(lapply(1:length(outtsPeriod), function(out) {
    timeInterval <- xmlElementsByTagName(outtsPeriod[[out]], "timeInterval")$timeInterval
    start <- xmlElementsByTagName(timeInterval, "start")$start %>%
      xmlValue()
    
    resolution <- xmlElementsByTagName(outtsPeriod[[out]], "resolution")$resolution %>%
      xmlValue()
    
    ### Recuperation of the position values by day
    Point <- xmlElementsByTagName(outtsPeriod[[out]], "Point")
    dtPoints <- rbindlist(lapply(Point, function(pt) {
      position <- xmlElementsByTagName(pt, "position")$position %>%
        xmlValue
      quantity <- xmlElementsByTagName(pt, "quantity")$quantity %>%
        xmlValue
      data.table(position = position, quantity = quantity)
    }))
    if (out == 1) {
      maxPos <<- max(as.numeric(dtPoints$position))
    }
    if (maxPos == 23 | maxPos == 25) {
      maxPos <<- 24
    }
    if (maxPos == 92 | maxPos == 100) {
      maxPos <<- 96
    }
    Date <- .dateTreatment(maxPos, dtPoints, start)
    
    data.table(Date = Date, psrType = rep(Mkt[out], length(Date)), resolution, dtPoints)
  }))
  dtres[, Date := as.POSIXct(Date, format = "%Y-%m-%dT%H:%M", tz = "UTC")]
  dtres[, quantity := as.numeric(quantity)]
  dtres[, BiddingZone := bz]
  setcolorder(dtres, "BiddingZone")
  if(!is.null(writeDataDirectory)) {
    if(is.null(fileName)) {
      fileName <- paste0(Sys.Date(), "_", gsub("In_Domain=", "", bz), ".rds")
    }
    if(grepl(".rds$", fileName)) {
      saveRDS(dtres, file = paste0(writeDataDirectory, "/", fileName))
    } else if(grepl(".csv$", fileName)) {
      fwrite(dtres, file = paste0(writeDataDirectory, "/", fileName), sep = "|")
    }
  }
  dtres
}




