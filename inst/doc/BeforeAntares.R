## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fbAntares)
library(manipulateWidget)
library(rAmCharts)
library(pipeR)
library(DT)

## ---- eval=FALSE---------------------------------------------------------
#  
#  #Convert domains from PTDF file, save the output in the directory "model1"
#  computeFB(PTDF = system.file("/input/ptdf/ptdfraw.csv", package = "fbAntares"), verbose = 1, nbFaces = 75, outputName = "D:/model1")
#  
#  #Generate reports from the output of computeFB
#  domainesFB <- readRDS("D:/model1/domainesFB.rds")
#  for (day in domainesFB$idDayType) {
#  generateReportFb(dayType = day allFB = domainesFB)
#  }
#  
#  # To check on the modelization results
#  getMaxImportExport(domainesFB)
#  
#  #Set antares study path
#  antaresRead::setSimulationPath("D:/exemple_test", 0)
#  
#  #Create flow-based time series considering their correlations with other inputs of an Antares Study, save the output in the directory "model1"
#  calendar <- system.file("calendar/calendar.txt", package = "fbAntares")
#  createFBTS(probabilityMatrix = probabilityMatrix, multiplier = multiplier,
#  calendar = calendar, firstDay = firstDay, outputPath = "D:/model1")
#  
#  
#  #Set setFlowbased directory path
#  setFlowbasedPath(path = "D:/model1")
#  
#  
#  
#  #Run shiny application to visualize the results of the convertion
#  runAppError()
#  
#  #Initialize the Antares study
#  initFlowBased(scenario = rep(1:200, times = 5))
#  

## ----eval = FALSE--------------------------------------------------------
#  ## Example of computeFB with cwe_at
#  computeFB(PTDF = system.file("/input/ptdf/ptdfraw.csv", package = "fbAntares"),
#            reports = FALSE, areaName = "cwe_at",
#            hubDrop = list(NL = c("BE", "DE", "FR", "AT")),
#            nbFaces = 75, dayType = 1,
#            fixFaces = data.table(func = "min", zone = "BE"))
#  
#  ## Another example with more arguments and faces clustering only on most important hours
#  computeFB(PTDF = system.file("testdata/2019-07-18ptdfraw.csv",
#                               package = "fbAntares"),
#            reports = FALSE, areaName = "cwe_at",
#            hubDrop = list(NL = c("BE", "DE", "FR", "AT")),
#            nbFaces = 75, dayType = 1,
#            clusteringHours = c(7:10, 17:19), nbLines = 50000,
#            maxiter = 20, thresholdIndic = 95,
#            fixFaces = data.table(func = "min", zone = "BE"))
#  

## ----eval = FALSE--------------------------------------------------------
#  ## Example of computeFB with cwe_at and virtual area
#  computeFB(PTDF = system.file("/input/ptdf/ptdfraw.csv", package = "fbAntares"),
#            reports = FALSE, areaName = "cwe_at",
#            hubDrop = list(NL = c("BE", "DE", "FR", "AT")),
#            nbFaces = 75, dayType = 1,
#            fixFaces = data.table(func = "min", zone = "BE"),
#            virtualFBarea = TRUE)

## ------------------------------------------------------------------------
#Generate reports from the output of computeFB
domainesFB <- readRDS(system.file("/input/model/antaresInput/domainesFB.RDS", package = "fbAntares"))

# To check on the modelization results
maxImportExport <- getMaxImportExport(domainesFB, writecsv = F)

# for a good visualisation in this vignette :
DT::datatable(maxImportExport, options = list(scrollX = TRUE))


## ---- eval=FALSE---------------------------------------------------------
#  
#  # build probabilityMatrix with the function getProbability() from the package
#  # fbClust
#  # select an antaresStudy with the function setSimulationPath() from the package antaresRead
#  
#  # rename columns of the probability matrix
#  
#  matProb <- setNamesProbabilityMatrix(probabilityMatrix,
#                                       c("FR_load", "DE_wind", "DE_solar"),
#                                       c("fr@load", "de@wind", "de@solar"))
#  
#  # set installed capacity for Wind (addition of onshore and offshore) and solar
#  multiplier <- data.frame(variable = c("fr@load", "de@wind", "de@solar"),
#                           coef = c(1, 71900, 61900))
#  
#  # set Calendar
#  firstDay <- identifyFirstDay(opts = antaresStudy)
#  calendar <- system.file("calendar/calendar.csv", package = "fbAntares")
#  
#  # create ts.txt in D:/model1
#  ts <- createFBTS(opts = antaresStudy, probabilityMatrix = probabilityMatrix,
#                   multiplier = multiplier, calendar = calendar,
#                   firstDay = firstDay, outputPath = "D:/model1")
#  
#  

## ------------------------------------------------------------------------
ts <- fread(system.file("testdata/antaresInput/ts.txt", package = "fbAntares"), header = T)

statsFBts <- getStatsFBts(ts, calendar = system.file("calendar/calendar.txt", 
                                        package = "fbAntares"), output = "summary")

# for visualization in the vignette :
datatable(statsFBts, options = list(scrollX = TRUE))


## ---- eval = FALSE-------------------------------------------------------
#  # Specify a repository
#  setFlowbasedPath(path = "C:/PATH/TO/INPUT")

## ---- eval = FALSE-------------------------------------------------------
#  runAppError()

## ---- fig.width= 7, fig.height= 7, warning=FALSE-------------------------
# For example, using the short model available in the package
library(fbAntares)
setFlowbasedPath(model = "antaresInput")
plotFB(hour = 5:6, dayType = 1, country1 = c("FR", "DE"),
       country2 = c("DE", "NL"), areaName = "cwe_at", export = T)






## ---- eval=FALSE---------------------------------------------------------
#  antaresRead::setSimulationPath("D:/exemple_test", 0)
#  
#  # initialisation of flow-based study
#  initFlowBased(scenario = rep(1:200, times = 5))

