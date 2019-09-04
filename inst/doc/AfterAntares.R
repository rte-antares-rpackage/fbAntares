## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Set Antares study path and read the simulation results
#  study <- "MyStuDy"
#  opts <- antaresRead::setSimulationPath(study, 2)
#  dta <- antaresRead::readAntares(
#    areas = c("fr", "be", "de", "nl", "at"),
#    links = c("be - de","be - fr","be - nl",
#              "de - fr","de - nl", "at - de"), mcYears = 1:10,
#    select = c("LOLD", "UNSP. ENRG",
#               "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#    opts = opts)
#  
#  
#  # Add to the results the time series of typical flow-based days
#  dta <- addTypicalDayId(dta)
#  
#  
#  
#  
#  # Visualise the flow-based exchanges (position in the flow-based domains)
#  # Plot a domain and the matching output positions
#  plotNetPositionFB(fb_opts = opts,
#                    data = dta, areaName = "cwe_at",
#                    dayType = 1, hour = c(0, 19),
#                    country1 = "BE", country2 = "FR")
#  
#  # Run a shiny application to visualise the domains and matching output positions
#  runAppPosition(dta)
#  
#  
#  # Add to the results a calculation of the net position of the countries within an area (by default, position in CWE)
#  dta <- addNetPosition(data, opts = antaresRead::simOptions(),
#                        inAreas = c("be",  "de", "fr", "nl", "at"), newName = "_CWEAt")
#  
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#  antaresRead::setSimulationPath("MyStuDy", 1)
#  dta <- readAntares(areas = "all", links = "all", clusters = "all" ,mcYears = 1:10)
#  dta <- addTypicalDayId(data = dta)

## ---- eval=FALSE---------------------------------------------------------
#  opts <- antaresRead::setSimulationPath("MyStuDy", 2)
#  dta <- antaresRead::readAntares(
#    areas = c("fr", "be", "de", "nl"),
#    links = c("be - de","be - fr","be - nl","de - fr","de - nl"),
#    mcYears = 1:10,
#    select = c("LOLD", "UNSP. ENRG",
#               "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."))

## ---- eval=FALSE---------------------------------------------------------
#  optsVirtual <- antaresRead::setSimulationPath("MyStuDy", 3)
#  
#  dtaVirtual <- antaresRead::readAntares(
#    areas = c("fr", "be", "de", "nl", "at", "zz_flowbased"),
#    links = c("be - zz_flowbased",
#              "fr - zz_flowbased", "nl - zz_flowbased",
#              "de - zz_flowbased", "at - zz_flowbased"), mcYears = 1:2,
#    select = c("LOLD", "UNSP. ENRG",
#               "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#    opts = opts)

## ---- eval=FALSE---------------------------------------------------------
#  ## Plot the typical day 1 at hours 0 and 19 and the matching net positions on a Belgium-France graph
#  plotNetPositionFB(fb_opts = opts,
#                    data = dta,
#                    dayType = 1, hour = c(0, 19),
#                    country1 = "BE", country2 = "FR")

## ---- eval=FALSE---------------------------------------------------------
#  ## Plot the three typical days of summer at hour 19 and the matching net positions
#  plotNetPositionFB(fb_opts = opts,
#                    data = dta,
#                    dayType = c(1,2,3), hour = c(19),
#                    country1 = "BE", country2 = "FR")

## ---- eval=FALSE---------------------------------------------------------
#  ## Plot only one time, without knowing the used flow-based domain
#  dta$areas <- dta$areas[timeId == 5659,]
#  plotNetPositionFB(fb_opts = opts,
#                    data = dta, areaName = "cwe_at",
#                    dayType = "all", hour = "all",
#                    filteringEmptyDomains = TRUE,
#                    country1 = "FR", country2 = "DE")

## ---- eval=FALSE---------------------------------------------------------
#  ## An exemple of authorized filter :
#  idC <- c(antaresRead::getIdCols(dta$areas))
#  idC <- idC[idC!="area"]
#  LOLD <- dta$areas[,lapply(.SD, sum), by = idC, .SDcols = "LOLD"]
#  LOLD <- LOLD[LOLD!=0]
#  LOLD[,LOLD := NULL]
#  
#  # Merge to filter data
#  dta$areas <- merge(dta$areas, LOLD, by =  idC)
#  ## End filter
#  
#  
#  ## plot domains
#  plotNetPositionFB(fb_opts = opts,
#                    data = dta,
#                    dayType = "all", hour = c(19),
#                    country1 = "BE", country2 = "FR", filteringEmptyDomains = TRUE)
#  
#  ## or with the virtual fb area
#  plotNetPositionFB(fb_opts = optsVirtual,
#                    data = dtaVirtual,
#                    dayType = "all", hour = c(19),
#                    country1 = "BE", country2 = "FR", filteringEmptyDomains = TRUE)
#  

## ---- eval=FALSE---------------------------------------------------------
#  runAppPosition(dta)

## ---- eval=FALSE---------------------------------------------------------
#  opts <- antaresRead::setSimulationPath("MyStuDy", 2)
#  data <- readAntares(area = "all", links = "all", mcYears = 1)
#  
#  ##Add net position for CWE
#  data <- addNetPosition(data, opts)
#  
#  ##Add net position for CWE+AT
#  data <- addNetPosition(data, opts
#                         inAreas = c("be", "de", "fr", "nl", "at"), newName = "_CWEAt")

