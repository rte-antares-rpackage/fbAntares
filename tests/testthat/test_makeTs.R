context("createFBTS")

test_that("make ts", {
  
  op5 <- suppressWarnings(antaresRead::setSimulationPath(testStudy2))
  
  matProb <- readRDS(system.file("testdata/proba.RDS", package = "fbAntares"))
  
  setnames(matProb[[1]],"FR_load", "fr@load" )
  setnames(matProb[[2]],"FR_load", "fr@load" )
  
  setnames(matProb[[1]],"DE_wind", "de@wind" )
  setnames(matProb[[2]],"DE_wind", "de@wind" )
  
  setnames(matProb[[1]],"DE_solar", "be@wind" )
  setnames(matProb[[2]],"DE_solar", "be@wind" )
  
  multiplier <- data.frame(variable = c("fr@load", "de@wind", "be@wind"),
                           coef = c(1, 352250, 246403))
  firstDay <- suppressWarnings(identifyFirstDay(op5, firstArea = "fr", secondArea = NULL))
  
  
  interSeasonBegin <- as.Date(c("2017-09-03", "2018-02-02"))
  interSeasonEnd <- as.Date(c("2017-10-04", "2018-05-02"))
  calendar <- system.file("calendar/calendar.csv", package = "fbAntares")
  firstF <- secondF <-  NULL
  for(k in 1:10)
  {
    ts <- suppressWarnings(createFBTS(opts = op5, probabilityMatrix = matProb, multiplier = multiplier,
                     # interSeasonBegin = interSeasonBegin, interSeasonEnd = interSeasonEnd,
                     calendar = calendar,
                     firstDay = firstDay, seed = k, silent = TRUE, outputPath =  tempdir()))
    
    
    frLoad <- suppressWarnings(antaresRead::readInputTS(load = "fr", timeStep = "daily", showProgress = FALSE))
    windbe <- suppressWarnings(antaresRead::readInputTS(wind = c("be"), timeStep = "daily", showProgress = FALSE))
    windde <- suppressWarnings(antaresRead::readInputTS(wind = c("de"), timeStep = "daily", showProgress = FALSE))
    allDta <- data.table(frLoad, be = windbe[["wind"]],de = windde[["wind"]])
    allDta <- allDta[tsId == 1]
    
    dates <- allDta$time
    calendar2 <- .getVirtualCalendar(dates, interSeasonBegin, interSeasonEnd, firstDay)
    
    data1 <- allDta[180]
   
    firstF <- c(firstF, ts[ts$Date == data1$time]$`1`)
    
    data2 <- allDta[1]
    
    secondF <- c(secondF, ts[ts$Date == data2$time]$`1`)
    
    
  }
  expect_true(4 %in% firstF)
  expect_true(5 %in% firstF)
  expect_true(all(firstF%in%c(4, 5)))
  expect_true(all(secondF == 3))

  
  expect_error(suppressWarnings(createFBTS(opts = op5, probabilityMatrix = matProb, multiplier = "toto",
                                    interSeasonBegin = interSeasonBegin, interSeasonEnd = interSeasonEnd,
                                    firstDay = firstDay, seed = k, silent = TRUE, outputPath =  tempdir())))
  
  
  multiplier1 <- multiplier
  multiplier1$toto <- 1
  expect_error(createFBTS(opts = op5, probabilityMatrix = matProb, multiplier = multiplier1,
                                           interSeasonBegin = interSeasonBegin, interSeasonEnd = interSeasonEnd,
                                           firstDay = firstDay, seed = k, silent = TRUE, outputPath =  tempdir()))
  
  expect_error(createFBTS(opts = op5, probabilityMatrix = matProb, multiplier = multiplier,
                          interSeasonBegin = interSeasonBegin, interSeasonEnd = interSeasonEnd,
                          firstDay = 8, seed = k, silent = TRUE, outputPath =  tempdir()))
  multiplier1 <- rbind(multiplier, data.frame(variable = "tot", coef = 1))
  expect_error(createFBTS(opts = op5, probabilityMatrix = matProb, multiplier = multiplier1,
                          interSeasonBegin = interSeasonBegin, interSeasonEnd = interSeasonEnd,
                          firstDay = firstDay, seed = k, silent = TRUE, outputPath =  tempdir()))
  
 setNamesProbabilityMatrix(matProb, "fr@load", "toto")
  expect_true("toto" %in% names(matProb[[1]]))
  expect_true("toto" %in% names(matProb[[2]]))
  
})
