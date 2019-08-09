context("graphs")

test_that("graphs", {
  
  domainesFB <- readRDS(system.file("testdata/domainesFBReduce.rds", package = "fbAntares"))
  graph <- graphFlowBased2D(flowbased = domainesFB, ctry1 = "FR", ctry2 = "NL", 
                            hour = 5, dayType = 1)
  expect_true("htmlwidget" %in% class(graph))
  
  graph2 <- graphFlowBased2D(flowbased = domainesFB, ctry1 = "ptdfFR", ctry2 = "DE", 
                            hour = NULL, dayType = NULL)
  expect_true("htmlwidget" %in% class(graph2))
  
  graph3 <- graphFlowBased2D(flowbased = domainesFB, ctry1 = "NL", ctry2 = "ptdfAT", 
                             hour = NULL, dayType = NULL)
  expect_true("htmlwidget" %in% class(graph3))
  
  
   fb_opts <- setFlowbasedPath(path = system.file("input/model/antaresInput/", package = "fbAntares"))
   
   out <- plotFB(dayType = 1, hour = 1, country1 = "FR", country2 = "NL",
   fb_opts = fb_opts, areaName = "cwe-at")
   expect_true("combineWidgets" %in% class(out))
   
   out <- plotFB(dayType = 1, hour = 1:4,country1 = "FR",country2 = "NL",
   fb_opts = fb_opts, areaName = "cwe")
   expect_true("combineWidgets" %in% class(out))
   
   out <- plotFB(dayType = 1, hour = 1:2, country1 = "DE", country2 = "AT",
   fb_opts = fb_opts, areaName = "cwe-at")
   expect_true("combineWidgets" %in% class(out))
   
   out <- plotFB(dayType = 1, hour = 1, country1 = c("FR", "DE"),
       country2 = c("NL", "FR"), fb_opts = fb_opts, areaName = "cwe-at")
  expect_true("combineWidgets" %in% class(out))
})

# test_that("test plotNetPositionFB", {
#   
#   
#   dta <- antaresRead::readAntares(areas = c("fr", "be", "de", "nl"), 
#                                   links = c("be - de","be - fr","be - nl",
#                                             "de - fr","de - nl"), mcYears = 2,
#                                   select = c("LOLD", "UNSP. ENRG", 
#                                              "DTG MRG", "UNSP. ENRG", "BALANCE", "FLOW LIN."),
#                                   opts = testSt , showProgress = FALSE)
#   
#   ## plot a domain and the matching output points 
#   res <- plotNetPositionFB(fb_opts = testSt, 
#                            data = dta,
#                            dayType = 1, hour =19:20, 
#                            country1 = "BE", country2 = "FR")
#   
#   
#   expect_true("combineWidgets" %in% class(res))
# })
