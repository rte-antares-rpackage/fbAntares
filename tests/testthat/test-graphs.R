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
})
