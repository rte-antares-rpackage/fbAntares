context("Function getStatsFBts")

test_that("getStatsFBts", {
  
  ts <- fread(system.file("testdata/antaresInput/ts.txt", package = "fbAntares"),
              header = T)
  interSeasonBegin <-  c("2028-10-01", "2029-03-16")
  interSeasonEnd <- c("2028-10-31", "2029-05-15")
  statsFBts <- getStatsFBts(ts, interSeasonBegin, interSeasonEnd)
  expect_true(class(statsFBts) == "list")
  expect_true("data.table" %in% class(statsFBts[[1]]) &
                "data.table" %in% class(statsFBts[[2]]))
  
  ts2 <- copy(ts)
  ts2[1, 2] <- 12 
  expect_error(getStatsFBts(ts2, interSeasonBegin, interSeasonEnd))
  ts2 <- copy(ts)
  ts2[3, 2] <- 12
  expect_error(getStatsFBts(ts2, interSeasonBegin, interSeasonEnd))
  ts2 <- copy(ts)
  ts2[253, 2] <- 12
  expect_error(getStatsFBts(ts2, interSeasonBegin, interSeasonEnd))
})