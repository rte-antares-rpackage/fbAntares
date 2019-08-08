context("Function getStatsFBts")

test_that("getStatsFBts", {
  
  ts <- fread(system.file("testdata/antaresInput/ts.txt", package = "fbAntares"),
              header = T)
  interSeasonBegin <-  c("2028-10-01", "2029-03-16")
  interSeasonEnd <- c("2028-10-31", "2029-05-15")
  statsFBts <- getStatsFBts(ts, interSeasonBegin, interSeasonEnd, output = "all")
  expect_true(class(statsFBts) == "list")
  expect_true("data.table" %in% class(statsFBts[[1]]) &
                "data.table" %in% class(statsFBts[[2]]))
  
  statsFBts <- getStatsFBts(ts, interSeasonBegin, interSeasonEnd, output = "summary")
  expect_true("data.table" %in% class(statsFBts))
  statsFBts <- getStatsFBts(ts, interSeasonBegin, interSeasonEnd, output = "yearbyyear")
  expect_true("data.table" %in% class(statsFBts))
  
  ts2 <- copy(ts)
  ts2[1, 2] <- 12 
  expect_error(getStatsFBts(ts2, interSeasonBegin, interSeasonEnd), output = "summary")
  ts2 <- copy(ts)
  ts2[3, 2] <- 12
  expect_error(getStatsFBts(ts2, interSeasonBegin, interSeasonEnd), output = "summary")
  ts2 <- copy(ts)
  ts2[253, 2] <- 12
  expect_error(getStatsFBts(ts2, interSeasonBegin, interSeasonEnd), output = "summary")
  ts2 <- copy(ts)
  ts2[93, 2] <- 11
  expect_error(getStatsFBts(ts2, interSeasonBegin, interSeasonEnd), output = "summary")
  ts2 <- copy(ts)
  ts2[125, 2] <- 11
  expect_error(getStatsFBts(ts2, interSeasonBegin, interSeasonEnd), output = "summary")
  ts2 <- copy(ts)
  ts2[119, 2] <- 1
  expect_error(getStatsFBts(ts2, interSeasonBegin, interSeasonEnd), output = "summary")
})