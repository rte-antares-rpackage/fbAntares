context("Function .readPTDF")

test_that(".readPTDF", {
  
  data <- .readPTDF(PTDF = system.file("testdata/2019-07-17ptdf.csv", package = "fbAntares"))
  expect_true("data.table" %in% class(data))
  expect_true(nrow(data) > 0 & ncol(data) > 0)
  expect_error(.readPTDF(PTDF = system.file("testdata/polyhedra.rds", package = "fbAntares")))
})
