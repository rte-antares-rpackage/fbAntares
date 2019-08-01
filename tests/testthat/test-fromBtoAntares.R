context("Function .fromBtoAntares")

test_that(".fromBtoAntares", {
  
  data <- .readPTDF(PTDF = system.file("testdata/2019-07-17ptdf.csv", package = "fbAntares"))
  col_ptdf <- colnames(data)[grep("ptdf", colnames(data))]
  output <- .fromBtoAntares(data, col_ptdf, areaName = "cwe_at")
  
  expect_true(ncol(output) == 7)
  expect_true(nrow(output) > 0)
  expect_true("data.table" %in% class(output))
  output2 <- .fromBtoAntares(data, col_ptdf, areaName = "cwe")
  
  expect_true(ncol(output2) == 6)
  expect_true(nrow(output2) > 0)
  expect_true("data.table" %in% class(output2))
  expect_error(.fromBtoAntares(data, col_ptdf, areaName = "CWE"))
})
