context("Function .fromBtoAntares")

test_that(".fromBtoAntares", {
  
  data <- .readPTDF(PTDF = system.file("testdata/2019-07-17ptdf.csv", package = "fbAntares"))
  col_ptdf <- colnames(data)[grep("ptdf", colnames(data))]
  
  areaConf <- .getAreaName("cwe_at")
  
  
  output <- .fromBtoAntares(data, col_ptdf, areaConf = areaConf)
  
  expect_true(ncol(output) == 7)
  expect_true(nrow(output) > 0)
  expect_true("data.table" %in% class(output))
  
  areaConf <- .getAreaName("cwe")
  output2 <- .fromBtoAntares(data, col_ptdf, areaConf = areaConf)
  
  expect_true(ncol(output2) == 6)
  expect_true(nrow(output2) > 0)
  expect_true("data.table" %in% class(output2))
  expect_error(.getAreaName("CWE"))
})
