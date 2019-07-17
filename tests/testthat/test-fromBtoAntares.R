context("Function .fromBtoAntares")

test_that(".fromBtoAntares", {
  
  data <- .readPTDF(PTDF = system.file("testdata/2019-07-17ptdf.csv", package = "fbAntares"))
  col_ptdf <- colnames(data)[grep("ptdf", colnames(data))]
  output <- .fromBtoAntares(data, col_ptdf)
  
  expect_true(ncol(output) == ncol(combn(col_ptdf, 2)))
  expect_true(nrow(output) > 0)
  expect_true("data.table" %in% class(output))
})
