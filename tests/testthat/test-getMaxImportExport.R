context("Function getMaxImportExport")

test_that("getMaxImportExport", {
  
  domainesFB <- readRDS(system.file("testdata/antaresInput/domainesFB.rds", package = "fbAntares"))
  dtImportExport <- getMaxImportExport(domainesFB)
  expect_true("data.table" %in% class(dtImportExport))
  expect_true(ncol(dtImportExport) == 26)
})