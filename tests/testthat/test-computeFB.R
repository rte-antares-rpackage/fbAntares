context("computeFB")

test_that("computeFB",{
  suppressWarnings(allFB <- computeFB(PTDF = system.file("testdata/2019-07-17ptdf.csv", package = "fbAntares"),
    dayType = 2, hour = 1, reports = FALSE, outputName = tempdir()))
  
  expect_true(file.exists(paste0(allFB, "/second_member.txt")))
  expect_true(file.exists(paste0(allFB, "/weight.txt")))
  expect_true(file.exists(paste0(allFB, "/domainesFB.RDS")))
})