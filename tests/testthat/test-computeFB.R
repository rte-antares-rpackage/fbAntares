context("computeFB")
library(fbAntares)
test_that("computeFB",{
  suppressWarnings(allFB <- computeFB(
    PTDF = system.file("testdata/2019-07-18ptdfraw.csv", package = "fbAntares"),
    dayType = 2, hour = 1, reports = FALSE, outputName = tempdir(),
    nbLines = 10000, maxiter = 3))
  
  expect_true(file.exists(paste0(allFB, "/second_member.txt")))
  expect_true(file.exists(paste0(allFB, "/weight.txt")))
  expect_true(file.exists(paste0(allFB, "/domainesFB.RDS")))
  
  suppressWarnings(allFB2 <- computeFB(
    PTDF = system.file("testdata/2019-07-18ptdfraw.csv", package = "fbAntares"),
    dayType = 2, hour = 1, reports = FALSE, outputName = tempdir(),
    fixFaces = data.table(func = c("min", "min", "max", "min"), 
                          zone = c("BE", "FR", "DE", "DE")),
    verbose = 2, virtualFBarea = F, thresholdIndic = 90,
    nbLines = 10000))
  
  expect_true(file.exists(paste0(allFB2, "/second_member.txt")))
  expect_true(file.exists(paste0(allFB2, "/weight.txt")))
  expect_true(file.exists(paste0(allFB2, "/domainesFB.RDS")))
  weight <- fread(paste0(allFB2, "/weight.txt"))
  expect_true(!any(grepl("ZZ_flowbased", colnames(weight))))
  
  
  
  suppressWarnings(allFB3 <- computeFB(
    PTDF = system.file("testdata/2019-07-18ptdfraw.csv", package = "fbAntares"),
    dayType = 2, hour = 1, reports = FALSE, outputName = tempdir(),
    fixFaces = data.table(func = c("min", "min", "max", "min"), 
                          zone = c("BE", "FR", "DE", "DE")),
    verbose = 2, virtualFBarea = T, thresholdIndic = 85,
    nbLines = 10000))
  
  expect_true(file.exists(paste0(allFB3, "/second_member.txt")))
  expect_true(file.exists(paste0(allFB3, "/weight.txt")))
  expect_true(file.exists(paste0(allFB3, "/domainesFB.RDS")))
  weight <- fread(paste0(allFB3, "/weight.txt"))
  expect_true(all(grepl("ZZ_flowbased", colnames(weight)[which(colnames(weight) != "Name")])))
})