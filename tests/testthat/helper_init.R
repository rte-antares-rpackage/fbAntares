temp_dir <- tempdir()

library(testthat)

#Untar and read study
testStudy2 <- system.file("testdata",package = "fbAntares")
if(testStudy2 == "")testStudy2 <- system.file("inst/testdata",package = "fbAntares")

temp_dir <- tempdir()
if (Sys.info()['sysname'] == "Windows") {
  untar(file.path(testStudy2, "ex_test.tgz"), exdir = temp_dir,
        extras = "--force-local")
  
  untar(file.path(testStudy2, "exemple_test.tgz"), exdir = temp_dir,
        extras = "--force-local")
  
} else {
  untar(file.path(testStudy2, "ex_test.tgz"), exdir = temp_dir)
  untar(file.path(testStudy2, "exemple_test.tgz"), exdir = temp_dir)
  
}


testStudy2 <- file.path(temp_dir, "ex_test")
opts2 <- antaresRead::setSimulationPath(testStudy2)
opts <- opts2
testStudy <- testStudy2
assign("opts2", opts2, envir = globalenv())
assign("opts", opts, envir = globalenv())
assign("testStudy2", testStudy2, envir = globalenv())
assign("testStudy", testStudy, envir = globalenv())

testSt <-  file.path(temp_dir, "exemple_test")
testSt <- antaresRead::setSimulationPath(testSt, 1)
# 
# tar(tarfile = "ex_test.tgz",files = "ex_test",
#     compression = "gzip")

# tar(tarfile = "exemple_test.tgz",files = "exemple_test",
#     compression = "gzip")


###Init adq
opts3 <- list()
opts3$studyPath <- system.file("testdata/adq/antaresStudy37", package = "fbAntares")

if(opts3$studyPath== "") opts3$studyPath <- system.file("inst/testdata/adq/antaresStudy37", package = "fbAntares")
