context("Function initFlowBased")

#Untar and read study
testStudy <- system.file("testdata/exemple_test.tgz",package = "fbAntares")
temp_dir <- tempdir()
untar(testStudy, exdir = temp_dir)
testStudy <- paste0(temp_dir, "/exemple_test")
opts <- suppressWarnings(antaresRead::setSimulationPath(testStudy))

test_that("test initFlowBased", {
  opts2 <- antaresRead::setSimulationPath(testStudy2)
  expect_true(suppressWarnings(identifyFirstDay(opts2) == 1))
  expect_true(suppressWarnings(identifyFirstDay(opts2, secondArea = NULL) == 1))
  expect_warning(identifyFirstDay(opts2, secondArea = NULL))

})


test_that("test initFlowBased", {
  opts2 <- suppressWarnings(antaresRead::setSimulationPath(testStudy2))

  fb_opts <- system.file("input/model/antaresInput", package = "fbAntares")
  if(fb_opts == "") fb_opts <- system.file("testdata/antaresInput", package = "fbAntares")
  init <- try(suppressWarnings(initFlowBased(
    fb_opts = fb_opts, scenarios = rep(1:200, times = 5), 
    opts = opts2, areaName = "cwe")))
  # expect_true(class(init) == "list")
  opts2 <- antaresRead::setSimulationPath(testStudy2)


  expect_true("model_description_fb" %in% antaresRead::getAreas(opts = opts2))


  # ctr <- fread(paste0(fb_opts, "/weight.txt"))
  # bdc <- names(antaresRead::readBindingConstraints(opts2))
  # expect_true(all(paste0(ctr$Name, "_fb") %in% bdc))

  clusters <- antaresRead::readClusterDesc(opts2)
  clusters <- clusters[area == "model_description_fb"]
  expect_true(all(clusters$unitcount == 1))

  # clbdc <- all(clusters$cluster %in% paste0("model_description_fb_", tolower(bdc)))
  # expect_true(clbdc)
  
  
  
  opts2 <- suppressWarnings(antaresRead::setSimulationPath(testStudy2))
  fb_opts2 <- system.file("input/model/antaresInputVirtualFBarea", package = "fbAntares")
  
  if(fb_opts2 == "") fb_opts2 <- system.file("testdata/antaresInput", package = "fbAntares")
  init <- try(suppressWarnings(initFlowBased(
    fb_opts = fb_opts2, scenarios = rep(1:200, times = 5), opts = opts2, areaName = "cwe")))
  # expect_true(class(init) == "list")
  opts2 <- antaresRead::setSimulationPath(testStudy2)
  
  
  expect_true("model_description_fb" %in% antaresRead::getAreas(opts = opts2))
  
  
  # ctr <- fread(paste0(fb_opts, "/weight.txt"))
  # bdc <- names(antaresRead::readBindingConstraints(opts2))
  # expect_true(all(paste0(ctr$Name, "_fb") %in% bdc))
  
  clusters <- antaresRead::readClusterDesc(opts2)
  clusters <- clusters[area == "model_description_fb"]
  expect_true(all(clusters$unitcount == 1))

})
