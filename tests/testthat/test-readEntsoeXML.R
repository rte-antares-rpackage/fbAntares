context("readEntsoeXML")

test_that("readEntsoeXML",{
  
  token <- "3ea2c90d-8ecb-4452-898a-263ea835f498"
  dtDESolar1 <- readEntsoeXML(bz = "10YDE-VE-------2", token = token,
                               docType = "A69", timeStart = "201807200000", timeEnd = "201807230000",
                               processType = "A01", psrType = "B16", 
                              writeDataDirectory =  temp_dir, fileName = "DESolar.rds")
  
  
  expect_true("data.table" %in% class(dtDESolar1))
  expect_true(ncol(dtDESolar1) == 6)
  expect_true(file.exists(paste0(temp_dir, "/DESolar.rds")))

  
  dtDEWind1 <- readEntsoeXML(bz = "10YDE-VE-------2", token = token,
                              docType = "A69", timeStart = "201807210000", timeEnd = "201807230000",
                              processType = "A01", psrType = "B19", 
                              writeDataDirectory =  temp_dir, fileName = "DEWind.csv")
  
  expect_true(file.exists(paste0(temp_dir, "/DEWind.csv")))
})