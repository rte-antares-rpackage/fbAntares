context("Function .ctrlHubDrop")

test_that(".ctrlHubDrop", {
  library(data.table)
  
  PTDF <- fread(system.file("testdata/2019-07-18ptdfraw.csv", package = "fbAntares"))
  hubDrop <- list(NL = c("BE", "DE", "FR", "AT"))
  
  .ctrlHubDrop(hubDrop = hubDrop, PTDF = PTDF)
  
  expect_error(.ctrlHubDrop(list(NL = c("BE", "DE", "FR")), PTDF), fixed = T,
               regexp = "hubDrop does not contain all the ptdf in PTDF")
  
  expect_warning(.ctrlHubDrop(list(NL = c("BE", "DE", "FR", "AT", "UK")), PTDF), fixed = T,
               regexp = "ptdfUK is (are) not in ptdf name")
  
})