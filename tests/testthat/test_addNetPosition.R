context("addNetPosition")

test_that("computeFB",{
  
  data <- antaresRead::readAntares(
    area = c("fr", "be", "de", "nl", "at"), 
    links = c("be - de","be - fr","be - nl",
              "de - fr","de - nl", "at - de"), mcYears = 1, opts = testSt2)
  data <- addNetPosition(data, opts = testSt2)
  
  
  expect_true(all(data$areas[,sum(Balance_CWEAt), by =  c('timeId', 'mcYear')]$V1 == 0))
  
  ipn1 <- data$areas[!is.na(Balance_CWEAt)]
  ipn12 <- melt(.giveIpn(data, areaName = c("FR", "BE", "DE", "NL", "AT")), id = 1:2)
  ipn1 <- ipn1[ipn1$area %in% c("be", "de", "fr","nl", "at")]
  ipn1 <- droplevels(ipn1)
  expect_true(all(ipn1$Balance_CWEAt == ipn12$value))

  data2 <- addNetPosition(data, opts = testSt2, inAreas = c("fr", "be", "de", "nl"), newName = "_CWE")
  expect_true(all(data2$areas[,sum(Balance_CWE), by =  c('timeId', 'mcYear')]$V1 == 0))
  
})