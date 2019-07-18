context("Function getVertices")

test_that("getVertices", {
  library(data.table)
  
  PLAN <- readRDS(system.file("testdata/plan_new_format_apres_not_wanted_ptdf.rds", package = "fbAntares"))

  VERT <- getVertices(PLAN)
  expect_true(all(colnames(VERT) %in% c("AT", "BE", "DE", "FR", "Date", "Period")) &
                ncol(VERT) == 6)
  expect_true(nrow(VERT) == 5278)
  VERT2 <- getVertices(PLAN, ctrdel = "BE")
  expect_true(all(colnames(VERT2) %in% c("AT", "DE", "FR", "Date", "Period")) &
                ncol(VERT2) == 5)
  expect_true(nrow(VERT2) == 774)
})



