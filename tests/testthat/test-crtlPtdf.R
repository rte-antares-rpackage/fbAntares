context("Function .crtlPtdf")

test_that(".crtlPtdf", {
  
  polyhedra <- readRDS(system.file("testdata/polyhedra.rds", package = "fbAntares"))
  A <- polyhedra[Date == "2019-02-14"]
  B <- polyhedra[Date == "2019-02-15"]
  col_ptdf <-  .crtlPtdf(A, B)
  expect_true(all(col_ptdf == c("ptdfAT", "ptdfBE", "ptdfDE", "ptdfFR")))
  col_ptdf <-  .crtlPtdf(A, dt2 = NULL)
  expect_true(all(col_ptdf == c("ptdfAT", "ptdfBE", "ptdfDE", "ptdfFR")))
  B[, ptdfDE := NULL]
  expect_error(.crtlPtdf(A, B))
  dt <- data.table(V1 = rep("toto", 10))
  expect_error(.crtlPtdf(dt))
})
