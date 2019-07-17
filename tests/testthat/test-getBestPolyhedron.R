context("Function getBestPolyhedron")

test_that("getBestPolyhedron", {
  
  polyhedra <- readRDS(system.file("testdata/polyhedra.rds", package = "fbAntares"))
  A <- polyhedra[Date == "2019-02-14"]
  B <- polyhedra[Date == "2019-02-15"]
  reslin <- getBestPolyhedron(A, B, nbLines = 10000, maxiter = 5, 
                             thresholdIndic = 0.75, quad = F)
  resquad <- getBestPolyhedron(A, B, nbLines = 10000, maxiter = 5, 
                             thresholdIndic = 0.75, quad = T)
  expect_true(nrow(reslin) == nrow(B))
  expect_true(nrow(resquad) == nrow(B))
  expect_true(ncol(reslin) == (ncol(B)+1))
  expect_true(ncol(resquad) == (ncol(B)+1))
  expect_true(all(colnames(reslin)[1:(ncol(reslin)-1)] == colnames(B)))
  expect_true(all(colnames(resquad)[1:(ncol(resquad)-1)] == colnames(B)))
  res <- getBestPolyhedron(A, B, nbLines = 10000, maxiter = 1, 
                               thresholdIndic = 0.95, quad = T)
  
  expect_true(nrow(res) == nrow(B))
  expect_true(all(colnames(res)[1:(ncol(res)-1)] == colnames(B)))
})
