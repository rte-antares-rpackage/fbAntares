context("setFlowbasedPath")

test_that("setFlowbasedPath", {
  
  fbPath <- setFlowbasedPath(path = system.file("testdata/antaresInput", package = "fbAntares"))
  
  expect_true(class(fbPath) == "flowBasedPath")
  expect_true(length(fbPath) == 1)
})