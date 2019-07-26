context("setFlowbasedPath")

test_that("setFlowbasedPath", {

  fbPath <- setFlowbasedPath(path = system.file("testdata/antaresInput", package = "fbAntares"))
  expect_true(class(fbPath) == "flowBasedPath")
  expect_true(length(fbPath) == 1)

  # print(paste("Models :", getAvailableModel()))
  # fbModel <- setFlowbasedPath(model = "model2017")
  # expect_true(class(fbModel) == "flowBasedPath")
  # expect_true(length(fbModel) == 1)

})

