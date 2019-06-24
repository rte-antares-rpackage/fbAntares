context("Function .crtlgetNormalizedLines")

test_that(".crtlgetNormalizedLines", {
  
  expect_error(.crtlgetNormalizedLines(nbLines = 100, dim = 1))
  expect_error(.crtlgetNormalizedLines(nbLines = -4, dim = 5))
  expect_error(.crtlgetNormalizedLines(nbLines = "toto", dim = 5))
})