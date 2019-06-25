context("Function .crtlNumeric .crtlBoolean")

test_that(".crtlNumeric", {
  
  expect_error(.crtlNumeric("toto"))
  expect_silent(.crtlNumeric(10))
  expect_error(.crtlNumeric(c(10, 5)))
  
  expect_error(.crtlBoolean("toto"))
  expect_silent(.crtlBoolean(T))
  expect_error(.crtlBoolean(c(T, F)))
})
