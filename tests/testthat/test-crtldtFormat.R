context("Function .crtldtFormat")

test_that(".crtldtFormat", {
  
  expect_error(.crtldtFormat(data.table()))
  expect_error(.crtldtFormat("toto"))

})
