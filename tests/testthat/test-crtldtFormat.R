context("Function .crtldtFormat")

test_that(".crtldtFormat", {
  
  expect_error(.crtldtFormat(data.table()))
  expect_error(.crtldtFormat("toto"))
  expect_silent(.crtldtFormat(data.table(V1 = rep(1:10))))

})
