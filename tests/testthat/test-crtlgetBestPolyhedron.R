context("Function .crtlgetBestPolyhedron")

test_that(".crtlgetBestPolyhedron", {
  
  expect_error(.crtlgetBestPolyhedron(maxiter = -1, thresholdIndic = 0.5))
  expect_error(.crtlgetBestPolyhedron(maxiter = 3, thresholdIndic = 1.2))
  expect_warning(.crtlgetBestPolyhedron(maxiter = 3.5, thresholdIndic = 0.5))
  expect_silent(.crtlgetBestPolyhedron(maxiter = 5, thresholdIndic = 0.8))
})