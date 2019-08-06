context(".crtlFixFaces")

test_that(".crtlFixFaces",{
  
  fixFaces = data.table(func = c("min", "min", "max", "min"),
                        zone = c("BE", "FR", "DE", "DE"))
  col_ptdf <- c("ptdfAT", "ptdfBE", "ptdfDE", "ptdfFR")
  expect_silent(.crtlFixFaces(fixFaces, col_ptdf))
  fixFaces2 <-rbindlist(list(fixFaces, data.table(func = "min", zone = "SP")))
  expect_error(.crtlFixFaces(fixFaces2, col_ptdf))
  fixFaces3 <-rbindlist(list(fixFaces, data.table(func = "Min", zone = "FR")))
  expect_error(.crtlFixFaces(fixFaces3, col_ptdf2))
})