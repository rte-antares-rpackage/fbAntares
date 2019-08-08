context("Function .crtlCountriesCombn")

test_that(".crtlCountriesCombn", {

  countries <- list(c("BE", "FR"), c("BE", "NL"), c("DE", "FR"), c("DE", "AT"))
  dfcountries <- .crtlCountriesCombn(countries)
  expect_true("data.frame" %in% class(dfcountries))
  expect_true(all(dim(dfcountries) == c(4, 2)))
  
  countries2 <- c("BE", "FR", "AT")
  dfcountries2 <- .crtlCountriesCombn(countries2)
  expect_true("data.frame" %in% class(dfcountries2))
  expect_true(all(dim(dfcountries2) == c(3, 2)))
  
  expect_error(crtlCountriesCombn(5))
  expect_error(crtlCountriesCombn(list(c("BE", "FR", "DE"))))
})
  