context("Function .getNormalizedLines")

test_that(".getNormalizedLines", {
  
  nbLines <- 10000
  dim <- 4
  dtLines <- .getNormalizedLines(nbLines, dim)
  expect_true("data.table" %in% class(dtLines))
  expect_true(nrow(dtLines) == 10000)
  expect_true(ncol(dtLines) == 4)
  expect_true(all(dtLines <= 1) & all(dtLines >= -1))
})
