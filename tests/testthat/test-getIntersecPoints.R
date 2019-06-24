context("Function .getIntersecPoints")

test_that(".getIntersecPoints", {
  
  dtLines <- .getNormalizedLines(nbLines = 10000, dim = 4)
  polyhedra <- readRDS(system.file("testdata/polyhedra.rds", package = "fbAntares"))
  A <- polyhedra[Date == "2019-02-14"]
  dtIntersec <- .getIntersecPoints(dtLines = dtLines, PLAN = A)
  
  expect_true("data.table" %in% class(dtIntersec))
  expect_true(nrow(dtIntersec) == 10000)
  expect_true(ncol(dtLines) == 4)
  expect_true(all(dtIntersec$X1 == dtIntersec$lambda*dtIntersec$Line_Coo_X1) & 
                all(dtIntersec$X2 == dtIntersec$lambda*dtIntersec$Line_Coo_X2))
  expect_true(all(c("Face", "lambda", paste0("Line_Coo_X", 1:4), paste0("X", 1:4),
                paste0("ptdf", c("AT", "BE", "DE", "FR")), "Date", 
                "Period", "ram", "distOrig") %in% colnames(dtIntersec)))
  expect_true(all(dtIntersec$lambda > 0))
  expect_true(all(dtIntersec[order(Line_Coo_X1, Line_Coo_X2), Line_Coo_X3] == 
                    dtLines[order(Line_Coo_X1, Line_Coo_X2), Line_Coo_X3]))
})
