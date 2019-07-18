context("Function setDiffNotWantedPtdf")

test_that("setDiffNotWantedPtdf", {
  library(data.table)
  PLAN <- data.table("ptdfFR" = seq(900,1000, 4), "ptdfDE" = seq(600,650, 2),
                     "ptdfNL" = seq(300, 400, 4), "bla" = seq(500, 525, 1))
  plan_res <- setDiffNotWantedPtdf(PTDF = PLAN, hubDrop = list(
    NL = c("DE", "FR")))
  
  expect_true(all(plan_res[['bla']] == PLAN[['bla']]))
  expect_true(all(plan_res[['ptdfFR']] == PLAN[['ptdfFR']] - PLAN[['ptdfNL']]))
  expect_true(all(plan_res[['ptdfDE']] == PLAN[['ptdfDE']] - PLAN[['ptdfNL']]))
  expect_true(is.null(plan_res$ptdfNL))
  
})
