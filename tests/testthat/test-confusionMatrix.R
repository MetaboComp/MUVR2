test_that("confusionMatrix works", {
  nRep <- 2 # Number of MUVR2 repetitions
  nOuter <- 3 # Number of outer cross-validation segments
  varRatio <- 0.75 # Proportion of variables kept per iteration
  method <- 'RF' # Selected core modeling algorithm

  regrModel <- MUVR2(
    X = XRVIP2,
    Y = YR2,
    nRep = nRep,
    nOuter = nOuter,
    varRatio = varRatio,
    method = method
  )
  ## An error will occur if the MUVR Object is from a regression analysis.
  ## It needs to be from classification analysis
  expect_error(confusionMatrix(regrModel))

})
