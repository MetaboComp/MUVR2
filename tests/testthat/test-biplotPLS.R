test_that("biplotPLS works", {
  nRep <- 2 # Number of MUVR2 repetitions
  nOuter <- 3 # Number of outer cross-validation segments
  varRatio <- 0.75 # Proportion of variables kept per iteration
  method <- 'PLS' # Selected core modeling algorithm

  #### The biplotPLS() will not work if one has run the MUVR2 using randomForest
  method <- 'RF'
  regrModel <- MUVR2(X = XRVIP2,
                     Y = YR2,
                     nRep = nRep,
                     nOuter = nOuter,
                     method = method)
  expect_error(biplotPLS(regrModel$Fit[[2]]))

})
