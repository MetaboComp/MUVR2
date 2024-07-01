# data("freelive2")
# data("mosquito")
test_that("rdCV works", {

  nRep <- 2 # Number of MUVR2 repetitions
  nOuter <- 3 # Number of outer cross-validation segments
  varRatio <- 0.75 # Proportion of variables kept per iteration
  method <- 'RF' # Selected core modeling algorithm

  ## An error will occur if you use different number of observations for X and Y
  expect_error(MUVR2(X = XRVIP2,
                     Y = Yotu,
                     nRep = nRep,
                     nOuter = nOuter,
                     method = method))

})
