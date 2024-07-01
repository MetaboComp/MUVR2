test_that("MUVR2_EN works", {

  nRep <- 2 # Number of MUVR2 repetitions
  nOuter <- 4 # Number of outer cross-validation segments
  method <- 'RF' # Selected core modeling algorithm

  ## An error will occur if you use different number of observations for X and Y
  expect_error(classModel <- MUVR2_EN(X = Xotu,
                                      Y = YR2,
                                      nRep = nRep,
                                      nOuter = nOuter,
                                      DA = TRUE))

  nOuter <- 3 # Number of outer cross-validation segments

  ## An error will occur if your nOuter is not bigger than the number of categories in your categorical outcome
  expect_error(classModel <- MUVR2_EN(X = Xotu,
                                      Y = Yotu,
                                      nRep = nRep,
                                      nOuter = nOuter,
                                      DA = TRUE))

})
