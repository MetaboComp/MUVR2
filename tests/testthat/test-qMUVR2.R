test_that("qMUVR works", {

  regrModel <- MUVR2(X = XRVIP2,
                     Y = YR2)
  ## An error will occur if you use different number of observations for X and Y
  expect_error(  regrModel <- MUVR2(X = XRVIP2,
                                    Y = Yotu))



})


