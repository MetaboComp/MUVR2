test_that("qMUVR works", {


  ## An error will occur if you use different number of observations for X and Y
  expect_error(  regrModel <- qMUVR2(X = XRVIP2,
                                    Y = Yotu))



})


