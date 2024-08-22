# data("freelive2")
# data("mosquito")
test_that("checkinput works", {



## When using a numeric outcome and the prediction metric is for categorical variables, it will gives a warning
  expect_warning(checkinput(X = XRVIP2,
                              Y = YR2,  ## YR2 a numeric variable
                              DA=FALSE,
                              fitness="BER"))

  ## When using a numeric outcome and discrimination analysis is sepcified as TRUE, it will gives a error
  expect_error(  checkinput(X = XRVIP2,
                            Y = YR2,  ## YR2 a numeric variable
                            DA=TRUE,
                            fitness="BER"))

})
