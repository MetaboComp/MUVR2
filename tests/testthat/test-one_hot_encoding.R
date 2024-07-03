test_that("onehotencoding works", {
  data("freelive2")

  ## The input must be a dataframe or matrix
  expect_error(onehotencoding(YR2))
})
