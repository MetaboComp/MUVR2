test_that("nearZeroVar works", {

  ### All data needs to be numerice
data("mosquito")
## The input needs to be a dataframe
  expect_error(
  nearZeroVar(Yotu)
  )
})
