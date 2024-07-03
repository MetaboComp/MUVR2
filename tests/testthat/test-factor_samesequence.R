test_that("factor_samesequence works", {
  data("freelive2")

  ## the input needs to be a factor variable
  expect_warning(
  factor_samesequence(YR2)
  )
  })
