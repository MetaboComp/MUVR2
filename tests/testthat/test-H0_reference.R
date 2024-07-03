test_that("H0_reference works", {
 data("freelive2")

  ## The input needs to be a vector
  expect_error(
  H0_reference(XRVIP2)
  )

})
