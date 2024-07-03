test_that("pls works", {
  data("freelive2")
  data("mosquito")

  #### the object needs to have the same length

  expect_error(
  pls(XRVIP2,
      Yotu)
  )



})
