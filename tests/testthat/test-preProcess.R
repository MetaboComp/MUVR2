test_that("preProcess works", {
  data("freelive2")

  ## The trans shoulw be either 'log', 'sqrt', 'none'
  expect_error(
  preProcess(XRVIP2,
             trans="center")
  )
})
