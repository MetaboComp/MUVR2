test_that("rdcvNetParams works", {
  # Standard parameters for random forest


  ### An error will be generated when the family is not correctly specified with lowercase letter
  expect_error(methParam <- rdcvNetParams(family="Gaussian"))





})
