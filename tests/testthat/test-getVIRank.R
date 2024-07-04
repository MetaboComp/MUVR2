test_that("getVIRank works", {

  ### n should not be bigger than number of variables
expect_error(getVIRank(regrModel,n=2000))
})
