test_that("confusionMatrix works", {

  ## An error will occur if the MUVR Object is from a regression analysis.
  ## It needs to be from classification or multilevel analysis
  expect_error(confusionMatrix(regrModel))

})
