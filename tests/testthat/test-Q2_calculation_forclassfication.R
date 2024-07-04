test_that("Q2_calculation_forclassification works", {

  classification_prediction_matrix <- classModel$yPred
  y <- Yotu
  Q2_calculation_forclassification(classification_prediction_matrix,
  y)
  ### There should be one Q2 for each value
  expect_equal(length(Q2_calculation_forclassification(classification_prediction_matrix,y)),
               length(levels(y)) )
})

