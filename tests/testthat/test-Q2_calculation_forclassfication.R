test_that("Q2_calculation_forclassification works", {
  data("mosquito")
  actual <- Yotu
  predicted <- MUVR2::sampling_from_distribution(actual)
  set.seed(1)

  #' a<-MUVR2(Xotu[1:13,],factor(Yotu[1:13]),method="PLS",DA=T,modReturn = T)
  #' classification_prediction_matrix<-a$yPred$min
  #' y<-as.factor(as.character(Yotu[1:13]))
  #' b<-MUVR2(Xotu,factor(Yotu),method="PLS",DA=T,modReturn = T)
  #' classification_prediction_matrix<-b$yPred$min
  #' y<-as.factor(as.character(Yotu))
  expect_equal(Q2_calculation_forclassification(actual, predicted),
               37.14 )
})

