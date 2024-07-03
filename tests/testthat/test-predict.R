test_that("predict works", {

  data(freelive2)
  data("mosquito")
  object<-MUVR2::pls(XRVIP2,YR2)

  ### Need to have same number of observations
  expect_error(  predict(object,Xotu))

  nearZeroVar_object<-nearZeroVar(Xotu)
  Xotu2<-Xotu[,-c(nearZeroVar_object$Position)]
  Yotu2<-Yotu[-nearZeroVar_object$Position]
  object<-MUVR2::plsda(Xotu2,Yotu2)
  ### Need to have same number of variables
  expect_error(  predict(object,Xotu2[,-1]))
})
