test_that("plsInner works", {
  data("freelive2")

  xTrain<-XRVIP2[1:40,]
  yTrain<-YR2[1:40]
  xVal<-XRVIP2[41:nrow(XRVIP2),]
  yVal<-YR2[41:length(YR2)]

plsInner_object<-
    plsInner(xTrain=xTrain,
            yTrain=yTrain,
            xVal=xVal,
            yVal=yVal,
            fitness="RMSEP")
  expect_length(plsInner_object,3)


  ### Need to specify training data
  expect_error(

      plsInner(#xTrain=xTrain,
              yTrain=yVal,
              xVal=xVal,
              yVal=yVal,
              fitness="RMSEP")
  )
})
