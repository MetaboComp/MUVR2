test_that("rfInner works", {
  data("freelive2")

  xTrain<-XRVIP2[1:40,]
  yTrain<-YR2[1:40]
  xVal<-XRVIP2[41:nrow(XRVIP2),]
  yVal<-YR2[41:length(YR2)]

  rfInner_object<-
    rfInner(xTrain=xTrain,
          yTrain=yTrain,
          xVal=xVal,
          yVal=yVal,
          mtry=150,
          ntree=150,
          fitness="RMSEP")
  expect_length(rfInner_object,2)


  ### The xTrain and yTrain is supposed to have same number of observations
  expect_error(
  rfInner_object<-
    rfInner(xTrain=xTrain,
            yTrain=yVal,
            xVal=xVal,
            yVal=yVal,
            mtry=150,
            ntree=150,
            fitness="RMSEP")
  )
})
