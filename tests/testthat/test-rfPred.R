test_that("rfPred works", {
  data("freelive2")

  xTrain<-XRVIP2[1:40,]
  yTrain<-YR2[1:40]
  xTest<-XRVIP2[41:nrow(XRVIP2),]
  yTest<-YR2[41:length(YR2)]

  rfPred_object<-
    rfPred(xTrain=xTrain,
            yTrain=yTrain,
            xTest=xTest,
            yTest=yTest,
           DA= FALSE)
  expect_length(rfPred_object,3)


  data("mosquito")
  expect_error(
    rfPred_object<-
      rfPred(xTrain=xTrain,
             yTrain=yTest,
             xTest=xTest,
             yTest=yTest,
             DA= FALSE)

  )
})
