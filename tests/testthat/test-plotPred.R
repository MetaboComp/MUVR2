test_that("plotPred works", {
   data("freelive2")
   Ytrue<-YR2
   Ypreds<-sampling_from_distribution(YR2,n=length(YR2)+1)

   ## the Ytrue and Ypreds should be of same length
   expect_error(
   plotPred(Ytrue,Ypreds)
   )
})
