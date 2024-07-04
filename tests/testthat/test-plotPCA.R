test_that("plotPCA works", {
  data("freelive2")
   pca_object<-prcomp(XRVIP2)

   ### The number of principal component should not exceed number of observations and number of variables
   expect_error(
   plotPCA(pca_object,
           PC1=1,
           PC2=min(dim(XRVIP))+1)
   )
})
