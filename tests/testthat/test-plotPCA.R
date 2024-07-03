test_that("plotPCA works", {
  data("freelive2")
   pca_object<-principal(XRVIP2)

   ### The object is expected to be from prcomp
   expect_error(
   plotPCA(pca_object)
   )
})
