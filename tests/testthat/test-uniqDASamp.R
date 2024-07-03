test_that("uniqDASamp works", {
  data(mosquito)
   Y <- Yotu
   ID <- sample(c(1,1:length(Yotu)),replace = T)
   folds <- 5

   ## the Y length and the ID length is not the same
   expect_error(uniqDASamp(Y, ID, folds))
})
