test_that("foldVector works", {
 Y <- rep(LETTERS[1:2],10)
  ID <- 1:20
  folds <- 5
  foldList <- uniqDASamp(Y, ID, folds)


  ## Need to have ID specified
  expect_error(
  foldVect <- foldVector(foldList)
  )
   })
