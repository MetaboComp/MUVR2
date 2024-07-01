test_that("customParams works", {
   # Standard parameters for random forest
   methParam <- customParams() # or

   ### An error will be generated when there is a mismatch between method and rfMethod
   expect_error(methParam <- customParams(method='PLS',
                                          rfMethod = "ranger"))

   ### An error will be generated when the method is not specified correctly as RF
   expect_error(methParam <- customParams(method='ranger',
                                          rfMethod = "ranger"))

   ### An error will be generated when the method and rfMethod are misplaced
   expect_error(methParam <- customParams(method='randomForest',
                                          rfMethod = "RF"))



})
