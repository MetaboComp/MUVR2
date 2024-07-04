test_that("predMV works", {
  ## The number of variables and variable names in the new data must match with the MUVR object
  expect_error(
    predMV(regrModel,
               XRVIP2[,1:100])
               )
  expect_error(
    predMV(classModel,
           XRVIP2)
  )
})
