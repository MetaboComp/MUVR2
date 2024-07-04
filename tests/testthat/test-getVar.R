test_that("getVar works", {


  #### Only models run by elastic net will use the get Var function
  expect_error(
  regrModel<-getVar(regrModel)
  )
  expect_error(
    MLModel<-getVar(MLModel)
  )
})
