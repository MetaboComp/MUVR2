test_that("mergeModels works", {
  ### Can only merge regression models, not classification or multilevel
  expect_error(
  mergedModel<-mergeModels(classModel,classModel)
  )
  expect_error(
    mergedModel<-mergeModels(MLModel,classModel)
  )
  expect_error(
    mergedModel<-mergeModels(MLModel,MLModel)
  )
})
