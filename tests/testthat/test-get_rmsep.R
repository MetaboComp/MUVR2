test_that("get_rmsep works", {
  data("freelive2")
  actual <- YR2
  set.seed(1)
  predicted <- MUVR2::sampling_from_distribution(actual)

  expect_equal(round(get_rmsep(actual, predicted),2),
               37.14 )
})
