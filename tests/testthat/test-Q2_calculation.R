test_that("Q2_calculation works", {
  data("freelive2")
  actual <- YR2
  set.seed(1)
  predicted <- MUVR2::sampling_from_distribution(actual)

  expect_equal(round(Q2_calculation(actual, predicted),2),
               -2.37 )
})

