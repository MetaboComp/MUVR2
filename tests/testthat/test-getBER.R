test_that("getBER works", {
  data("mosquito")
  actual <- Yotu
  set.seed(1)

  predicted <- sampling_from_distribution(actual)


  expect_equal( round(getBER(actual, predicted),2),
                0.80)
})
