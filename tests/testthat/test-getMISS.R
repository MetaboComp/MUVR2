test_that("getMISS works", {
  data("mosquito")
  actual <- Yotu
  set.seed(1)
  predicted <- sampling_from_distribution(actual)


  expect_equal( getMISS(actual, predicted),
                6)
})
