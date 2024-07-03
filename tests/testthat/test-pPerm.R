test_that("pPerm works", {
  data("freelive2")
  set.seed(1)
  actual <- sample(YR2, 1)
  permutation_distribution <- YR2
  expect_equal(round(pPerm(actual, permutation_distribution)$p, 2), 0.29)
})
