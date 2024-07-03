test_that("plotPerm works", {
  data("freelive2")
  set.seed(1)
  actual <- sample(YR2, 1)
  distribution <- YR2

  ##### need to specify an actual value to test with permutation
  expect_error(
  plotPerm (permutation_distribution=permutation_distribution)
  )
  ### permutation_visualcan only be  "mean", "median", "none"
  expect_error(
    plotPerm (actual=actual,
              permutation_distribution=permutation_distribution,
              permutation_visual ="average")
  )

})
