test_that("sampling_from_distribution works", {

  data(freelive2)

  ## A warning will be given when specifying upper/lowerlimit and extend at the same time

      warnings <- capture_warnings({
        sampling_from_distribution(
          YR2,
          upperlimit = 200,
          lowerlimit = 0,
          extend=0.1,
          n = length(YR2)
        )
      })
      expect_length(warnings, 2)
      expect_match(warnings[1], "The upper boundary is the smallest value between upperlimit and the upper value generated from extend argument")
      expect_match(warnings[2], "The lower boundary is the largest value between lowerlimit and the lower value generated from extend argument")



})
