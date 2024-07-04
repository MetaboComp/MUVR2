test_that("H0_test works", {

## Only option "resampling" and "permutation" is allowed for type
expect_error(H0_test(classModel,
                     type ="resample"))
})
