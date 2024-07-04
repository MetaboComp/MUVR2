test_that("vip works", {
   data("freelive2")
   object <- prcomp(XRVIP2)
   ### The object needs to be a MUVR pls(da) object
   expect_error(
   vip(object)
   )
})
