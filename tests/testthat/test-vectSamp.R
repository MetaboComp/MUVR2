test_that("vectSamp works", {
  data("mosquito")
  n<-5
  vectSamp_list<-vectSamp(Yotu,n)
  expect_equal(length(vectSamp_list), n)
})
