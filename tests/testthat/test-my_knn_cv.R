test_that("k-nearest neighbors cross validation works", {
  expect_equal(my_knn_cv(data.frame("a" = c(1:50), "b" = c(1:50), "c" = c(1:50)), "a", 5, 5)[[1]], .95, tolerance = .1)
})
