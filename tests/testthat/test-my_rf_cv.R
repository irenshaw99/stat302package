test_that("random forest cross-validation works", {
  expect_equal(my_rf_cv(5), 119406.2, tolerance = 5000)
})
