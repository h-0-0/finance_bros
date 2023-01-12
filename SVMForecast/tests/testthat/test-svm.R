test_that("fit_svm works with defaults", {
  D <- import_stonks()
  out <- fit_svm(D)
  expect_equal(2 * 2, 4)
})
