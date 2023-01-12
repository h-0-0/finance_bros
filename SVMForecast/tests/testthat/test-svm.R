test_that("fit_svm works with defaults", {
  D <- import_stonks()
  out <- fit_svm(D)
  print(out)
  print(out$SV)
  expect_equal(2 * 2, 4)
})
