library(e1071)

test_that("fit_svm works with defaults", {
  D <- import_stonks()
  out <- fit_svm(D)
  expect_equal(out$SV, svm(BTC_USD~ ETH_USD_1 + DOGE_USD_1, D, type="eps-regression")$SV)
})


test_that("fit_svm works with setting params", {
  D <- import_stonks()
  out1 <- fit_svm(D, "BTC_USD~ DOGE_USD_1", gamma= 1, C=2, eps=0.2, k_cross=10)
  out2 <- svm(BTC_USD~ DOGE_USD_1, D, type="eps-regression", kernel="radial", gamma=1, cost=2, epsilon=0.2, cross=10)
  expect_equal(out1$formula, out2$formula)
  expect_equal(out1$type, out2$type)
  expect_equal(out1$kernel, out2$kernel)
  expect_equal(out1$gamma, out2$gamma)
  expect_equal(out1$cost, out2$cost)
  expect_equal(out1$epsilon, out2$epsilon)
  expect_equal(out1$cross, out2$cross)

  expect_equal(out1$SV, out2$SV )
})
