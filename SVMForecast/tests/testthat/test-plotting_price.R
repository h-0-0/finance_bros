D <- SVMForecast::import_stonks(day_lag = c(1))

test_that("long_format works", {
  lfD <- long_format(D)
  check <- D
  check["Date"]  <- as_date(rownames(check))
  expect_equal(lfD, check)
})
