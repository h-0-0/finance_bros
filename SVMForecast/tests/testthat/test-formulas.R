test_that("get_formulas works", {
  columns <- c("NVDA_USD","ETH_USD", "DOGE_USD")
  indices <- list(c(1,2,3), c(1,3), c(2))
  fs <- get_formulas(columns, indices)
  expect_equal(fs, c("BTC_USD~ NVDA_USD+ETH_USD+DOGE_USD", "BTC_USD~ NVDA_USD+DOGE_USD", "BTC_USD~ ETH_USD") )
})
