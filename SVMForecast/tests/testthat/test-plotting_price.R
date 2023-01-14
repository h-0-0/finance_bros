D <- import_stonks()

test_that("long_format works", {
  lfD <- long_format(D)
  check <- D
  check["Date"]  <- as_date(rownames(check))
  expect_equal(lfD, check)
})

test_that("plotp works", {
  png(filename = "result.png")
  plotp(D, "BTC_USD")
  dev.off()

  png(filename = "compare.png")
  ggplot(long_format(D), aes_string(x = "Date", y = "BTC_USD")) +
    geom_line() +
    theme_classic() +
    labs(x = 'Date',
         y = "Adjusted Price",
         title = "BTC_USD Price Chart" )
  dev.off()

  visualTest::getFingerprint("result.png")
  visualTest::getFingerprint("compare.png")

  bool <- visualTest::isSimilar(file = "compare.png", fingerprint = visualTest::getFingerprint(file = "result.png"), threshold = 1)
  expect_equal(bool, TRUE)
})
