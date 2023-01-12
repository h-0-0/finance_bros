#' import_stonks
#'
#' @param stocks
#'
#' @return
#' @export
#'
#' @examples
import_stonks = function(stock_outcome = c("BTC-USD"), stock_pred =  c("ETH-USD"), lag = 1, from = "2018-01-01"){

  prices = tidyquant::tq_get(c(stock_outcome, stock_pred), from = from)

  prices_adj = prices[,c("symbol", "date", "adjusted")]
  prices_adj_wide =
    prices_adj %>%
    tidyr::pivot_wider(names_from = symbol, values_from = adjusted)

  prices_adj_wide


}


import_stonks()
