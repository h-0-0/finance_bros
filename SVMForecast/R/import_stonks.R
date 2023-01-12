#' import_stonks
#'
#' @param stocks
#'
#' @return
#' @export
#'
#' @examples
import_stonks = function(stock_outcome = c("BTC-USD"), stock_pred =  c("ETH-USD", "DOGE-USD"), day_lag = c(1), from = as.Date("2018-01-02")){

  prices_out = tidyquant::tq_get(stock_outcome, from = from)
  prices_out = prices_out[,c("symbol", "date", "adjusted")]


  stagger_stock = function(i){
    prices_pred = tidyquant::tq_get(i, from = from - max(day_lag))
    prices_pred = prices_pred[,c("symbol", "date", "adjusted")]

    # day_lag = c(1,2,3)
    stagger_stock = vector(length = max(day_lag), "list")

    for(j in day_lag){
      if((max(day_lag) -j) == 0){
        stagger_stock[[j]] = prices_pred[-c((nrow(prices_pred) - j):nrow(prices_pred)),]
      } else {
        stagger_stock[[j]] = prices_pred[-c(1:(max(day_lag) -j),(nrow(prices_pred) - j):nrow(prices_pred)),]
      }

    }

    dplyr::bind_cols(stagger_stock)

  }

  lapply(stock_pred,  stagger_stock)


  prices_adj = prices[,c("symbol", "date", "adjusted")]
  prices_adj_wide =
    prices_adj %>%
    tidyr::pivot_wider(names_from = symbol, values_from = adjusted)

  prices_adj_wide


}


import_stonks()
