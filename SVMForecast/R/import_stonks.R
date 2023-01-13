
#' import_stonks
#'
#' @param stock_outcome Character. Outcome variable we will try to predict. Default: "BTC-USD"
#' @param stock_pred Character Vector. Stock prices we will use as predictors
#' @param day_lag Numeric Vector. The day lags of the predictors we want in dataset.
#' @param from Date. The start date we want data from the outcome variable.
#'
#' @return
#'   [`data.frame`] Containing outcome variable adjusted stock price and other Stocks selected as predictors with time lag selected by `day_lag` argument.
#' @export
#' @importFrom tidyquant tq_get
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr::bind_cols
#'
#' @examples
#' \dontrun{
#'   head(import_stonks(stock_outcome = c("BTC-USD"), stock_pred =  c("ETH-USD", "DOGE-USD"), day_lag = c(1,2,3)))
#' }
import_stonks = function(stock_outcome = c("BTC-USD"), stock_pred =  c("ETH-USD", "DOGE-USD"), day_lag = c(1,2), from = as.Date("2018-01-02")){

  # allign_fun =function(x){
  #   vv = tq_get(x, from = from - max(day_lag), to = from - max(day_lag))
  # }
  # while (any(is.na(unlist(lapply(stock_pred, allign_fun))))) {
  #   from = from + 1
  #   print(from)
  # }

  prices_out = tq_get(stock_outcome, from = from - max(day_lag))
  prices_out = prices_out[,c("symbol", "date", "adjusted")]
  prices_out =
    prices_out %>%
    pivot_wider(names_from = symbol, values_from = adjusted)
  colnames(prices_out)[colnames(prices_out) == stock_outcome] = gsub(x = stock_outcome, pattern = "-", replacement = "_")


  # i = stock_pred[1]
  # i = stock_pred[3]
  stagger_stock = function(i){
    prices_pred = tq_get(i, from = from - max(day_lag))
    prices_pred = prices_pred[,c("symbol", "date", "adjusted")]
    colnames(prices_pred)[3] = paste(gsub(x = prices_pred$symbol[1], pattern = "-", replacement = "_"))
    prices_pred = merge(prices_out, prices_pred, by="date", all.x = TRUE,)
    prices_pred = prices_pred[4]
    prices_pred = zoo::na.locf(prices_pred, na.rm = F)
    # day_lag = c(1,2,3)
    stagger_stock = vector(length = max(day_lag), "list")

    for(j in day_lag){
      if((max(day_lag) -j) == 0){
        stagger_stock[[j]] = prices_pred[-c((nrow(prices_pred) - j + 1):nrow(prices_pred)),, drop = F]
        colnames(stagger_stock[[j]]) = paste0(colnames(stagger_stock[[j]]),"_",j)
      } else {
        stagger_stock[[j]] = prices_pred[-c(1:(max(day_lag) -j),(nrow(prices_pred) - j +1):nrow(prices_pred)),, drop = F]
        colnames(stagger_stock[[j]]) = paste0(colnames(stagger_stock[[j]]),"_",j)
      }

    }

    dplyr::bind_cols(stagger_stock)

  }



  output = cbind(prices_out[-(1:max(day_lag)),], dplyr::bind_cols(lapply(stock_pred,  stagger_stock)))
  rownames(output) = output$date

  return(output[,-1])

}


