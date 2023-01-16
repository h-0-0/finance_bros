#' Modify to long format
#'
#' Checks if there is a column with dates, if not adds a column with dates from the row names (this is how data frames are created using import_stonks())
#' @param df, a data frame
#' @return a data frame with added date column (if it was missing)
#' @importFrom lubridate as_date is.Date
#' @export
long_format <- function(df){
  if( ! any(unname(unlist(lapply(df[1:ncol(df)], is.Date)))) ){
    df["Date"]  <- as_date(rownames(df))
  }
  df
}


#' Plot price action
#'
#' Will plot price action of supplied ticker over time using data in supplied data frame.
#' @param data data frame containing price action data you want to plot
#' @param ticker string of the ticker you want to plot (must be one of the column names)
#' @return Returns a plot of the price of the ticker against time
#' @export
#' @import ggplot2
plotp <- function(data, ticker){
  # We make sure data is formatted correctly for plotting
  data <- long_format(data)
  if(! "Date" %in% colnames(data)){stop("No column labelled 'Date', either remove all date columns or change date column to name: 'Date'")}

  plt <- ggplot(data, aes_string(x = "Date", y = ticker)) +
    geom_line() +
    theme_classic() +
    labs(x = 'Date',
         y = "Adjusted Price",
         title = paste(ticker," Price Chart") )
  return(plt)
}

#' Plot forecasting
#'
#' Will plot price action of BTC_USD for testing data (as a line), along with testing data (as black points) and the predictions (red dots).
#' @param data data frame containing price action data you want to plot
#' @param train_range row indices used in training of SVM
#' @param test_range row indices used for testing of SVM
#' @param preds predictions
#' @param restrict_train integer saying how many testing samples we want, defult is NULL where all avliable are used
#' @return Returns a plot
#' @export
#' @import ggplot2
plotf <- function(data, train_range, test_range, preds, restrict_train = NULL){
  if(! is.null(restrict_train)){
    train_range <- train_range[(length(train_range)-restrict_train) : length(train_range)]
  }
  data.long <- long_format(data)
  plt <- ggplot(data.long[train_range,], aes_string(x = "Date", y = "BTC_USD")) +
    geom_line() +
    geom_point(data = preds, colour="red") +
    geom_line(data = preds, colour="red") +
    geom_point(data = data.long[test_range,], colour="black") +
    geom_line(data = data.long[test_range,], colour="black") +

    theme_classic() +
    labs(x = 'Date',
         y = "Adjusted Price",
         title = "BTC_USD Forecasting" )
}
