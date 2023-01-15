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
