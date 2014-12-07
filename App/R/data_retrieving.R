get_stock_data <- function(symbol, start, end, return_dates){
  error <- FALSE
  tryCatch({
    quandl_code <- paste0("YAHOO/", symbol)
    stock <- Quandl(quandl_code, start_date=start, end_date=end)
    stock <- stock[nrow(stock):1,]
  }, error = function(e){
    error <<- TRUE
  })
  if (error){
    return(NULL)
  }
  if (return_dates){
    data <- data.frame(Dates=stock[,'Date'], Prices=stock[,'Adjusted Close'], stringsAsFactors=FALSE)
    colnames(data) <- c("Date", symbol)
    return(data)
  }
  else{
    data <- data.frame(Prices=stock[,'Adjusted Close'])
    colnames(data) <- c(symbol)
    return(data)
  }
}