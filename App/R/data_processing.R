Quandl.auth("Xwpyys22sxHPzyXBrGdH")

get_stock_data <- function(symbol, start, end){
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
  stock_row <- data.frame(Symbol = symbol,
                          Start = start,
                          End = end,
                          Prices = I(list(stock[,'Adjusted Close'])),
                          Dates = I(list(stock[,'Date'])),
                          stringsAsFactors = FALSE)
}

create_timeseries_plot <- function(stocks){
  stock_ts <- apply(stocks, 1, function(row)
  {
    data.frame(Symbol = row$Symbol, Prices = row$Prices, Date=row$Dates)
  })
  stock_ts <- do.call("rbind", stock_ts)
  
  plot <- ggplot(stock_ts, aes(x=Date, y=Prices)) + geom_line() + facet_wrap(~Symbol,nrow=length(stocks$Symbol), scales="free")
  plot
}