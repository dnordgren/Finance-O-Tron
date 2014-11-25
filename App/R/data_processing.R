Quandl.auth("Xwpyys22sxHPzyXBrGdH")

create_plot <- function(stocks){
  error <- FALSE
  stock_ts <- lapply(stocks$Symbol, function(symbol)
  {
    tryCatch({
      quandl_code <- paste0("YAHOO/", symbol)
      stock <- Quandl(quandl_code, start_date=stocks$Start[stocks$Symbol==symbol], end_date=stocks$End[stocks$Symbol==symbol])
      stock <- stock[nrow(stock):1,]
      data.frame(Symbol = symbol, Prices = stock[,'Adjusted Close'], Date=stock[,'Date'])
    }, error = function(e){
      error <<- TRUE
    })
  })
  if (error){
    return(NULL)
  }
  stock_ts <- do.call("rbind", stock_ts)
  
  plot <- ggplot(stock_ts, aes(x=Date, y=Prices)) + geom_line() + facet_wrap(~Symbol,nrow=length(stocks$Symbol), scales="free")
  plot
}