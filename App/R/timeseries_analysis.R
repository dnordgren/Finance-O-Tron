create_timeseries_plot <- function(stocks, stock_data){
  stock_ts <- lapply(stocks$Symbol, function(symbol){
    data.frame(Symbol = symbol, Prices = stock_data[,symbol], Date = stock_data$Date)
  })
  stock_ts <- do.call("rbind", stock_ts)
  
  plot <- ggplot(stock_ts, aes(x=Date, y=Prices)) + geom_line() + facet_wrap(~Symbol,nrow=length(stocks$Symbol), scales="free")
  plot
}