# specify moving average trails
ma_trail1 = 50
ma_trail2 = 100

create_timeseries_plot <- function(stocks, stock_data){
  stock_ts <- lapply(stocks$Symbol, function(symbol){
    sma_1 <- SMA(stock_data[,symbol], n=ma_trail1)
    sma_2 <- SMA(stock_data[,symbol], n=ma_trail2)
    data.frame(Symbol = symbol,
               Prices = stock_data[,symbol],
               MA1    = sma_1,
               MA2    = sma_2,
               Date   = stock_data$Date)
  })
  stock_ts <- do.call("rbind", stock_ts)

plot <- ggplot(stock_ts, aes(x=Date)) +
          geom_line(aes(y=Prices)) +
          geom_line(aes(y=MA1), color="red") +
          geom_line(aes(y=MA2), color="blue") +
          facet_wrap(~Symbol,nrow=length(stocks$Symbol), scales="free")
plot
}
