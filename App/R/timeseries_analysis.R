create_timeseries_plot <- function(ma1, ma2, stocks, stock_data) {
  # specify moving average trails
  ma_trail1 <- as.numeric(ma1)
  ma_trail2 <- as.numeric(ma2)

  if (is.na(ma_trail1) || ma_trail1 < 0 || ma_trail2 > stock_data$Date) {
    ma_trail1 <- length(stock_data$Date)
  }
  if (is.na(ma_trail2) || ma_trail2 < 0 || ma_trail2 > stock_data$Date) {
    ma_trail2 <- length(stock_data$Date)
  }

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
