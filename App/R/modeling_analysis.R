plot_list <- list()

analyze_timeseries <- function(stock_data) {
  plots <- apply(stock_data[-1], 2, function(column) {
    years <- as.duration(ymd(tail(stock_data$Date, 1)) - ymd(head(stock_data$Date, 1))) / as.duration(years(1))
    number_per_year <- floor(length(column)/years)
    if (length(column) < number_per_year) {
      number_per_year <- length(column)
    }
    tryCatch({
      ts <- ts(column,
               start=c(year(stock_data$Date[1]),
               month(stock_data$Date[1])),
               freq=number_per_year)
      trend_analysis(ts, b=FALSE, g=FALSE)
      trend_analysis(ts, b=TRUE, g=FALSE)
      trend_analysis(ts, b=TRUE, g=TRUE)
    }, error = function(e) {
      return(NULL)
    })
  })
  return(plot_list)
}

trend_analysis <- function(ts, b, g) {
  h_value <- round(length(ts)/10)
  p_value_threshhold <- 0.1

  ts_fc <- HoltWinters(ts, beta=b, gamma=g)
  ts_fc2 <- forecast.HoltWinters(ts_fc, h=h_value)
  box_test <- Box.test(ts_fc2$residuals, lag=h_value, type="Ljung-Box")

  if (box_test$p.value > p_value_threshhold) {
    plot_list <<- list(plot_list, ts_fc2)
  }
}
