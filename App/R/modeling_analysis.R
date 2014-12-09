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
      return(trend_analysis(ts))
    }, error = function(e) {
      return(NULL)
    })
  })
}

trend_analysis <- function(ts) {
  plot_list <- NULL
  h_value <- round(length(ts)/10)
  p_value_threshhold <- 0.1

  beta_gamma_forecasts <- HoltWinters(ts)
  beta_gamma_forecasts2 <- forecast.HoltWinters(beta_gamma_forecasts, h=h_value)
  beta_gamma_box <- Box.test(beta_gamma_forecasts2$residuals, lag=h_value, type="Ljung-Box")

  neither_forecasts <- HoltWinters(ts, beta=FALSE, gamma=FALSE)
  neither_forecasts2 <- forecast.HoltWinters(neither_forecasts, h=h_value)
  neither_box <- Box.test(neither_forecasts2$residuals, lag=h_value, type="Ljung-Box")

  beta_forecasts <- HoltWinters(ts, gamma=FALSE)
  beta_forecasts2 <- forecast.HoltWinters(beta_forecasts, h=h_value)
  beta_box <- Box.test(beta_forecasts2$residuals, lag=h_value, type="Ljung-Box")

  p_values <- c(beta_gamma_box$p.value, neither_box$p.value, beta_box$p.value)
  matching_models <- p_values >= p_value_threshhold

  b <- NULL
  b_g <- NULL
  neither <- NULL

  if (matching_models[1]) {
    b_g <- beta_gamma_forecasts2
  }
  if (matching_models[2]) {
    neither <- neither_forecasts2
  }
  if (matching_models[3]) {
    b <- beta_forecasts2
  }

  plot_list <- list(b, b_g, neither)
  return(plot_list)
}
