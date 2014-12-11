analyze_timeseries <- function(selected_stock, output, date, selected_symbol_data) {
  years <- as.duration(ymd(date[length(date)]) - ymd(date[1])) / as.duration(years(1))
  number_per_year <- floor(length(selected_symbol_data)/years)
  if (length(selected_symbol_data) < number_per_year) {
    number_per_year <- length(selected_symbol_data)
  }
  tryCatch({
    ts <- ts(selected_symbol_data,
             start=c(year(date[1]),
             month(date[1])),
             freq=number_per_year)
    h_value <- round(length(ts)/10)
    p_value_threshhold <- 0.1

    beta_gamma_forecasts <- HoltWinters(ts)
    beta_gamma_forecasts2 <- forecast.HoltWinters(beta_gamma_forecasts, h=h_value)
    beta_gamma_box <- Box.test(beta_gamma_forecasts2$residuals, lag=h_value, type="Ljung-Box")

    beta_forecasts <- HoltWinters(ts, gamma=FALSE)
    beta_forecasts2 <- forecast.HoltWinters(beta_forecasts, h=h_value)
    beta_box <- Box.test(beta_forecasts2$residuals, lag=h_value, type="Ljung-Box")

    neither_forecasts <- HoltWinters(ts, beta=FALSE, gamma=FALSE)
    neither_forecasts2 <- forecast.HoltWinters(neither_forecasts, h=h_value)
    neither_box <- Box.test(neither_forecasts2$residuals, lag=h_value, type="Ljung-Box")

    p_values <- c(beta_gamma_box$p.value, beta_box$p.value, neither_box$p.value)
    matching_models <- p_values >= p_value_threshhold

    if (matching_models[1]) {
      output$beta_gamma <- renderPlot({
        plot.forecast(beta_gamma_forecasts2,
                      main=paste0(as.character(selected_stock), " Holt-Winters Exponential Smoothing"),
                      xlab="Date",
                      ylab="Price (dollars)")
      })
    } else {
      output$beta_gamma <- renderPlot({
        NULL
      })
    }
    if (matching_models[2]) {
      output$beta <- renderPlot({
        plot.forecast(beta_forecasts2,
                      main=paste0(as.character(selected_stock), " Holt Exponential Smoothing"),
                      xlab="Date",
                      ylab="Price (dollars)")
      })
    } else {
      output$beta <- renderPlot({
        NULL
      })
    }
    if (matching_models[3]) {
      output$neither <- renderPlot({
        plot.forecast(neither_forecasts2,
                      main=paste0(as.character(selected_stock), " Exponential Smoothing"),
                      xlab="Date",
                      ylab="Price (dollars)")
      })
    } else {
      output$neither <- renderPlot({
        NULL
      })
    }
  }, error = function(e) {
    output$beta_gamma <- renderPlot({
      NULL
    })
    output$beta <- renderPlot({
      NULL
    })
    output$neither <- renderPlot({
      NULL
    })
  })
}
