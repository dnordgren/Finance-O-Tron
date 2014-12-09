timeseries_analysis <- function(output, stocks, stock_data){
  if(length(stocks$Symbol) == 0){
    create_blank_output(output)
  }
  output$timeseries_plot.ui <- renderUI({
    plotOutput("timeseries_plot", height = paste0(200*length(stocks$Symbol), "px"))
  })
  output$timeseries_plot <- renderPlot({
    create_timeseries_plot(stocks, stock_data)
  })
}

financial_analysis <- function(output, stocks, stock_data){
  weights <- calculate_weights(stocks$Weight)
  weights_formatted <- sapply(weights, function(weight){
    sprintf("%1.2f%%", 100*weight)
  })
  returns <- calculate_stock_returns(stock_data)
  weighted_returns <- calculate_weighted_returns(returns, weights)
  gspc_rates <- get_gspc_rates(stock_data$Date[1])
  expected_returns <- calculate_expected_returns(returns, gspc_rates)
  expected_returns_formatted <- sapply(expected_returns, function(expected_return){
    sprintf("%1.2f%%", 100*expected_return)
  })
  standard_deviations <- apply(returns, 2, function(column){
    sd(column)
  })
  standard_deviations_formatted <- sapply(standard_deviations, function(standard_deviation){
    sprintf("%1.3f", standard_deviation)
  })
  betas <- apply(returns, 2, function(column){
    calculate_beta(column, gspc_rates)
  })
  output$stock_table <- renderTable({
    table <- data.frame(Symbol=stocks$Symbol,
               Weight=weights_formatted,
               Beta=betas,
               StandardDeviaton=standard_deviations_formatted,
               Return=expected_returns_formatted)
    colnames(table)[4] <- "Standard Deviation"
    table
  }, include.rownames=FALSE)
  output$correlation_plot.ui <- renderUI({
    plotOutput("correlation_plot", height = paste0(200*length(stocks$Symbol), "px"))
  })
  output$correlation_plot <- renderPlot({
    create_correlation_plot(returns, length(stocks))
  })
  output$expected_return <- renderText({
    sprintf("%1.2f%%", 100*calculate_expected_return(expected_returns, weights))
  })
  output$standard_deviation <- renderText({
    sprintf("%1.3f", calculate_standard_deviation(weighted_returns))
  })
  output$combination_plot <- renderPlot({
    create_combination_plot(length(stocks$Symbol), returns, expected_returns)
  })
}

modeling_analysis <- function(output, stock_data) {
  plots <- analyze_timeseries(stock_data)

  i <- 1
  lapply(plots, function(plot) {
    my_i <- i
    plotname <- paste0("plot", my_i)
    output[[plotname]] <- renderPlot({
      plot.forecast(plot)
    })
    i <<- i + 1
  })

  output$model_plots.ui <- renderUI({
    if (is.null(plots)) {
      return(NULL)
    }
    plot_output_list <- lapply(1:length(plots), function(i) {
      plotname <- paste0("plot", i)
      plotOutput(plotname, height=250)
   })
   do.call(tagList, plot_output_list)
  })
}

find_weights <- function(output, rate, stocks, stock_data){
  returns <- calculate_stock_returns(stock_data)
  gspc_rates <- get_gspc_rates(stock_data$Date[1])
  expected_returns <- calculate_expected_returns(returns, gspc_rates)
  calculated_weights <- calculate_weight_combination(rate, stocks$Symbol, returns, expected_returns)
  calculated_weights_formatted <- sapply(calculated_weights, function(weight){
    sprintf("%1.2f%%", 100*weight)
  })
  calculation <- calculate_plot_point(calculated_weights, returns, expected_returns)
  output$weights_table <- renderTable({
    data.frame(Symbol=stocks$Symbol, Weight=calculated_weights_formatted)
  }, include.rownames=FALSE)
}

create_blank_output <- function(output){
  output$timeseries_plot.ui <- renderUI({
    plotOutput("timeseries_plot")
  })
  output$timeseries_plot <- renderPlot({
    NULL
  })
  output$combination_plot <- renderPlot({
    NULL
  })
}
