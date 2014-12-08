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
  output$stock_table <- renderTable({
    stocks
  }, include.rownames=FALSE)
  returns <- calculate_stock_returns(stock_data)
  weights <- calculate_weights(stocks$Weight)
  weighted_returns <- calculate_weighted_returns(returns, weights)
  gspc_rates <- get_gspc_rates(stock_data$Date[1])
  expected_returns <- calculate_expected_returns(returns, gspc_rates)
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