timeseries_analysis <- function(output, stocks){
  if(length(stocks$Symbol) == 0){
    create_blank_output(output)
  }
  
  output$timeseries_plot.ui <- renderUI({
    plotOutput("timeseries_plot", height = paste0(200*length(stocks$Symbol), "px"))
  })
  output$timeseries_plot <- renderPlot({
    create_timeseries_plot(stocks)
  })
}

financial_analysis <- function(output, stocks){
  returns <- calculate_stock_returns(stocks)
  expected_returns <- calculate_expected_returns(returns)
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