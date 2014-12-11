timeseries_analysis <- function(ma1, ma2, output, stocks, stock_data){
  if(length(stocks$Symbol) == 0){
    create_blank_output(output)
  }
  output$timeseries_plot.ui <- renderUI({
    plotOutput("timeseries_plot", height = paste0(200*length(stocks$Symbol), "px"))
  })
  output$timeseries_plot <- renderPlot({
    create_timeseries_plot(ma1, ma2, stocks, stock_data)
  })
}

financial_analysis <- function(output, stocks, stock_data){
  returns <- calculate_stock_returns(stock_data)
  expected_returns <- calculate_expected_returns(returns)
  output$combination_plot <- renderPlot({
    create_combination_plot(length(stocks$Symbol), returns, expected_returns)
  })
}

modeling_analysis <- function(selected_stock, output, stock_data) {
  forecasts <- analyze_timeseries(selected_stock,
                                  output,
                                  stock_data[,1],
                                  stock_data[,selected_stock])
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

populate_modeling_choices <- function(output, stocks) {
  if (!is.null(stocks)) {
    output$modeling_stock_symbols <- renderUI({
      select_box_list <- lapply(stocks$Symbol, function(symbol) {
        symbol
      })
      selectInput("stock_selection",
          label=strong("Choose a stock to model"),
          choices=select_box_list)
    })
  } else {
    output$modeling_stock_symbols <- renderUI({
      NULL
    })
  }
}
