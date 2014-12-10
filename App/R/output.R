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
