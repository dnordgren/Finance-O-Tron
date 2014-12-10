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

financial_analysis <- function(input, output, stocks, stock_data){
  weights <- calculate_weights(stocks$Weight)
  weights_formatted <- sapply(weights, function(weight){
    sprintf("%1.2f%%", 100*weight)
  })
  returns <- calculate_stock_returns(stock_data)
  weighted_returns <- calculate_weighted_returns(returns, weights)
  gspc_rates <- get_gspc_rates(stock_data$Date[1], tail(stock_data$Date,1))
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
  output$correlation_ui <- renderUI({
    if(input$correlation == "Plot"){
      plotOutput("correlation_plot", height = paste0(200*length(stocks$Symbol), "px"))
    }
    else{
      tableOutput("correlation_table")
    }
  })
  output$correlation_plot <- renderPlot({
    create_correlation_plot(returns, length(stocks$Symbol))
  })
  output$correlation_table <- renderTable({
    create_correlation_table(returns, length(stocks$Symbol))
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
  gspc_rates <- get_gspc_rates(stock_data$Date[1], tail(stock_data$Date,1))
  expected_returns <- calculate_expected_returns(returns, gspc_rates)
  calculated_weights <- calculate_weight_combination(rate, stocks$Symbol, returns, expected_returns)
  calculated_weights_formatted <- sapply(calculated_weights, function(weight){
    sprintf("%1.2f%%", 100*weight)
  })
  calculation <- calculate_plot_point(calculated_weights, returns, expected_returns)
  output$weights_table <- renderTable({
    data.frame(Symbol=stocks$Symbol, Weight=calculated_weights_formatted)
  }, include.rownames=FALSE)
  output$optimized_expected_return <- renderText({
    sprintf("%1.2f%%", 100*calculation[1])
  })
  output$optimized_standard_deviation <- renderText({
    sprintf("%1.3f", calculation[2])
  })
}

clear_output <- function(output, session){
  output$timeseries_plot.ui <- renderUI({
    plotOutput("timeseries_plot")
  })
  output$timeseries_plot <- renderPlot({
    NULL
  })
  output$combination_plot <- renderPlot({
    NULL
  })
  output$stock_table <- renderTable({
    NULL
  })
  output$expected_return <- renderText({
    NULL
  })
  output$standard_deviation <- renderText({
    NULL
  })
  output$correlation_plot.ui <- renderUI({
    plotOutput("correlation_plot")
  })
  output$correlation_plot <- renderPlot({
    NULL
  })
  output$weights_table <- renderPlot({
    NULL
  })
  output$model_plots.ui <- renderUI({
    NULL
  })
  output$symbol_error <- renderText({
    NULL
  })
  output$input_warning <- renderText({
    NULL
  })
  enableInputSmall(session)
  populate_remove_checkboxes(output, NULL, session)
}

populate_remove_checkboxes <- function(output, stocks, session){
  if(!is.null(stocks)){
    output$remove_stock_symbols <- renderUI({
      checkbox_list <- lapply(stocks$Symbol, function(symbol){
        symbol
      })
      checkboxGroupInput("remove_symbols", label=NULL, choices=checkbox_list)
    })
    enableUIElement("remove_stocks", session)
  }
  else{
    output$remove_stock_symbols <- renderUI({
      NULL
    })
    disableUIElement("remove_stocks", session)
    disableUIElement("calculate_weights", session)
  }
}

render_input_warning <- function(output, symbol, start){
  output$input_warning <- renderPrint({
    cat("Can only retrieve data for ", symbol, "from", as.character(start), "on. All data has been shortened accordingly.")
  })
}
