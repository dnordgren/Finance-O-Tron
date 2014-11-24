shinyServer(function(input, output){
  stocks <- data.frame(Symbol = character(), Start = character(), End = character(), stringsAsFactors = FALSE)
  # Monitor "Clear Stocks" button presses
  observe({
    if(input$clear_stocks == 0){
      return()
    }
    stocks <<- data.frame(Symbol = character(), Start = character(), End = character(), stringsAsFactors = FALSE)
    output$symbols <- renderPrint({
      cat("Stocks: ")
    })
    output$plot <- renderPlot({
      NULL
    })
  })
  # Monitor "Add Stock" button presses
  observe({
    if (input$add_stock == 0){
      return()
    }
    isolate({
      if(length(stocks$Symbol) == 0 || !(input$symbol %in% stocks$Symbol)){
        stock_row <- data.frame(Symbol = as.character(toupper(input$symbol)), Start = as.character(input$range[1]), End = as.character(input$range[2]), stringsAsFactors = FALSE)
        stocks <<- rbind(stocks, stock_row)
      }
      output$symbols <- renderPrint({
        cat("Stocks: ")
        cat(stocks$Symbol, sep=", ")
      })
      if (input$timeseries){
        output$plot <- renderPlot({
          create_plot(stocks)
        })
        # If they've selected timeseries, print the output
        # Start date = as.character(input$range[1])
        # End date = as.character(input$range[2])
        # For now, could just loop through and get quandl data for each
        # stock in list, create timeseries for each 'Adjusted Close' column
        # and plot all timeseries on same graph. Would need to assume same
        # start and end dates.
      }
    })
  })
})