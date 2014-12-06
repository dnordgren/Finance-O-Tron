shinyServer(function(input, output, session){
  stocks <- NULL

  # Monitor "Clear Stocks" button presses
  observe({
    if(input$clear_stocks == 0){
      return()
    }
    output$error <- renderText({
      NULL
    })
    stocks <<- NULL
    output$symbols <- renderPrint({
      cat("Stocks: ")
    })
    create_blank_timeseries_plot_output(output)
  })

  # Monitor "Add Stock" button presses
  observe({
    if (input$add_stock == 0){
      return()
    }
    isolate({
      output$error <- renderText({
        NULL
      })
      withProgress(session, min = 0, max = 2, {
        if(length(stocks$Symbol) == 0 || !(input$symbol %in% stocks$Symbol)){
          symbol <- as.character(toupper(input$symbol))
          setProgress(message = "Getting data", value = 1)
          stock_row <- get_stock_data(symbol, as.character(input$range[1]), as.character(input$range[2]))
          if (is.null(stock_row)){
            output$error <- renderPrint({
              cat(symbol, "is not a valid ticker symbol")
            })
          }
          else{
            stocks <<- rbind(stocks, stock_row)
            output$symbols <- renderPrint({
              cat("Stocks: ")
              cat(stocks$Symbol, sep=", ")
            })
            setProgress(message = "Creating plots", value = 2)
            create_timeseries_plot_output(input, output, session, stocks)
          }
        }
      })
    })

    # Clear input text box on button press
    updateTextInput(session, "symbol", value = "")
  })
})
