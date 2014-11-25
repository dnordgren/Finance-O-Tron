shinyServer(function(input, output, session){
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
    create_blank_plot_output(output)
  })
  
  # Monitor "Add Stock" button presses
  observe({
    if (input$add_stock == 0){
      return()
    }
    isolate({
      if(length(stocks$Symbol) == 0 || !(input$symbol %in% stocks$Symbol)){
        stock_row <- data.frame(Symbol = as.character(toupper(input$symbol)),
                                Start = as.character(input$range[1]), End = as.character(input$range[2]),
                                stringsAsFactors = FALSE)
        stocks <<- rbind(stocks, stock_row)
      }
      print(stocks)
      output$symbols <- renderPrint({
        cat("Stocks: ")
        cat(stocks$Symbol, sep=", ")
      })
      if (input$timeseries){
        stocks <<- create_plot_output(input, output, session, stocks)
        print(stocks)
      }
    })
  })
  
  # Monitor "Apply Analysis" button presses
  observe({
    if (input$apply_analysis == 0){
      return()
    }
    isolate({
      if (input$timeseries){
        stocks <<- create_plot_output(input, output, session, stocks)
      }
      else{
        create_blank_plot_output(output)
      }
    })
  })
})