shinyServer(function(input, output, session){
  stocks <- NULL
  stock_data <- NULL
  start <- NULL
  end <- NULL

  get_market_data()

  # Monitor "Clear Stocks" button presses
  observe({
    if(input$clear_stocks == 0){
      return()
    }
    output$error <- renderText({
      NULL
    })
    stocks <<- NULL
    stock_data <<- NULL
    start <<- NULL
    end <<- NULL
    enableInputSmall(session)
    output$symbols <- renderPrint({
      cat("Stocks: ")
    })
    create_blank_output(output)
    populate_modeling_choices(output, stocks)

    # clear any forecast plots
    output$beta_gamma <- renderPlot({
      NULL
    })
    output$beta <- renderPlot({
      NULL
    })
    output$neither <- renderPlot({
      NULL
    })

    # Clear input text box on button press
    updateTextInput(session, "symbol", value = "")
  })

  # Monitor Forecasting Stock Selection
  observe({
    selected_stock <- input$stock_selection
    if (!is.null(selected_stock)) {
      modeling_analysis(selected_stock, output, stock_data)
    }
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
      withProgress(session, min = 0, max = 3, {
        if (is.null(start) || is.null(end)){
          start <<- as.character(input$range[1])
          end <<- as.character(input$range[2])
          disableInputSmall(session)
        }
        if(length(stocks$Symbol) == 0 || !(input$symbol %in% stocks$Symbol)){
          symbol <- as.character(toupper(input$symbol))
          setProgress(message = "Getting data", value = 1)
          return_dates <- FALSE
          if (is.null(stock_data)){
            return_dates <- TRUE
          }
          stock_column <- get_stock_data(symbol, start, end, return_dates)
          if (is.null(stock_column)){
            output$error <- renderPrint({
              cat(symbol, "is not a valid ticker symbol")
            })
          }
          else{
            if (is.null(stock_data)){
              stock_data <<- stock_column
            }
            else{
              if(length(stock_column[,1]) < length(stock_data$Date)){
                stock_data <<- tail(stock_data, length(stock_column[,1]))
                start <<- stock_data$Date[1]
              }
              stock_data <<- cbind(stock_data, stock_column)
            }
            stocks <<- rbind(stocks, data.frame(Symbol=symbol, Weight=input$weight, stringsAsFactors=FALSE))
            populate_modeling_choices(output, stocks)
            output$symbols <- renderPrint({
              cat("Stocks: ")
              cat(stocks$Symbol, sep=", ")
            })
            setProgress(message = "Analyzing Timerseries Data", value = 2)
            timeseries_analysis(output, stocks, stock_data)
            setProgress(message = "Analyzing Financial Data", value = 3)
            #financial_analysis(output, stocks, stock_data)
          }
        }
      })
    })

    # Clear input text box on button press
    updateTextInput(session, "symbol", value = "")
  })
})
