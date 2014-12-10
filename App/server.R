shinyServer(function(input, output, session){
  stocks <- NULL
  stock_data <- NULL
  start <- NULL
  end <- NULL
  
  disableUIElement("remove_stocks", session)
  disableUIElement("calculate_weights", session)

  get_market_data()

  # Monitor "Clear Stocks" button presses
  observe({
    if(input$clear_stocks == 0){
      return()
    }
    clear_output(output, session)
    stocks <<- NULL
    stock_data <<- NULL
    start <<- NULL
    end <<- NULL
  })

  # Monitor "Add Stock" button presses
  observe({
    if (input$add_stock == 0){
      return()
    }
    isolate({
      output$symbol_error <- renderText({
        NULL
      })
      output$input_warning <- renderText({
        NULL
      })
      withProgress(session, min = 0, max = 4, {
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
            output$symbol_error <- renderPrint({
              cat(symbol, "is not a valid ticker symbol")
            })
          }
          else{
            if (is.null(stock_data)){
              stock_data <<- stock_column
              if (stock_data$Date[1] != start){
                start <<- stock_data$Date[1]
                render_input_warning(output, symbol, start)
              }
            }
            else{
              if(length(stock_column[,1]) < length(stock_data$Date)){
                stock_data <<- tail(stock_data, length(stock_column[,1]))
                start <<- stock_data$Date[1]
                output$input_warning <- renderPrint({
                  cat("Can only retrieve data for ", symbol, "from", as.character(start), "on. All data has been shortened accordingly.")
                })
              }
              stock_data <<- cbind(stock_data, stock_column)
            }
            stocks <<- rbind(stocks, data.frame(Symbol=symbol, Weight=input$weight, stringsAsFactors=FALSE))
            if(length(stocks$Symbol) > 1){
              enableUIElement("calculate_weights", session)
            }
            else{
              disableUIElement("calcualte_weights", session)
            }
            populate_remove_checkboxes(output, stocks, session)
            setProgress(message = "Analyzing Timerseries Data", value = 2)
            timeseries_analysis(output, stocks, stock_data)
            setProgress(message = "Analyzing Financial Data", value = 3)
            financial_analysis(input, output, stocks, stock_data)
            setProgress(message = "Modeling Data", value = 4)
            modeling_analysis(output, stock_data)
          }
        }
      })
    })

    # Clear input text box on button press
    updateTextInput(session, "symbol", value = "")
  })

  # Monitor "Calculate Weights" button presses
  observe({
    if(input$calculate_weights == 0){
      return()
    }

    isolate({
      desired_rate <- input$desired_return
      find_weights(output, desired_rate, stocks, stock_data)
    })
  })

  # Monitor "Remove Stocks" button presses
  observe({
    if(input$remove_stocks == 0){
      return()
    }

    isolate({
      remove_list <- input$remove_symbols
      remove_all <- TRUE
      lapply(stocks$Symbol, function(symbol){
        if (!(symbol %in% remove_list)){
          remove_all <<- FALSE
        }
      })
      if(remove_all){
        clear_output(output, session)
        stocks <<- NULL
        stock_data <<- NULL
        start <<- NULL
        end <<- NULL
      }
      else{
        stocks <<- stocks[!(stocks$Symbol %in% remove_list),]
        stock_data <<- stock_data[,!(names(stock_data) %in% remove_list)]
      }
      populate_remove_checkboxes(output, stocks, session)
      if(!is.null(stocks)){
        if(length(stocks$Symbol) > 1){
          enableUIElement("calculate_weights", session)
        }
        else{
          disableUIElement("calculate_weights", session)
        }
        withProgress(session, min = 0, max = 3, {
          setProgress(message = "Analyzing Timerseries Data", value = 1)
          timeseries_analysis(output, stocks, stock_data)
          setProgress(message = "Analyzing Financial Data", value = 2)
          financial_analysis(input, output, stocks, stock_data)
          setProgress(message = "Modeling Data", value = 3)
          modeling_analysis(output, stock_data)
        })
      }
      else{
        disableUIElement("calculate_weights", session)
      }
    })
  })
})
