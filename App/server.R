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
    output$plot.ui <- renderUI({
      plotOutput("plot")
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
        stock_row <- data.frame(Symbol = as.character(toupper(input$symbol)),
                                Start = as.character(input$range[1]), End = as.character(input$range[2]),
                                stringsAsFactors = FALSE)
        stocks <<- rbind(stocks, stock_row)
      }
      output$symbols <- renderPrint({
        cat("Stocks: ")
        cat(stocks$Symbol, sep=", ")
      })
      if (input$timeseries){
        output$plot.ui <- renderUI({
          plotOutput("plot", height = paste0(200*length(stocks$Symbol), "px"))
        })
        output$plot <- renderPlot({
          withProgress(session, min = 0, max = 2, {
            setProgress(message = "Creating plots", value = 1)
            create_plot(stocks)
          })
        })
      }
    })
  })
  
  # Monitor "Apply Analysis" button presses
  observe({
    if (input$apply_analysis == 0){
      return()
    }
    if (input$timeseries){
      output$plot.ui <- renderUI({
        plotOutput("plot", height = paste0(200*length(stocks$Symbol), "px"))
      })
      output$plot <- renderPlot({
        create_plot(stocks)
      })
    }
    else{
      output$plot.ui <- renderUI({
        plotOutput("plot")
      })
      output$plot <- renderPlot({
        NULL
      })
    }
  })
})