create_plot_output <- function(input, output, session, stocks){
  if(length(stocks$Symbol) == 0){
    create_blank_plot_output(output)
    return(stocks)
  }
  
  output$plot.ui <- renderUI({
    plotOutput("plot", height = paste0(200*length(stocks$Symbol), "px"))
  })
  withProgress(session, min = 0, max = 2, {
    setProgress(message = "Creating plots", value = 1)
    plot <- create_plot(stocks)
    output$plot <- renderPlot({
      plot
    })
    if(is.null(plot)){
      output$error <- renderPrint({
        cat(tail(stocks, 1)$Symbol, "is not a valid ticker symbol")
      })
      create_plot_output(input, output, session, head(stocks, -1))
      return(head(stocks, -1))
    }
    return(stocks)
  })
}

create_blank_plot_output <- function(output){
  output$plot.ui <- renderUI({
    plotOutput("plot")
  })
  output$plot <- renderPlot({
    NULL
  })
}