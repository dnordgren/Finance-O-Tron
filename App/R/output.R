create_plot_output <- function(input, output, session, stocks){
  if(length(stocks$Symbol) == 0){
    create_blank_plot_output(output)
    return()
  }
  
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

create_blank_plot_output <- function(output){
  output$plot.ui <- renderUI({
    plotOutput("plot")
  })
  output$plot <- renderPlot({
    NULL
  })
}