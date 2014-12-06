create_timeseries_plot_output <- function(input, output, session, stocks){
  if(length(stocks$Symbol) == 0){
    create_blank_timeseries_plot_output(output)
  }
  
  output$timeseries_plot.ui <- renderUI({
    plotOutput("timeseries_plot", height = paste0(200*length(stocks$Symbol), "px"))
  })
  output$timeseries_plot <- renderPlot({
    create_timeseries_plot(stocks)
  })
}

create_blank_timeseries_plot_output <- function(output){
  output$timeseries_plot.ui <- renderUI({
    plotOutput("timeseries_plot")
  })
  output$timeseries_plot <- renderPlot({
    NULL
  })
}