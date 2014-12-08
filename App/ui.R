shinyUI(fluidPage(
  tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
        function(message) {
          console.log(message)
          eval(message.code);
        }
      );
    '))),
  
  progressInit(),
  
  titlePanel("Finance-O-Tron"),
  
  sidebarPanel(
    textInput("symbol", "Enter a stock symbol:"),
    numericInput("weight", "Enter the weight (see Help for details):", min = 0, value = 1),
    dateRangeInput("range", "Date Range:", start = as.Date(ymd(Sys.Date()) - years(1)), end = as.Date(ymd(Sys.Date()))),
    actionButtonHalf("add_stock", "Add Stock"),
    actionButtonHalf("clear_stocks", "Clear Stocks")
  ),
  
  mainPanel(
    textOutput("symbols"),
    textOutput("error"),
    tags$head(tags$style("#error{color: red;}")),
    tabsetPanel(
      tabPanel("Portfolio",
               h3("Stocks:"),
               tableOutput("stock_table"),
               strong("Expected Portfolio Return:"),
               textOutput("expected_return"),
               strong("Portfolio Standard Deviation:"),
               textOutput("standard_deviation")),
      tabPanel("Timeseries Analysis", uiOutput("timeseries_plot.ui")),
      tabPanel("Fundamental Analysis"),
      tabPanel("Financial Analysis", plotOutput("combination_plot")),
      tabPanel("Help")
    )
  )
))