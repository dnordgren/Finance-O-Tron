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
    actionButtonHalf("clear_stocks", "Clear All Stocks"),
    textOutput("input_warning")
  ),

  mainPanel(
    textOutput("symbol_error"),
    tags$head(tags$style("#symbol_error{color: red;}")),
    tags$head(tags$style("#timeseries_error{color: red;}")),
    tags$head(tags$style("#portfolio_error{color: red;}")),
    tags$head(tags$style("#financial_error{color: red;}")),
    tabsetPanel(
      tabPanel("Portfolio",
               textOutput("portfolio_error"),
               h3("Stocks:", id="stocks"),
               tableOutput("stock_table"),
               strong("Expected Portfolio Return:"),
               textOutput("expected_return"),
               strong("Portfolio Standard Deviation:"),
               textOutput("standard_deviation"),
               numericInput("desired_return", "Enter a desired return rate:", min=0, max = 1, value=.05),
               actionButtonRow("calculate_weights", "Find Weights"),
               tableOutput("weights_table")),
      tabPanel("Timeseries Analysis",
               textOutput("timeseries_error"),
               uiOutput("timeseries_plot.ui")),
      tabPanel("Modeling", uiOutput("model_plots.ui")),
      tabPanel("Financial Analysis",
               textOutput("financial_error"),
               uiOutput("correlation_plot.ui"),
               plotOutput("combination_plot")),
      tabPanel("Help")
    )
  )
))
