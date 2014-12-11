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
    dateRangeInput("range", "Date Range:", start = as.Date(ymd(Sys.Date()) - years(3)), end = as.Date(ymd(Sys.Date()))),
    actionButtonHalf("add_stock", "Add Stock"),
    actionButtonHalf("clear_stocks", "Clear All Stocks"),
    tags$div(style="margin-bottom:15px;"),
    uiOutput("remove_stock_symbols"),
    actionButtonRow("remove_stocks", "Remove Specific Stocks"),
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
               textOutput("standard_deviation")),
      tabPanel("Optimization",
               numericInput("desired_return", "Enter a desired return rate:", min=0, max = 1, value=.05),
               actionButtonRow("calculate_weights", "Find Weights"),
               tags$div(style="margin-bottom:15px;"),
               tableOutput("weights_table"),
               strong("Portfolio Return:"),
               textOutput("optimized_expected_return"),
               strong("Standard Deviation:"),
               textOutput("optimized_standard_deviation")),
      tabPanel("Timeseries Analysis",
               textInput("ma1",
                         label=strong("Short Moving Average Period"),
                         value="50"),
               textInput("ma2",
                         label=strong("Long Moving Average Period"),
                         value="100"),
               actionButtonRow("update_mas",
                               "Update Moving Averages"),
               uiOutput("timeseries_plot.ui")),
      tabPanel("Modeling",
               uiOutput("modeling_stock_symbols"),
               textOutput("forecast_error"),
               plotOutput("beta_gamma"),
               plotOutput("beta"),
               plotOutput("neither")),
      tabPanel("Financial Analysis",
               textOutput("financial_error"),
               selectInput("correlation", "Choose Correlation View", c("Plot", "Table"), selected = "Plot", multiple = FALSE, selectize = TRUE),
               uiOutput("correlation_ui"),
               plotOutput("combination_plot")),
      tabPanel("Help",
               h3("Inputs"),
               strong("Stock Symbol"),
               p("Any valid NYSE ticker symbol."),
               tags$div(style="margin-bottom:15px;"),
               strong("Weight"),
               p("All weights per stock you have entered will be used to calculate the
                 respective percentage weight for each stock."),
               p("i.e. 1, 1, and 2 will result in 25%, 25%, and 50%, respectively."),
               tags$div(style="margin-bottom:15px;"),
               strong("Date Range"),
               p("The date range you enter for your first stock will be locked in for
                 all successive stocks added. Hit 'Clear Stocks' to start over."),
               tags$div(style="margin-bottom:15px;"),
               h3("Areas of Analysis"),
               strong("Portfolio"),
               p("Each stock and its respective weight that you entered will be displayed
                 here as you build your portfolio. Each stock's expected return and standard
                 deviation will also be displayed. In addition, this analysis area allows
                 the user to specify a desired rate of return for their portfolio. Finance-O-Tron
                 will determine what percentage of your investment you should make in each
                 stock in order to achieve this return rate. The standard deviation of your portfolio
                 will also be calculated."),
               tags$div(style="margin-bottom:15px;"),
               strong("Timeseries Analysis"),
               p("Each ticker symbol that you add will have its daily adjusted closing
                 price history over the specified period plotted here. In addition, it
                 will have the 50- and 100-day moving average plotted."),
               tags$div(style="margin-bottom:15px;"),
               strong("Modeling"),
               p("Using Holt-Winters timeseries forecasting algorithms, Finance-O-Tron
                 will forecast future adjusted closing prices for the stocks that you've
                 entered."),
               tags$div(style="margin-bottom:15px;"),
               strong("Financial Analysis"),
               p("All possible combinations of weights of the stocks that you've entered
                are plotted for all possible returns.")
        )
    ), width = 10
  )
))
