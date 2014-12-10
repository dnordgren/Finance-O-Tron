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
    actionButtonHalf("clear_stocks", "Clear Stocks")
  ),

  mainPanel(
    textOutput("symbols"),
    textOutput("error"),
    tags$head(tags$style("#error{color: red;}")),
    tabsetPanel(
      tabPanel("Timeseries Analysis",
        uiOutput("timeseries_plot.ui")),
      tabPanel("Modeling",
        uiOutput("modeling_stock_symbols"),
        plotOutput("beta_gamma"),
        plotOutput("beta"),
        plotOutput("neither")),
      tabPanel("Financial Analysis", plotOutput("combination_plot")),
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
    )
  )
))
