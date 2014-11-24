shinyUI(fluidPage(
  titlePanel("Finance-O-Tron"),
  
  sidebarPanel(
    textInput("symbol", "Enter a stock symbol:"),
    checkboxInput("fundamental", "Fundamental Analysis", value = FALSE),
    checkboxInput("timeseries", "Timeseries Analysis", value = FALSE),
    checkboxInput("financial", "Financial Analysis", value = FALSE),
    dateRangeInput("range", "Date Range:", start = as.Date(ymd(Sys.Date()) - years(1)), end = as.Date(ymd(Sys.Date()))),
    actionButtonRow("add_stock", "Add Stock"),
    actionButtonRow("clear_stocks", "Clear Stocks")
  ),
  
  mainPanel(
    textOutput("symbols"),
    plotOutput("plot")
  )
))