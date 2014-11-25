shinyUI(fluidPage(
  progressInit(),
  
  titlePanel("Finance-O-Tron"),
  
  sidebarPanel(
    textInput("symbol", "Enter a stock symbol:"),
    dateRangeInput("range", "Date Range:", start = as.Date(ymd(Sys.Date()) - years(1)), end = as.Date(ymd(Sys.Date()))),
    actionButtonRow("add_stock", "Add Stock"),
    actionButtonRow("clear_stocks", "Clear Stocks"),
    tags$div(style="margin-bottom:15px;"),
    checkboxInput("fundamental", "Fundamental Analysis", value = FALSE),
    checkboxInput("timeseries", "Timeseries Analysis", value = TRUE),
    checkboxInput("financial", "Financial Analysis", value = FALSE),
    actionButtonRow("apply_analysis", "Apply")
  ),
  
  mainPanel(
    textOutput("symbols"),
    textOutput("error"),
    tags$head(tags$style("#error{color: red;}")),
    uiOutput("plot.ui")
  )
))