shinyUI(fluidPage(
  progressInit(),
  
  titlePanel("Finance-O-Tron"),
  
  sidebarPanel(
    textInput("symbol", "Enter a stock symbol:"),
    dateRangeInput("range", "Date Range:", start = as.Date(ymd(Sys.Date()) - years(1)), end = as.Date(ymd(Sys.Date()))),
    actionButtonHalf("add_stock", "Add Stock"),
    actionButtonHalf("clear_stocks", "Clear Stocks")
  ),
  
  mainPanel(
    textOutput("symbols"),
    textOutput("error"),
    tags$head(tags$style("#error{color: red;}")),
    tabsetPanel(
      tabPanel("Fundamental Analysis"),
      tabPanel("Timeseries Analysis", uiOutput("timeseries_plot.ui")),
      tabPanel("Financial Analysis"),
      tabPanel("Help")
    )
  )
))