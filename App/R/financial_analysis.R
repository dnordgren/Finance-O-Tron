years <- 10
start_date <- as.Date(ymd(Sys.Date()) - years(years))
end_date <- Sys.Date()

erp <- NULL
gspc_data <- NULL
tnx_rate <- NULL

get_market_data <- function(){
  # US stock market (S&P 500) values
  gspc_data <<- Quandl("YAHOO/INDEX_GSPC", start_date=start_date, end_date=end_date)
  # Risk-free rate of return values
  tnx_data <- Quandl("YAHOO/INDEX_TNX", start_date=start_date, end_date=end_date)
  # Reverse the data to get the normal form
  gspc_data <<- gspc_data[nrow(gspc_data):1,]
  tnx_data <- tnx_data[nrow(tnx_data):1,]
  # Calculate the rates of return for gspc
  gspc_rates <- get_gspc_rates(gspc_data$Date[1])
  # Find the Average rate for each, and then annualize it
  gspc_rate <- mean(gspc_rates)
  gspc_rate <- (1 + gspc_rate)^(length(gspc_rates)/years) - 1
  # TNX data is already annualized, just need to find the average
  tnx_rate <<- mean(tnx_data$'Adjusted Close'/100)
  # Calculate the equity risk premium
  erp <<- gspc_rate - tnx_rate
}

get_gspc_rates <- function(start_date){
  if(as.Date(start_date) < gspc_data$Date[1]){
    return(NULL)
  }
  data <- gspc_data[as.Date(gspc_data$Date) >= as.Date(start_date),]
  return(diff(data$'Adjusted Close')/data$'Adjusted Close'[-length(data$'Adjusted Close')])
}

# Returns the expected return and standard deviation for the specefied portfolio element weights
calculate_plot_point <- function(weights, returns, expected_returns)
{
  expected_return <- calculate_expected_return(expected_returns, weights)
  # Create dataframe of stock rates adjusted by weights
  weighted_stock_rates <- as.data.frame(mapply(calculate_weighted_returns, returns, weights))
  # Calculate the standard deviation
  standard_deviation <- calculate_standard_deviation(weighted_stock_rates)
  c(expected_return, standard_deviation)
}

# Find the standard deviation for a weighted collection of return rates for each item in a portfolio
calculate_standard_deviation <- function(weighted_rates)
{
  # Sum each row for single return rates per period
  single_rates <- rowSums(weighted_rates)
  #Calculate the standard deviation
  sd(single_rates)
}

# Creates a matrix of all possible combinations of weights
calculate_weights_matrix <- function(stock_number, sum)
{
  if(stock_number == 1)
  {
    return(matrix(1-sum))
  }
  else
  {
    new_values <- seq(0,1-sum,.01)
    previous <- lapply(new_values, function(thing){
      matrix_continued <- calculate_weights_matrix(stock_number-1, sum+thing)
      cbind(thing,matrix_continued)
    })
    do.call("rbind", previous)
  }
}

calculate_returns <- function(prices, dates){
  # Determine frequency
  years <- as.duration(ymd(tail(dates, 1)) - ymd(head(dates, 1))) /  as.duration(years(1))
  number_per_year <- round(length(prices)/years)
  # Calculate the rates of return for the symbol
  stock_ts <- ts(prices, freq=number_per_year, start=c(year(start_date), month(start_date)))
  rates <- diff(prices)/prices[-length(prices)]
  rates
}

calculate_weights <- function(weight_integers){
  totalWeight <- sum(weight_integers)
  return(weight_integers/totalWeight)
}

# Find the expected return for a portofilio based on individual expected_returns and corresponding weights
calculate_expected_return <- function(expected_returns, weights)
{
  sum(mapply(calculate_weighted_return, expected_returns,weights))
}

# Weights a single expected return
calculate_weighted_return <- function(expected_return,weight)
{
  expected_return * weight
}

# Weights a full series of returns
calculate_weighted_returns <- function(returns,weights)
{
  # Multiply weight by each element in column
  weights * returns
}

calculate_stock_returns <- function(stock_data){
  returns <- apply(stock_data[-1], 2, function(column){
    returns <- calculate_returns(column, stock_data$Date)
  })
}

calculate_expected_returns <- function(returns, gspc_rates){
  # Calculate the expected return for each stock
  expected_returns <- apply(returns, 2, function(column)
  {
    res <- lm(column~gspc_rates)
    # Beta is the slope of the Linear model
    beta <- res$coefficients[[2]]
    # Find the expected return using the formula from the CAPM model
    expected_return <- tnx_rate + beta*erp
    expected_return
  })
}

create_combination_plot <- function(num_stocks, returns, expected_returns){
  # Builds a data frame that contains all combinations of three weights for step size = .01
  plot_weights <- calculate_weights_matrix(num_stocks, 0)

  # For each weight combination, calculate the mean return rate and standard deviation to plot
  points <- as.data.frame(t(apply(plot_weights,1,calculate_plot_point, returns, expected_returns)))

  # Plot the points, putting standard deviation on the x-axis and mean rate of return on the y-axis
  qplot(points, x=points[,2], y=points[,1], xlab="Standard Deviation", ylab="Mean Rate of Return", main="Portfolio Combinations Plot")
}

create_correlation_plot <- function(returns, num_stocks){
  cor_matrix <- cor(returns)

  # Need to adjust the data format to be able to plot it
  print_cor <- melt(cor_matrix)
  return(NULL)
  #plot <- ggplot(print_cor, aes(x=Var1, y=value)) +
  #  geom_bar() +
  #  facet_grid(~ Var | Var2)
  #plot

  # Plot the correlation on a barchart using lattice
  # return(barchart(value ~ Var1 | Var2,
  #                data = print_cor,
  #                layout=c(1, num_stocks),
  #                main="Porfolio Correlations",
  #                scales=list(x=list(rot=90))))
}

calculate_weight_combination <- function(desired_rate, stock_names, returns, expected_returns){
  # Variables needed to solve for weights
  cov_matrix <- cov(returns)
  risk <- rep(0,length(stock_names))
  amat <- matrix(c(rep(1, nrow(cov_matrix)), unlist(expected_returns)), nrow=nrow(cov_matrix))
  bvec <- c(1, desired_rate)
  meq <- 2

  # From quadprog package. Calculates the weights needed to reach a specified mean rate of return while minimizing standard deviation
  calculated_weights <- solve.QP(cov_matrix, risk, amat, bvec, meq, factorized=FALSE)$solution
}
