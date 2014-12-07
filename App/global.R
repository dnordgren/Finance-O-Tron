library(ggplot2)
library(lubridate)
library(Quandl)
library(reshape2)
library(shiny)
library(shinyIncubator)

source('HTMLR/input.R')
source('R/data_retrieving.R')
source('R/financial_analysis.R')
source('R/timeseries_analysis.R')
source('R/output.R')

Quandl.auth("Xwpyys22sxHPzyXBrGdH")