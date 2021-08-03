# EURUSD
A model for exchange rate prices for the Euro/US Dollar. The goal here is to do prediction and see if foreign exchange market prices are predictable. We will use some variables from a discipline called Technical Analysis which purports to be able to profit from inefficiencies and predictable behavior in financial markets.

# Required Libraries

install.packages("shiny", "shinydashboard", "DT", "plotly", "tidyverse", "caret")

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(caret)

shiny::runGitHub("PeterLungNCSU", "EURUSD") 


