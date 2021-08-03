# EURUSD
A model for exchange rate prices for the Euro/US Dollar. The goal here is to do prediction and see if foreign exchange market prices are predictable. We will use some variables from a discipline called Technical Analysis which purports to be able to profit from inefficiencies and predictable behavior in financial markets. The raw data is in minute-by-minute cadence which is too large to process. The EURUSD.R script shrinks it and creates the predictor variables.

After working many, many hours on this, I was unfortunately unable to finish by the deadline. I was unable to get it to run from github and a there are a few items I was unable to include or troubleshoot.

# Required Libraries

install.packages("shiny", "shinydashboard", "DT", "plotly", "tidyverse", "caret")

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(caret)

shiny::runGitHub("PeterLungNCSU/EURUSD", "app") 


