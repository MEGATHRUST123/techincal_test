####################################################################################################################
# This script contains the main code that creates shiny dashboard.
# Capacity Dashboard Version 4 developed by Kelly Tay
####################################################################################################################

# Shiny Packages ----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(shinyhelper)

# Data Manipulation Packages ----
library(lubridate)
library(dplyr)
library(DT)
library(gtable)     
library(tidyverse)
library(ggvis)

# Data Visualisation Packages ----
library(plotly)
library(directlabels)
library(ggpubr)
library(sparkline)
library(formattable)
library(sparkline)
library(htmlwidgets)
library(grid)
library(gridExtra)
library(ggthemes)

# Workout Hours Forecasting ----
library(forecast)
library(prophet)
library(tseries)
library(RQuantLib)
library(bizdays)

# Run this script to load the MTK Datasets
source("C:/Users/kellytay/Desktop/kelly/Digital MTK Instrumentation/Shiny Prototype/lib_codes/capacity_data.R")

# Run this script to load functions used to design shiny Dashboard UI
source("C:/Users/kellytay/Desktop/kelly/Digital MTK Instrumentation/Shiny Prototype/lib_codes/capacity_html.R")

# Run this script to load functions used for MTK calculations
source("C:/Users/kellytay/Desktop/kelly/Digital MTK Instrumentation/Shiny Prototype/lib_codes/common_functions.R")

# Run this script to load function used for forecasting 
source("C:/Users/kellytay/Desktop/kelly/Digital MTK Instrumentation/Shiny Prototype/lib_codes/capacity_forecast.R")

# Create the shinydashboard - Calls R Shiny UI and Server
runApp("C:\\Users\\kellytay\\Desktop\\kelly\\Digital MTK Instrumentation\\Shiny Prototype")
