# Load required libraries
library(shiny)
library(DT)
# library(ggplot2)
# library(dplyr)
# library(lubridate)
# library(bslib)
# library(ggExtra)

# Source helper functions
source("./R/data_functions.R")
source("./R/plot_functions.R")
source("./R/helper_functions.R")

# Load and prepare data once
company_data <- read.csv("data/raw/Companies.csv")
# sales_data <- read.csv("data/processed/sales_clean.csv") %>%
#   mutate(date = as.Date(date))