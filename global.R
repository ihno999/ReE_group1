# Load required libraries
library(shiny)
library(DT)
library(dplyr)
library(visNetwork)
# library(ggplot2)
# library(lubridate)
# library(bslib)
# library(ggExtra)

# Source helper functions
source("./R/data_functions.R")
source("./R/plot_functions.R")
source("./R/helper_functions.R")

# Load and prepare data once
company_data <- read.csv("data/raw/Companies.csv")
company_contacts_data <- read.csv("data/raw/Company_Contacts.csv")
project_board_data <- read.csv("data/raw/Project_Board.csv")
projects_data <- read.csv("data/raw/Projects.csv")
research_groups_data <- read.csv("data/raw/Research_Groups.csv")
research_participation_data <- read.csv("data/raw/Research_Participation.csv")
researchers_data <- read.csv("data/raw/Researchers.csv")
# sales_data <- read.csv("data/processed/sales_clean.csv") %>%
#   mutate(date = as.Date(date))