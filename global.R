# Load required libraries
library(shiny)
library(DT)
library(dplyr)
# library(lubridate)
library(bslib)
# library(ggExtra)
library(ggplot2)
library(palmerpenguins)
library(viridis)
library(hrbrthemes)
library(duckdb)

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

# Custom views.
df_researchers_and_groups <- merge(researchers_data, research_groups_data, by.x="main_research_group", by.y="group_id")
# df_general <- merge(projects_data, research_participation_data, by.x='project_id', by.y='project_id')
# df_general <- merge(df_general, researchers_data, by.x='researcher_id', by.y='employee_id')
# df_general <- merge(df_general, research_groups_data, by.x='main_research_group', by.y='group_id')
# df_general <- merge(df_general, research_participation_data, by.x='project_id', by.y='project_id')
# df_general <- merge(df_general, research_participation_data, by.x='project_id', by.y='project_id')
# df_general <- merge(df_general, research_participation_data, by.x='project_id', by.y='project_id')
# sales_data <- read.csv("data/processed/sales_clean.csv") %>%
#   mutate(date = as.Date(date))

# DuckDB
con <- dbConnect(duckdb())
duckdb_register(con, "company", company_data)
duckdb_register(con, "company_contacts", company_contacts_data)
duckdb_register(con, "project_board", project_board_data)
duckdb_register(con, "projects", projects_data)
duckdb_register(con, "research_groups", research_groups_data)
duckdb_register(con, "research_participation", research_participation_data)
duckdb_register(con, "researchers", researchers_data)