# Load required libraries
library(shiny)
library(DT)

# library(ggExtra)0
library(ggExtra)
library(dplyr)
library(visNetwork)
library(lubridate)
library(bslib)
library(ggplot2)
library(viridis)
library(duckdb) # commented out for deployment
library(forcats)
library(igraph)

# Source helper functions
source("./R/data_functions.R")
source("./R/plot_functions.R")
source("./R/helper_functions.R")
source("./R/data_sql_queries.R")

# Load and prepare data once
company_data <- read.csv("data/raw/Companies.csv")
company_contacts_data <- read.csv("data/raw/Company_Contacts.csv")
project_board_data <- read.csv("data/raw/Project_Board.csv")
projects_data <- read.csv("data/raw/Projects.csv")
research_groups_data <- read.csv("data/raw/Research_Groups.csv")
research_participation_data <- read.csv("data/raw/Research_Participation.csv")
researchers_data <- read.csv("data/raw/Researchers.csv", stringsAsFactors = FALSE, check.names = TRUE)
# sales_data <- read.csv("data/processed/sales_clean.csv") %>%
#   mutate(date = as.Date(date))


# DuckDB (only locally, not in deployed version/ commented out for deployment)
con <- dbConnect(duckdb())
duckdb_register(con, "companies", company_data)
duckdb_register(con, "company_contacts", company_contacts_data)
duckdb_register(con, "project_board", project_board_data)
duckdb_register(con, "projects", projects_data)
duckdb_register(con, "research_groups", research_groups_data)
duckdb_register(con, "research_participation", research_participation_data)
duckdb_register(con, "researchers", researchers_data)

# Custom views.
df_researchers_and_groups <-
    merge(researchers_data, research_groups_data, by.x = "main_research_group", by.y = "group_id") %>%
    rename("researcher_name" = "name.x", "main_research_group_name" = "name.y")


# ------------------------------------------------------------------------------

# Dataframes from SQL queries                                                                                       (local version)
df_general <- dbGetQuery(con, q_df_general)
df_general_with_project_fields <- dbGetQuery(con, q_df_general_with_project_fields)
df_for_project_details_stacked_bar_chart <- dbGetQuery(con, q_df_for_project_details_stacked_bar_chart)

# Load precomputed queries instead of DB queries                                                              (deployed/github version)
# df_general <- readRDS("data/processed/df_general.rds")
# df_general_with_project_fields <- readRDS("data/processed/df_general_with_project_fields.rds")
# df_for_project_details_stacked_bar_chart <- readRDS("data/processed/df_for_project_details_stacked_bar_chart.rds")
# df_for_project_graph_network <- readRDS("data/processed/df_for_project_graph_network.rds")

# don't forget to create these RDS files when updating the database or the queries!
# Create local files once (execute in R console/terminal):
# saveRDS(df_for_project_graph_network, "data/processed/df_for_project_graph_network.rds")
# saveRDS(df_general, "data/processed/df_general.rds")
# saveRDS(df_general_with_project_fields, "data/processed/df_general_with_project_fields.rds")

# ------------------------------------------------------------------------------

# bq1_a1_data
df_for_project_graph_network <- dbGetQuery(con, q_df_for_project_graph_network)
