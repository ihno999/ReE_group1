source("pages/projects/tabs/details.R")
source("pages/projects/tabs/table.R")

server_projects <- function(input, output) {
  server_details_projects_page(input, output)
  server_table_projects_page(input, output)
}

ui_projects <- fluidPage(
  tabsetPanel(
    tabPanel('Table', ui_table_projects_page),
    tabPanel('Details', ui_details_projects_page)
  )
)