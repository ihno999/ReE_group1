source("pages/projects/tabs/graph.R")
source("pages/projects/tabs/table.R")

server_projects <- function(input, output) {
  server_graph_projects_page(input, output)
  server_table_projects_page(input, output)
}

ui_projects <- fluidPage(
  tabsetPanel(
    tabPanel('Table', ui_table_projects_page),
    tabPanel('Graph', ui_graph_projects_page)
  )
)