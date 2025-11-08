source("pages/projects/tabs/graph.R")
source("pages/projects/tabs/table.R")

server_projects <- function(input, output) {
  server_graph(input, output)
  server_table(input, output)
}

ui_projects <- fluidPage(
  tabsetPanel(
    tabPanel('Table', ui_table),
    tabPanel('Graph', ui_graph)
  )
)