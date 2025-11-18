source("pages/people/tabs/graph.R")
source("pages/people/tabs/table.R")

server_people <- function(input, output) {
  server_graph_people_page(input, output)
  server_table_people_page(input, output)
}

ui_people <- fluidPage(
  tabsetPanel(
    tabPanel('Table', ui_table_people_page),
    tabPanel('Graph', ui_graph_people_page)
  )
)