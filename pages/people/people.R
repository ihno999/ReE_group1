source("pages/people/tabs/graph.R")
source("pages/people/tabs/table.R")

server_people <- function(input, output) {
  server_graph(input, output)
  server_table(input, output)
}

ui_people <- fluidPage(
  tabsetPanel(
    tabPanel('Table', ui_table),
    tabPanel('Graph', ui_graph)
  )
)