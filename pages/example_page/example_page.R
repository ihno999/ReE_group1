source("pages/example_page/tabs/graph.R")
source("pages/example_page/tabs/table.R")

server_example_page <- function(input, output) {
  server_graph(input, output)
  server_table(input, output)
}

ui_example_page <- fluidPage(
  titlePanel("Hello World!!"),
  tabsetPanel(
    tabPanel('Table', ui_table),
    tabPanel('Graph', ui_graph)
  )
)