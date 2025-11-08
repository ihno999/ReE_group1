source("./pages/example_page/graph_example_page.R")
source("./pages/example_page/table_example_page.R")

example_page <- fluidPage(
  titlePanel("Hello World!!"),
  tabsetPanel(
    tabPanel('Table', table_example_page),
    tabPanel('Graph', graph_example_page)
  )
)



