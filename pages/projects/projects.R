source("pages/projects/tabs/graph.R", local = TRUE)
source("pages/projects/tabs/details.R", local = TRUE)

server_projects <- function(input, output, session) {
  server_details_projects_page(input, output)
  server_graph_projects_page(input, output, session)
}

ui_projects <- fluidPage(
  tabsetPanel(
    tabPanel("Graph", ui_graph_projects_page),
    tabPanel("Details", ui_details_projects_page)
  )
)
