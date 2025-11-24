source("pages/projects/tabs/graph.R", local = TRUE)
source("pages/projects/tabs/details.R", local = TRUE)

# server that creates a per-session shared reactiveValues and passes it to both tab servers
server_projects <- function(input, output, session) {
  rv <- reactiveValues(
    selection = if (exists("p_graph_selection")) p_graph_selection else "All researchers",
    fields = if (exists("p_graph_project_fields")) p_graph_project_fields else c()
  )

  # pass session and shared rv into both page servers so they stay synchronized
  server_graph_projects_page(input, output, session, rv)
  server_details_projects_page(input, output, session, rv)
}

# simple UI combining both tabs (adjust as your app uses)
ui_projects_page <- tabsetPanel(
  tabPanel("Graph", ui_graph_projects_page),
  tabPanel("Details", ui_details_projects_page)
)

# Alias expected by the rest of the app
ui_projects <- ui_projects_page
