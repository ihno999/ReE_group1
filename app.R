source("./global.R")
source("pages/projects/projects.R", local=TRUE)
source("pages/peoples/companies.R", local=TRUE)

### Configuration
options(shiny.port=8080)
options(shiny.launch.browser=FALSE)
options(shiny.autoreload=TRUE)


### Server
server <- function(input, output, session, rv) {
  rv <- reactiveValues(
    selection = if (exists("p_graph_selection")) p_graph_selection else "All researchers",
    fields = if (exists("p_graph_project_fields")) p_graph_project_fields else c(),
    type = if (exists("p_graph_type")) p_graph_type else "Researcher"
  )

  server_projects(input, output, session, rv)
  server_companies(input, output, session, rv)

}


### UI
ui <- page_navbar(
  title = "RnE",
  bg = "#2D89C8",
  inverse = TRUE,
  nav_panel(title = "Researchers", ui_projects),
  nav_panel(title = "Companies", ui_companies)

)

shinyApp(ui = ui, server = server)