source("./global.R")
source("pages/projects/projects.R", local=TRUE)
source("pages/peoples/companies.R", local=TRUE)

### Configuration
options(shiny.port=8080)
options(shiny.launch.browser=FALSE)
options(shiny.autoreload=TRUE)


### Server
server <- function(input, output) {
  server_projects(input, output)
  # server_people(input, output)
  server_peoples(input, output)

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