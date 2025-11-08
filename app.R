source("./global.R")
source("pages/projects/projects.R", local=TRUE)
source("pages/people/people.R", local=TRUE)

### Configuration
options(shiny.port=8080)
options(shiny.launch.browser=FALSE)
options(shiny.autoreload=TRUE)


### Server
server <- function(input, output) {
  server_projects(input, output)
  server_people(input, output)
}


### UI
ui <- page_navbar(
  title = "RnE",
  bg = "#2D89C8",
  inverse = TRUE,
  nav_panel(title = "Projects", ui_projects),
  nav_panel(title = "People", ui_people)
)

shinyApp(ui = ui, server = server)