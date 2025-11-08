source("./global.R")
source("pages/example_page/example_page.R")

### Configuration
options(shiny.port=8080)
options(shiny.launch.browser=FALSE)
options(shiny.autoreload=TRUE)


### Server
server <- function(input, output) {
  server_example_page(input, output)
}


### UI
ui <- page_navbar(
  title = "RnE",
  bg = "#2D89C8",
  inverse = TRUE,
  nav_panel(title = "One", ui_example_page),
  nav_panel(title = "Two", p("Second page content.")),
  nav_panel(title = "Three", p("Third page content."))
)

shinyApp(ui = ui, server = server)