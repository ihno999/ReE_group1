source("./global.R")
source("./pages/example_page/example_page.R")

options(shiny.port=8080)
options(shiny.launch.browser=FALSE)
options(shiny.autoreload=TRUE)

server <- function(input, output) {
  output$companies_table <- table_example_page_output_companies_table
  output$companies_graph <- graph_example_page_output_companies_graph
}

ui <- page_navbar(
  title = "My App",
  bg = "#2D89C8",
  inverse = TRUE,
  nav_panel(title = "One", example_page),
  nav_panel(title = "Two", p("Second page content.")),
  nav_panel(title = "Three", p("Third page content.")),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Posit", href = "https://posit.co")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  )
)

shinyApp(ui = ui, server = server)