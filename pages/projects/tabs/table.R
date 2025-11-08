### UI
ui_table <- sidebarLayout(
  sidebarPanel(sliderInput('projects_page_nb_bins', 'Number of bins', 5, 1000 , 500)),
  mainPanel(DT::DTOutput("projects_page_companies_table"))
)

### Server
server_table <- function(input, output) {
  output$projects_page_companies_table <- DT::renderDT({
    company_data %>%
      DT::datatable()
  })
}

