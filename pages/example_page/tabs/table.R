### UI
ui_table <- sidebarLayout(
  sidebarPanel(sliderInput('nb_bins', 'Number of bins', 5, 1000 , 500)),
  mainPanel(DT::DTOutput("companies_table"))
)

### Server
server_table <- function(input, output) {
  output$companies_table <- DT::renderDT({
    company_data %>%
      DT::datatable()
  })
}

