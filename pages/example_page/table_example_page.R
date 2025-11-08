table_example_page_output_companies_table <- DT::renderDT({
  company_data %>%
    DT::datatable()
})

table_example_page <- sidebarLayout(
  sidebarPanel(sliderInput('nb_bins', 'Number of bins', 5, 1000 , 500)),
  mainPanel(DT::DTOutput("companies_table"))
)