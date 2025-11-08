graph_example_page_output_companies_graph <- DT::renderDT({
  company_data %>%
    DT::datatable()
})

graph_example_page <- sidebarLayout(
  sidebarPanel(sliderInput('nb_bins', 'Number of bins', 5, 10 , 5)),
  mainPanel(DT::DTOutput("companies_graph"))
)