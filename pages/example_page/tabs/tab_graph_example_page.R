server_example_page_output_companies_graph <- DT::renderDT({
  company_data %>%
    DT::datatable()
})