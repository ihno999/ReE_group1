server_example_page_output_companies_table <- DT::renderDT({
  company_data %>%
    DT::datatable()
})