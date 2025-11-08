source("./global.R")
source("./pages/example_page/server_example_page.R")


server <- function(input, output) {
  output$companies_table <- server_example_page_output_companies_table
  output$companies_graph <- server_example_page_output_companies_graph
}