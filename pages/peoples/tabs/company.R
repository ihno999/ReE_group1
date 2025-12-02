ui_table_company <- sidebarLayout(
  sidebarPanel(
    h4("Side Panel"),
    selectInput("select_company", "Select a company:",
      choices = c("", unique(company_data$name))
    ),
    width = 2
  ),
  mainPanel(
    card(
      h3("Company Overview"),
      div(
        style = "overflow-y: visible; overflow-x: visible; height: auto; width: 100%; ",
        dataTableOutput("company_table")
      ),
      full_screen = TRUE,
      style = " overflow-x: visible; overflow-y: visible;"
    )
  )
)

server_table_company <- function(input, output, session) {
  # Filter for company data
  joined_company_data <- reactive({
    company_data %>%
      inner_join(company_contacts_data, by = "company_id")
  })

  filtered_company_data <- reactive({
    data <- joined_company_data()

    if (!is.null(input$select_company) && input$select_company != "") {
      data <- data[data$name.x == input$select_company, , drop = FALSE]
    }

    data
  })


  # Render the filtered company table
  output$company_table <- DT::renderDataTable({
    df <- filtered_company_data()

    if ("description" %in% colnames(df)) {
      df$description <- sapply(df$description, function(text) {
        paste0(
          '<details><summary>Show</summary><p style="width: 500px">',
          text,
          "</p></details>"
        )
      })
    }

    DT::datatable(
      df,
      options = list(pageLength = 10, scrollX = FALSE),
      escape = FALSE
    )
  })
}
