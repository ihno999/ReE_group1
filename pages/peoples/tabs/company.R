### UI
ui_table_company <- sidebarLayout(
  sidebarPanel(
    h4("Side Panel"),
    uiOutput("select_input"),
    checkboxGroupInput(
      "checkbox_projects",
      "Filter by Project:",
      choices = NULL,
      selected = NULL
    ),
    width = 3
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
    ),
    textOutput("email_note"),
    verbatimTextOutput("pooop")
  )
)


### Server

server_table_company <- function(input, output, session, rv) {
  output$email_note <- renderText({
    "Note: Emails do not correspond to contact names the way we assume they should. This is due to mocking of data."
  })

  output$pooop <- renderText({
    rv$selection
  })

  output$select_input <- renderUI({
    selectInput("select_company", "Select a company:",
      choices = c("", unique(company_data$name)),
      selected = rv$selected_node_company_name
    )
  })

  # Filter for company data

  joined_company_data_2 <- reactive({
    company_data %>%
      left_join(company_contacts_data, by = "company_id") %>%
      left_join(project_board_data, by = "contact_id") %>%
      left_join(projects_data, by = "project_id") %>%
      left_join(researchers_data, by = c("responsible_employee" = "employee_id")) %>%
      rename(
        company_name = name.x,
        contact_name = name.y,
        project_name = name.x.x,
        researcher_name = name.y.y
      )
  })

  # Gets available projects for selected company
  available_projects <- reactive({
    if (!is.null(input$select_company) && input$select_company != "") {
      data <- joined_company_data_2() %>%
        filter(company_name == input$select_company)

      if (nrow(data) > 0) {
        project_choices <- unique(data$project_name)
        return(project_choices)
      }
    }
    return(character(0))
  })

  # Observes changes to selected company and update checkbox
  observe({
    projects <- available_projects()

    updateCheckboxGroupInput(
      session = session,
      inputId = "checkbox_projects",
      choices = projects,
      selected = if (length(rv$selected_node_connected_projects) == 0) projects else rv$selected_node_connected_projects
      # selected = if(length(projects) > 0) projects else NULL
    )
  })

  # Filtered data with both company and project filters
  filtered_company_data <- reactive({
    data <- joined_company_data_2()

    # Apply company filter
    if (!is.null(input$select_company) && input$select_company != "") {
      data <- data[data$company_name == input$select_company, , drop = FALSE]
    }

    # Apply project filter (if any checkboxes are selected)
    if (!is.null(input$checkbox_projects) && length(input$checkbox_projects) > 0) {
      # Filter by 'project_name'
      data <- data[data$project_name %in% input$checkbox_projects, , drop = FALSE]
    }

    data
  })


  # Render the filtered company table
  output$company_table <- DT::renderDataTable({
    df <- filtered_company_data()

    # Format company description
    if ("company_description" %in% colnames(df) || "description.x" %in% colnames(df)) {
      col_name <- ifelse("company_description" %in% colnames(df), "company_description", "description.x")
      df[[col_name]] <- sapply(df[[col_name]], function(text) {
        if (is.na(text) || text == "") {
          return("")
        }
        paste0(
          '<details><summary>Show</summary><p style="width: 500px">',
          text,
          "</p></details>"
        )
      })
    }

    # Format project description
    if ("project_description" %in% colnames(df) || "description.y" %in% colnames(df)) {
      col_name <- ifelse("project_description" %in% colnames(df), "project_description", "description.y")
      df[[col_name]] <- sapply(df[[col_name]], function(text) {
        if (is.na(text) || text == "") {
          return("")
        }
        paste0(
          '<details><summary>Show</summary><p style="width: 500px">',
          text,
          "</p></details>"
        )
      })
    }
    # Columns to hide
    columns_to_hide <- c(
      "company_id",
      "contact_id",
      "project_id",
      "responsible_group",
      "responsible_employee",
      "total_budget",
      "funding_source",
      "start_date",
      "end_date",
      "main_research_group"
    )

    # Remove columns that exist in the dataframe
    existing_columns_to_hide <- columns_to_hide[columns_to_hide %in% colnames(df)]
    df <- df[, !colnames(df) %in% existing_columns_to_hide, drop = FALSE] %>%
      na.omit() %>%
      select(project_name, type, researcher_name, contact_name, department, job_title, email, phone, role, description.y) %>%
      rename(
        ext_contact_name = contact_name,
        ext_department = department,
        ext_job_title = job_title,
        ext_email = email,
        ext_phone = phone,
        ext_role = role,
        int_contact_name = researcher_name,
        project_description = description.y
      )

    DT::datatable(
      df,
      options = list(pageLength = 10, scrollX = FALSE),
      escape = FALSE
    )
  })
}
