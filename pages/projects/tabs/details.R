### Parameters
# p_researcher_name <- "Kathleen Bailey"
p_project_fields <- c("Circular Economy", "Digital Education", "BioTech", "Cybersecurity")


### UI
ui_details_projects_page <- sidebarLayout(
  sidebarPanel(
    uiOutput("projects_page_details_researcher_name_output"),
    uiOutput("projects_page_details_project_fields_checkboxes_output"),
    width = 2
  ),
  mainPanel(
    card(plotOutput("projects_page_details_stacked_bar_chart_output"), full_screen = TRUE),
    card(
      h4("Project Details"),
      div(
        dataTableOutput("projects_page_graph_network_df_output_2"),
        style = "font-size:80%"
      ),
      full_screen = TRUE
    )
  )
)


### Server
# signature includes session and shared reactive state rv
server_details_projects_page <- function(input, output, session, rv) {
  output$projects_page_details_project_fields_checkboxes_output <- renderUI({
    project_fields <- distinct(df_for_project_details_stacked_bar_chart, project_field) %>%
      unlist(use.names = FALSE) %>%
      sort()

    # determine selected values: prefer shared rv, then graph inputs, then defaults
    if (!is.null(rv$fields)) {
      selected_vals <- intersect(rv$fields, project_fields)
      if (length(selected_vals) == 0) selected_vals <- project_fields
    } else if (!is.null(input$projects_page_graph_project_fields_checkboxes)) {
      selected_vals <- intersect(input$projects_page_graph_project_fields_checkboxes, project_fields)
      if (length(selected_vals) == 0) selected_vals <- project_fields
    } else {
      selected_vals <- intersect(p_project_fields, project_fields)
      if (length(selected_vals) == 0) selected_vals <- project_fields
    }

    checkboxGroupInput(
      "projects_page_details_project_fields_checkboxes",
      "Project fields", choices = project_fields,
      selected = selected_vals
    )
  })

  output$projects_page_details_researcher_name_output <- renderUI({

    selectInput("projects_page_details_researcher_name", "Researcher  ",
                choices = c("", researchers_data$name),
                selected = rv$selection
                # selected = rv$selected_node_researcher_name
    )
  })

  output$projects_page_graph_selection_output_23 <- renderText({
    input$projects_page_graph_project_fields_checkboxes
  })

  df_filtered_for_project_details_stacked_bar_chart <- reactive({
    req(input$projects_page_details_researcher_name, input$projects_page_details_project_fields_checkboxes)
    df_for_project_details_stacked_bar_chart %>%
      filter(researcher_name == input$projects_page_details_researcher_name) %>%
      filter(project_field %in% input$projects_page_details_project_fields_checkboxes) %>%
      arrange(desc(sort_digit))
  })

  output$projects_page_details_stacked_bar_chart_output <- renderPlot({
    # Ensure required inputs are available so plot updates when they change
    req(input$projects_page_details_researcher_name)
    req(input$projects_page_details_project_fields_checkboxes)

    plot_df <- df_filtered_for_project_details_stacked_bar_chart()

    # Show a message in the plot area when there's no data to plot
    validate(
      need(nrow(plot_df) > 0, "No data available for the selected researcher / fields")
    )

    ggplot(plot_df, aes(fill = project_field, y = sort_digit, x = fct_reorder(company_name, desc(sort_digit)))) +
      geom_bar(position = "stack", stat = "identity", width = 0.8) +
      xlab("Company") +
      ylab("Projects involved") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme(text = element_text(size = 16)) +
      ggtitle("Project field distribution across companies related to choosen researcher")
  })

  output$projects_page_details_plot_output <- renderPlot({
    ggplot(data = penguins, aes(body_mass_g)) +
      geom_histogram(bins = input$projects_page_details_slider)
  })

  output$projects_page_details_stacked_bar_chart_df_output <- renderDataTable(
    {
      df_filtered_for_project_details_stacked_bar_chart()
    },
    filter = "top"
  )

  # --- push details inputs -> shared rv ---
  observeEvent(input$projects_page_details_researcher_name, {
    rv$selection <- input$projects_page_details_researcher_name
  }, ignoreInit = TRUE)

  observeEvent(input$projects_page_details_project_fields_checkboxes, {
    rv$fields <- input$projects_page_details_project_fields_checkboxes
  }, ignoreInit = TRUE)

  # --- react to shared rv -> details inputs (keep UI synced when graph changes) ---
  observe({
    # update researcher name text input if needed
    if (!is.null(rv$selection) && !identical(rv$selection, input$projects_page_details_researcher_name)) {
      try(updateTextInput(session, "projects_page_details_researcher_name", value = rv$selection), silent = TRUE)
    }
    # update fields checkbox group if needed
    if (!is.null(rv$fields) && !identical(rv$fields, input$projects_page_details_project_fields_checkboxes)) {
      try(updateCheckboxGroupInput(session, "projects_page_details_project_fields_checkboxes", selected = rv$fields), silent = TRUE)
    }
  })
}