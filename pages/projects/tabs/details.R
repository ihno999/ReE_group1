### UI
ui_details_projects_page <- sidebarLayout(
  sidebarPanel(
    textInput("projects_page_details_researcher_name", "Researcher name", ""),
    uiOutput('projects_page_details_project_fields_checkboxes_output'),
    width=2
  ),
  mainPanel(
    card(tableOutput("projects_page_details_stacked_bar_chart_table_output")),
    card(plotOutput('projects_page_details_stacked_bar_chart_output'), full_screen=TRUE),
    card(verbatimTextOutput("projects_page_details_fields_output")),
    card(div(dataTableOutput("projects_page_details_stacked_bar_chart_df_output"), style = "font-size:80%"), full_screen=TRUE)
  )
)


### Server
server_details_projects_page <- function(input, output) {
  output$projects_page_details_project_fields_checkboxes_output <- renderUI({
    project_fields <- distinct(df_for_project_details_stacked_bar_chart, project_field)
    checkboxGroupInput(
      "projects_page_details_project_fields_checkboxes",
        "Fields:", project_fields %>% unlist(use.names = FALSE)
    )
  })

  df_filtered_for_project_details_stacked_bar_chart <- reactive({
    df_for_project_details_stacked_bar_chart %>%
      filter(researcher_name == input$projects_page_details_researcher_name) %>%
      arrange(desc(sort_digit))
  })

  output$projects_page_details_stacked_bar_chart_output <- renderPlot({
    ggplot(df_filtered_for_project_details_stacked_bar_chart(), aes(fill=project_field, y=sum_digit, x=fct_reorder(company_name, desc(sort_digit)))) +
      geom_bar(position="stack", stat="identity") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme(text=element_text(size=16)) +
      ggtitle("Researcher's projects in different companies and fields")
  })

  output$projects_page_details_plot_output <- renderPlot({
    ggplot(data = penguins, aes(body_mass_g)) +
      geom_histogram(bins = input$projects_page_details_slider)
  })

  output$projects_page_details_fields_output <- renderText({ input$projects_page_details_project_fields_checkboxes })

  df_researcher_details <- data.frame(
    "Name" = df_researchers_and_groups$name.x,
    "Research Group" = df_researchers_and_groups$name.y,
    check.names = FALSE
  )

  output$projects_page_details_stacked_bar_chart_table_output <- renderTable({
    df_researcher_details %>% filter(Name == input$projects_page_details_researcher_name)
  })

  output$projects_page_details_stacked_bar_chart_df_output <- renderDataTable({ df_filtered_for_project_details_stacked_bar_chart() })
}