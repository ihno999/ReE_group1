### Parameters
# p_researcher_name <- "Kathleen Bailey"
p_project_fields <- c('Circular Economy', 'Digital Education', 'BioTech', 'Cybersecurity')


### UI
ui_details_projects_page <- sidebarLayout(
  sidebarPanel(
    uiOutput("projects_page_details_researcher_name_output"),
    uiOutput('projects_page_details_project_fields_checkboxes_output'),
    width=2
  ),
  mainPanel(
    card(tableOutput("projects_page_details_stacked_bar_chart_table_output")),
    card(plotOutput('projects_page_details_stacked_bar_chart_output'), full_screen=TRUE),
    card(div(dataTableOutput("projects_page_details_stacked_bar_chart_df_output"), style = "font-size:80%"), full_screen=TRUE),
    card(verbatimTextOutput("projects_page_graph_selection_output_23"))
  )
)


### Server
server_details_projects_page <- function(input, output) {
  output$projects_page_details_project_fields_checkboxes_output <- renderUI({
    project_fields <- distinct(df_for_project_details_stacked_bar_chart, project_field)
    checkboxGroupInput(
      "projects_page_details_project_fields_checkboxes",
        "Fields:", project_fields %>% unlist(use.names = FALSE),
      selected=p_project_fields
    )
  })

  output$projects_page_details_researcher_name_output <- renderUI({
    # textInput("projects_page_details_researcher_name", "Researcher name", p_researchers_name)
    textInput("projects_page_details_researcher_name", "Researcher name", input$projects_page_graph_selection)
  })

  output$projects_page_graph_selection_output_23 <- renderText({
    input$projects_page_graph_selection
  })

  df_filtered_for_project_details_stacked_bar_chart <- reactive({
    df_for_project_details_stacked_bar_chart %>%
      filter(researcher_name == input$projects_page_details_researcher_name) %>%
      filter(project_field %in% input$projects_page_details_project_fields_checkboxes) %>%
      arrange(desc(sort_digit))
  })

  output$projects_page_details_stacked_bar_chart_output <- renderPlot({
    ggplot(df_filtered_for_project_details_stacked_bar_chart(), aes(fill=project_field, y=sort_digit, x=fct_reorder(company_name, desc(sort_digit)))) +
      geom_bar(position="stack", stat="identity", width=0.8) +
      xlab("Company") + ylab("Projects count") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme(text=element_text(size=16)) +
      ggtitle("Researcher's projects in different companies and fields")
  })

  output$projects_page_details_plot_output <- renderPlot({
    ggplot(data = penguins, aes(body_mass_g)) +
      geom_histogram(bins = input$projects_page_details_slider)
  })

  df_researcher_details <- data.frame(
    "Name" = df_researchers_and_groups$name.x,
    "Research Group" = df_researchers_and_groups$name.y,
    check.names = FALSE
  )

  output$projects_page_details_stacked_bar_chart_table_output <- renderTable({
    df_researcher_details %>% filter(Name == input$projects_page_details_researcher_name)
  })

  output$projects_page_details_stacked_bar_chart_df_output <- renderDataTable({ df_filtered_for_project_details_stacked_bar_chart() }, filter='top')
}