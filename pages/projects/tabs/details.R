### UI
vars <- setdiff(names(iris), "Species")

ui_details_projects_page <- sidebarLayout(
  sidebarPanel(
    sliderInput(
      "projects_page_details_slider",
      label = "Number of bins",
      min = 10,
      max = 60,
      value = 20
    ),
    textInput("projects_page_details_researcher_name", "Researcher name", "Test"),
    checkboxGroupInput("projects_page_details_fields", "Fields:",
                       c("AI" = "ai",
                         "Software development" = "sd")),
    selectInput('projects_page_xcol', 'X Variable', vars),
    selectInput('projects_page_ycol', 'Y Variable', vars, selected = vars[[2]]),
    numericInput('projects_page_clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  # mainPanel(plotOutput('projects_page_plot1'))
  # mainPanel(verbatimTextOutput("projects_page_details_researcher_name_output"))
  # mainPanel(verbatimTextOutput("projects_page_details_fields_output"))
  mainPanel(
    card(verbatimTextOutput("projects_page_details_researcher_name_output")),
    card(verbatimTextOutput("projects_page_details_fields_output")),
    card(tableOutput("projects_page_details_tableDT")),
    card(tableOutput('projects_page_details_table')),
    card(plotOutput('projects_page_details_plot_output')),
    card(plotOutput('projects_page_plot1'))
  )
)


### Server
server_details_projects_page <- function(input, output) {
  output$projects_page_details_plot_output <- renderPlot(
  {
    ggplot(data = penguins, aes(body_mass_g)) +
      geom_histogram(bins = input$projects_page_details_slider)
  }
  )

  output$projects_page_details_researcher_name_output <- renderText({ input$projects_page_details_researcher_name })
    output$projects_page_details_fields_output <- renderText({ input$projects_page_details_fields })
    test_db <- researchers_data
    rval_filtered_researchers <- reactive({
      test_db %>%
        filter(employee_id == 1001)
    })
  output$projects_page_details_tableDT <- renderTable({
    rval_filtered_researchers()
  })
    output$projects_page_details_table <- renderTable(researchers_data)
    # output$projects_page_details_table <- renderTable(filter(researchers_data, employee_id == 1))

    selectedData <- reactive({
      iris[, c(input$projects_page_xcol, input$projects_page_ycol)]
    })

    clusters <- reactive({
      kmeans(selectedData(), input$projects_page_clusters)
    })

    output$projects_page_plot1 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
}