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
    card(tableOutput('duckdb_results')),
    card(plotOutput('projects_page_details_stacked_bar_chart_output')),
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
  # create a dataset
  specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
  condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
  value <- abs(rnorm(12 , 0 , 15))
  data <- data.frame(specie,condition,value)

  data_2 <- data.frame(
    "projects" = c("A","B","C","D","E"),
    "companies" = c(rep("Worldline", 3), rep("Proximus", 2)),
    "fields" = c("AI", "AI", "Software development", "AI", "AI"),
    "count_projects" = c(rep(1, 5))
  )

  query <- r'(
        -- get_project_field
		WITH project_fields AS (
			SELECT
			    p.project_id project_id,
				p.name project_name,
				p.description project_description,
				r.name researcher_name,
				rg.name group_name,
				COUNT(*) OVER (PARTITION BY rg.name) sort_digit
			FROM projects p
				LEFT JOIN research_participation rp
					ON (p.project_id = rp.project_id)
				LEFT JOIN researchers r
					ON (rp.researcher_id = r.employee_id)
				LEFT JOIN research_groups rg
					ON (r.main_research_group = rg.group_id)
			-- WHERE
				-- p.project_id = 4001
			ORDER BY sort_digit DESC
		)
		SELECT *
		FROM project_fields;
    )'

  results <- dbGetQuery(con, query)

  output$duckdb_results <- renderTable({
    results %>%
      filter(project_id == 4003)
  })

  # Grouped
  output$projects_page_details_stacked_bar_chart_output <- renderPlot({
    # ggplot(data, aes(fill=condition, y=value, x=specie)) +
    #   geom_bar(position="stack", stat="identity")
    ggplot(data_2, aes(fill=fields, y=count_projects, x=companies)) +
      geom_bar(position="stack", stat="identity")
  })

  output$projects_page_details_plot_output <- renderPlot(
  {
    ggplot(data = penguins, aes(body_mass_g)) +
      geom_histogram(bins = input$projects_page_details_slider)
  }
  )

  output$projects_page_details_researcher_name_output <- renderText({ input$projects_page_details_researcher_name })
    output$projects_page_details_fields_output <- renderText({ input$projects_page_details_fields })

    df_researcher_details <- data.frame(
      "Name" = df_researchers_and_groups$name.x,
      "Research Group" = df_researchers_and_groups$name.y,
      check.names = FALSE
    )


  output$projects_page_details_tableDT <- renderTable({
    # df_researcher_details()
    # merge(df_filtered_researchers, research_groups_data, by.x="main_research_group", by.y="group_id")
    df_researcher_details %>% filter(Name == input$projects_page_details_researcher_name)
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