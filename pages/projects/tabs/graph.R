### Parameters
p_graph_type <- "Researcher"
p_graph_selection <- "All researchers"
p_graph_project_fields <- c("Circular Economy", "Digital Education", "BioTech", "Cybersecurity")

### UI
ui_graph_projects_page <- sidebarLayout(
  sidebarPanel(
    selectInput(
      "projects_page_graph_type",
      "Select type:",
      choices = c("Researcher", "Company"),
      selected = p_graph_type
    ),
    uiOutput("projects_page_graph_selection_output"),
    uiOutput("projects_page_graph_project_fields_checkboxes_output"),
    width = 2
  ),
  mainPanel(
    # --- GRAPH ---
    card(
      visNetworkOutput("projects_page_graph_network_output", height = "600px"),
      full_screen = TRUE
    ),

    # --- LEGEND BELOW GRAPH ---
    card(
      div(
        style = "display: flex; flex-direction: row; gap: 25px; padding: 15px; font-size: 14px; align-items: center;",
        # Selected Researcher / Company
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:14px; height:14px; background:#4CAF50; border-radius:50%; margin-right:6px;"),
          "Selected Researcher / Company"
        ),
        # Other Researchers
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:14px; height:14px; background:#FFC107; border-radius:50%; margin-right:6px;"),
          "Other Researchers"
        ),
        # Projects
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:14px; height:14px; background:#2196F3; border-radius:50%; margin-right:6px;"),
          "Projects"
        ),
        # Companies
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:14px; height:14px; background:#F44336; border-radius:50%; margin-right:6px;"),
          "Companies"
        )
      )
    ),

    # --- NODE DETAILS ---
    card(
      h4("Node Information"),
      div(
        dataTableOutput("projects_page_graph_node_info_output"),
        style = "font-size:90%"
      )
    )
  )
)

### SERVER
server_graph_projects_page <- function(input, output, session) {
  # --- Dynamic researcher/company selection ---
  output$projects_page_graph_selection_output <- renderUI({
    req(input$projects_page_graph_type)

    if (input$projects_page_graph_type == "Researcher") {
      researchers <- df_for_project_graph_network %>%
        pull(researcher_name) %>%
        unique() %>%
        na.omit() %>%
        sort()
      # include "All researchers" option
      choices <- c("All researchers", researchers)
      selectInput(
        "projects_page_graph_selection",
        "Select Researcher:",
        choices = choices,
        selected = if (p_graph_selection %in% choices) p_graph_selection else choices[1]
      )
    } else {
      companies <- df_for_project_graph_network %>%
        pull(company_name) %>%
        unique() %>%
        na.omit() %>%
        sort()
      selectInput("projects_page_graph_selection", "Select Company:", choices = companies, selected = if (p_graph_selection %in% companies) p_graph_selection else companies[1])
    }
  })

  # --- Checkboxes for project fields (all fields always available) ---
  output$projects_page_graph_project_fields_checkboxes_output <- renderUI({
    validate(need("project_field" %in% names(df_for_project_graph_network), "Error: project_field column missing."))

    all_fields <- df_for_project_graph_network %>%
      pull(project_field) %>%
      unique() %>%
      na.omit() %>%
      sort()
    selected_fields <- intersect(p_graph_project_fields, all_fields)
    if (length(selected_fields) == 0) selected_fields <- all_fields

    checkboxGroupInput("projects_page_graph_project_fields_checkboxes", "Fields:", choices = all_fields, selected = selected_fields)
  })

  # --- Filter dataset ---
  df_filtered_for_graph <- reactive({
    req(input$projects_page_graph_type, input$projects_page_graph_selection, input$projects_page_graph_project_fields_checkboxes)

    if (input$projects_page_graph_type == "Researcher") {
      # If "All researchers" selected -> include all projects in selected fields
      if (!is.null(input$projects_page_graph_selection) && input$projects_page_graph_selection == "All researchers") {
        projects <- df_for_project_graph_network %>%
          filter(project_field %in% input$projects_page_graph_project_fields_checkboxes) %>%
          pull(project_id) %>%
          unique()
      } else {
        # Projects of selected researcher in selected fields
        projects <- df_for_project_graph_network %>%
          filter(
            researcher_name == input$projects_page_graph_selection,
            project_field %in% input$projects_page_graph_project_fields_checkboxes
          ) %>%
          pull(project_id)
      }

      # All rows linked to these projects
      df_for_project_graph_network %>%
        filter(
          project_id %in% projects,
          project_field %in% input$projects_page_graph_project_fields_checkboxes
        ) %>%
        distinct(.keep_all = TRUE)
    } else {
      # Projects of selected company in selected fields
      projects <- df_for_project_graph_network %>%
        filter(
          company_name == input$projects_page_graph_selection,
          project_field %in% input$projects_page_graph_project_fields_checkboxes
        ) %>%
        pull(project_id)

      # All rows linked to these projects
      df_for_project_graph_network %>%
        filter(
          project_id %in% projects,
          project_field %in% input$projects_page_graph_project_fields_checkboxes
        )
    }
  })

  # --- Node info table (click node in graph) ---
  output$projects_page_graph_node_info_output <- renderDataTable(
    {
      sel <- input$projects_page_graph_network_output_selected
      if (is.null(sel) || is.null(sel$nodes) || length(sel$nodes) == 0) {
        return(data.frame(Message = "Click a node in the graph"))
      }
      node_id <- sel$nodes[[1]]

      selected_name_for_graph <- if (!is.null(input$projects_page_graph_selection) &&
        input$projects_page_graph_selection == "All researchers" &&
        input$projects_page_graph_type == "Researcher") {
        NULL
      } else {
        input$projects_page_graph_selection
      }

      graph_data <- prepare_network_graph_data(df_filtered_for_graph(), input$projects_page_graph_type, selected_name_for_graph)
      node <- graph_data$nodes %>% filter(id == node_id)
      if (nrow(node) == 0) {
        return(data.frame(Message = "No data for selected node"))
      }

      node %>% select(ID = id, Name = name, Type = type)
    },
    options = list(dom = "t", pageLength = 1)
  )

  # --- Graph (visNetwork) ---
  output$projects_page_graph_network_output <- renderVisNetwork({
    req(nrow(df_filtered_for_graph()) > 0)

    selected_name_for_graph <- if (!is.null(input$projects_page_graph_selection) &&
      input$projects_page_graph_selection == "All researchers" &&
      input$projects_page_graph_type == "Researcher") {
      NULL
    } else {
      input$projects_page_graph_selection
    }

    graph_data <- prepare_network_graph_data(df_filtered_for_graph(), input$projects_page_graph_type, selected_name_for_graph)
    nodes <- graph_data$nodes %>% mutate(label = name, title = name)
    edges <- graph_data$edges %>% mutate(width = 2, color = list(color = "gray"))

    visNetwork(nodes, edges, height = "600px") %>%
      visNodes(shadow = TRUE, borderWidth = 1) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE) %>%
      visEvents(select = "function(nodes) { Shiny.setInputValue('projects_page_graph_network_output_selected', nodes); }") %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE)
  })

  # --- Project details table ---
  rval_projects_page_graph_network_df_output <- reactive({
    df_filtered_for_graph() %>%
      arrange(project_field, project_name)
  })


  projects_page_graph_network_df_output <- renderDataTable(
    rval_projects_page_graph_network_df_output(),
    filter = "top"
  )
  output$projects_page_graph_network_df_output <- projects_page_graph_network_df_output


  rval_projects_page_graph_network_df_output_2 <- reactive({
    data <- rval_projects_page_graph_network_df_output()
    # If "All researchers" selected, don't filter by researcher name
    if (!is.null(input$projects_page_graph_selection) && input$projects_page_graph_selection == "All researchers") {
      filtered <- data
    } else {
      filtered <- data %>% filter(researcher_name == input$projects_page_graph_selection)
    }

    filtered %>%
      cbind(sum_digit = 1) %>%
      group_by(company_name) %>%
      mutate(sort_digit = n()) %>%
      ungroup()
  })

  projects_page_graph_network_df_output_2 <- renderDataTable(
    rval_projects_page_graph_network_df_output_2(),
    filter = "top"
  )

  output$projects_page_graph_network_df_output_2 <- projects_page_graph_network_df_output_2


  output$projects_page_details_stacked_bar_chart_output <- renderPlot({
    ggplot(rval_projects_page_graph_network_df_output_2(), aes(fill = project_field, y = sum_digit, x = reorder(company_name, -sort_digit))) +
      geom_bar(position = "stack", stat = "identity", width = 0.8) +
      xlab("Company") +
      ylab("Projects count") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme(text = element_text(size = 16)) +
      ggtitle("Researcher's projects in different companies and fields")
  })
}
