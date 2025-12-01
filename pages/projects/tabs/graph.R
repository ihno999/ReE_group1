### Parameters
p_graph_type <- "Researcher"
p_graph_selection <- "All researchers"
p_graph_project_fields <- c("Circular Economy", "Digital Education", "BioTech", "Cybersecurity")

### UI
ui_graph_projects_page <- sidebarLayout(
  sidebarPanel(
    uiOutput("projects_page_graph_type_output"),
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
        style = "display: flex; flex-direction: row; flex-wrap: wrap; gap: 15px; padding: 15px; font-size: 12px; align-items: center;",
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
        # Companies: Funding Companies
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:14px; height:14px; background:#F77777; border-radius:50%; margin-right:6px;"),
          "Funding Companies"
        ),
        # Companies: Steering Committee
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:14px; height:14px; background:#F44336; border-radius:50%; margin-right:6px;"),
          "Steering Committee"
        ),
        # Companies: Participation Companies
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:14px; height:14px; background:#B83027; border-radius:50%; margin-right:6px;"),
          "Participating Companies"
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
# server now accepts session and a shared reactiveValues rv
server_graph_projects_page <- function(input, output, session, rv) {
  # --- Dynamic type selection ---
  output$projects_page_graph_type_output <- renderUI({
    selectInput(
      "projects_page_graph_type",
      "Select type:",
      choices = c("Researcher", "Company"),
      selected = rv$type
    )
  })

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
        selected = if (!is.null(rv$selection) && rv$selection %in% choices) rv$selection else if (p_graph_selection %in% choices) p_graph_selection else choices[1]
      )
    } else {
      companies <- df_for_project_graph_network %>%
        pull(company_name) %>%
        unique() %>%
        na.omit() %>%
        sort()
      selectInput(
        "projects_page_graph_selection",
        "Select Company:",
        choices = companies,
        selected = if (!is.null(rv$selection) && rv$selection %in% companies) rv$selection else if (p_graph_selection %in% companies) p_graph_selection else companies[1]
      )
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

    checkboxGroupInput(
      "projects_page_graph_project_fields_checkboxes",
      "Fields:",
      choices = all_fields,
      selected = if (!is.null(rv$fields)) intersect(rv$fields, all_fields) else selected_fields
    )
  })

  # --- keep graph inputs -> shared state ---
  observeEvent(input$projects_page_graph_selection, {
    rv$selection <- input$projects_page_graph_selection
  }, ignoreInit = TRUE)

  observeEvent(input$projects_page_graph_project_fields_checkboxes, {
    rv$fields <- input$projects_page_graph_project_fields_checkboxes
  }, ignoreInit = TRUE)

  # --- react to shared state -> graph inputs (keep UI synced when details change) ---
  observe({
    # update selection input if rv changed
    if (!is.null(rv$selection) && !identical(rv$selection, input$projects_page_graph_selection)) {
      try(updateSelectInput(session, "projects_page_graph_selection", selected = rv$selection), silent = TRUE)
    }
    # update fields checkboxes if rv changed
    if (!is.null(rv$fields) && !identical(rv$fields, input$projects_page_graph_project_fields_checkboxes)) {
      try(updateCheckboxGroupInput(session, "projects_page_graph_project_fields_checkboxes", selected = rv$fields), silent = TRUE)
    }
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

      # Node information
      node_information_table <- NULL

      # Researcher node information.
      researcher_pattern <- "^researcher_"
      if (grepl(researcher_pattern, node_id)) {
        researcher_id <- as.integer(gsub(researcher_pattern, "", node_id))
        node_information_table <- df_researchers_and_groups %>% filter(employee_id == researcher_id)
      }

      # Company node informatinon.
      company_pattern <- "^company_"
      if (grepl(company_pattern, node_id)) {
        company_id_s <- as.integer(gsub(company_pattern, "", node_id))
        node_information_table <- company_data %>% filter(company_id == company_id_s)
      }

      # Project node infromation.
      project_pattern <- "^project_"
      if (grepl(project_pattern, node_id)) {
        project_id_s <- as.integer(gsub(project_pattern, "", node_id))
        node_information_table <- projects_data %>% filter(project_id == project_id_s)

        # Hide long description under a button.
        if("description" %in% colnames(node_information_table)) {
          node_information_table$description <- sapply(node_information_table$description, function(text) {
            paste0(
              '<details><summary>Show</summary><p style="width: 500px">',
              text,
              '</p></details>'
            )
          })
        }
      }

      node_information_table
    },
    options = list(dom = "t", pageLength = 1),
    escape = FALSE
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
    
    # Check if we have valid data
    if (nrow(graph_data$nodes) == 0 || nrow(graph_data$edges) == 0) {
      return(visNetwork(data.frame(), data.frame()) %>% 
              visNodes(shadow = TRUE) %>%
              visEdges(smooth = FALSE) %>%
              visOptions(highlightNearest = TRUE))
    }
    
    nodes <- graph_data$nodes %>% 
      mutate(
        label = name, 
        title = name,
        # Ensure IDs are character
        id = as.character(id)
      )
    
    edges <- graph_data$edges %>% 
      mutate(
        width = 2, 
        color = list(color = "gray"),
        # Ensure IDs are character
        from = as.character(from),
        to = as.character(to)
      )
    
    # Create the network
    network <- visNetwork(nodes, edges, height = "600px") %>%
      visNodes(shadow = TRUE, borderWidth = 1) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE) %>%
      visEvents(select = "function(nodes) { Shiny.setInputValue('projects_page_graph_network_output_selected', nodes); }") %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE)
    
    # Add group styling only if groups exist
    if ("Selected Researcher" %in% nodes$group) {
      network <- network %>% visGroups(groupname = "Selected Researcher", color = list(background = "#4CAF50", border = "#388E3C"))
    }
    if ("Other Researcher" %in% nodes$group) {
      network <- network %>% visGroups(groupname = "Other Researcher", color = list(background = "#FFC107", border = "#FFA000"))
    }
    if ("Project" %in% nodes$group) {
      network <- network %>% visGroups(groupname = "Project", color = list(background = "#2196F3", border = "#1976D2"))
    }
    if ("Funding Company" %in% nodes$group) {
      network <- network %>% visGroups(groupname = "Funding Company", color = list(background = "#F77777", border = "#e64d4dff"))
    }
    if ("Steering Committee Company" %in% nodes$group) {
      network <- network %>% visGroups(groupname = "Steering Committee Company", color = list(background = "#F44336", border = "#D32F2F"))
    }
    if ("Participating Company" %in% nodes$group) {
      network <- network %>% visGroups(groupname = "Participating Company", color = list(background = "#B83027", border = "#a3251c"))
    }
    
    network
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