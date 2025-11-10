source("./global.R")


# =============================================================
# UI for the Graph Tab
# =============================================================
ui_graph_projects_page <- sidebarLayout(
  sidebarPanel(
    selectInput(
      "entity_type",
      "Select type:",
      choices = c("Researcher", "Company")
    ),
    uiOutput("entity_select_ui"),
    uiOutput("field_select_ui")
  ),
  mainPanel(
    visNetworkOutput("network_plot", height = "600px")
  )
)

# =============================================================
# Server for the Graph Tab
# =============================================================
server_graph_projects_page <- function(input, output, session) {
  # --- Dynamic dropdown for researcher or company ---
  output$entity_select_ui <- renderUI({
    if (input$entity_type == "Researcher") {
      selectInput(
        "entity_name",
        "Select Researcher:",
        choices = unique(researchers_data$researcher_name)
      )
    } else {
      selectInput(
        "entity_name",
        "Select Company:",
        choices = unique(company_data$company_name)
      )
    }
  })

  # --- Field (project type) selection ---
  output$field_select_ui <- renderUI({
    selectInput(
      "selected_field",
      "Field of new project:",
      choices = unique(projects_data$type)
    )
  })

  # --- Reactive: Filter related projects based on field ---
  related_data <- reactive({
    req(input$selected_field)
    filtered_projects <- filter_projects_by_field(projects_data, input$selected_field)
    get_related_collaboration_data(filtered_projects)
  })

  # --- Build visNetwork graph ---
  output$network_plot <- renderVisNetwork({
    req(related_data(), input$entity_name)

    rd <- related_data()

    # --- Create nodes with unique IDs ---
    project_nodes <- data.frame(
      id = paste0("P_", rd$projects$project_id),
      label = rd$projects$type,
      group = "Project"
    )

    researcher_nodes <- data.frame(
      id = paste0("R_", rd$researchers$researcher_id),
      label = rd$researchers$researcher_name,
      group = "Researcher"
    )

    company_nodes <- data.frame(
      id = paste0("C_", rd$companies$company_id),
      label = rd$companies$company_name,
      group = "Company"
    )

    nodes <- dplyr::bind_rows(project_nodes, researcher_nodes, company_nodes) %>%
      dplyr::distinct(id, .keep_all = TRUE)

    # --- Create edges using prefixed IDs ---
    researcher_edges <- rd$researchers %>%
      dplyr::select(project_id, researcher_id) %>%
      dplyr::mutate(
        from = paste0("R_", researcher_id),
        to = paste0("P_", project_id)
      ) %>%
      dplyr::select(from, to)

    company_edges <- rd$companies %>%
      dplyr::select(project_id, company_id) %>%
      dplyr::mutate(
        from = paste0("C_", company_id),
        to = paste0("P_", project_id)
      ) %>%
      dplyr::select(from, to)

    edges <- dplyr::bind_rows(researcher_edges, company_edges)

    # --- Determine selected node ID ---
    selected_node_id <- if (input$entity_type == "Researcher") {
      node <- researcher_nodes %>%
        dplyr::filter(label == input$entity_name) %>%
        dplyr::pull(id)
      if (length(node) > 0) node else NULL
    } else {
      node <- company_nodes %>%
        dplyr::filter(label == input$entity_name) %>%
        dplyr::pull(id)
      if (length(node) > 0) node else NULL
    }

    # --- Render network ---
    visNetwork(nodes, edges) %>%
      visNodes(shape = "dot", size = 15) %>%
      visEdges(smooth = FALSE, arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>% # disable internal "Select by ID"
      visPhysics(stabilization = TRUE) %>%
      # Focus automatically on selected researcher or company
      visEvents(
        stabilized = sprintf(
          "function() { this.focus('%s', {scale: 1.5, animation: true}); }",
          selected_node_id
        )
      )
  })
}
