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
    uiOutput("field_select_ui"),
    hr(),
    h4("Node Information"),
    tableOutput("node_info")
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

  # =============================================================
  # Build visNetwork graph dynamically based on selection type
  # =============================================================
  output$network_plot <- renderVisNetwork({
    req(related_data(), input$entity_name)

    rd <- related_data()

    # --- Create base nodes ---
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

    # --- Create edges (researcher-project, company-project) ---
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

    # =============================================================
    # Identify selected entity and build contextual subgraph
    # =============================================================

    if (input$entity_type == "Researcher") {
      selected_node_id <- researcher_nodes %>%
        dplyr::filter(label == input$entity_name) %>%
        dplyr::pull(id)
      req(selected_node_id)

      # Step 1: projects this researcher is part of
      researcher_projects <- edges %>%
        dplyr::filter(from == selected_node_id | to == selected_node_id) %>%
        dplyr::mutate(project_id = ifelse(grepl("^P_", from), from, to)) %>%
        dplyr::pull(project_id) %>%
        unique()

      # Step 2: companies linked to those projects
      connected_companies <- edges %>%
        dplyr::filter(to %in% researcher_projects & grepl("^C_", from)) %>%
        dplyr::pull(from) %>%
        unique()

      # Step 3: researchers linked to those companies (via shared projects)
      related_projects <- edges %>%
        dplyr::filter(from %in% connected_companies | to %in% connected_companies) %>%
        dplyr::mutate(node = ifelse(grepl("^R_", from), from,
          ifelse(grepl("^R_", to), to, NA)
        )) %>%
        dplyr::pull(node) %>%
        unique()

      # collect all node IDs to keep
      neighborhood_node_ids <- unique(c(
        selected_node_id, researcher_projects,
        connected_companies, related_projects
      ))
    } else {
      selected_node_id <- company_nodes %>%
        dplyr::filter(label == input$entity_name) %>%
        dplyr::pull(id)
      req(selected_node_id)

      # Step 1: projects this company is part of
      company_projects <- edges %>%
        dplyr::filter(from == selected_node_id | to == selected_node_id) %>%
        dplyr::mutate(project_id = ifelse(grepl("^P_", from), from, to)) %>%
        dplyr::pull(project_id) %>%
        unique()

      # Step 2: researchers linked to those projects
      connected_researchers <- edges %>%
        dplyr::filter(to %in% company_projects & grepl("^R_", from)) %>%
        dplyr::pull(from) %>%
        unique()

      # Step 3: companies linked to those researchers (via shared projects)
      related_projects <- edges %>%
        dplyr::filter(from %in% connected_researchers | to %in% connected_researchers) %>%
        dplyr::mutate(node = ifelse(grepl("^C_", from), from,
          ifelse(grepl("^C_", to), to, NA)
        )) %>%
        dplyr::pull(node) %>%
        unique()

      # collect all node IDs to keep
      neighborhood_node_ids <- unique(c(
        selected_node_id, company_projects,
        connected_researchers, related_projects
      ))
    }

    filtered_nodes <- nodes %>%
      dplyr::filter(id %in% neighborhood_node_ids)

    filtered_edges <- edges %>%
      dplyr::filter(from %in% neighborhood_node_ids & to %in% neighborhood_node_ids)

    # =============================================================
    # Render network
    # =============================================================
    visNetwork(filtered_nodes, filtered_edges) %>%
      visNodes(shape = "dot", size = 20) %>%
      visEdges(smooth = FALSE, arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visPhysics(stabilization = TRUE) %>%
      visEvents(
        stabilized = sprintf(
          "function() { this.focus('%s', {scale: 2.2, animation: true}); }",
          selected_node_id
        ),
        selectNode = "function(params) {
          Shiny.onInputChange('selected_node_id', params.nodes[0]);
        }"
      )
  })

  # =============================================================
  # Node information display (table format)
  # =============================================================
  output$node_info <- renderTable({
    req(input$selected_node_id)
    rd <- related_data()
    node_id <- input$selected_node_id
    type_prefix <- substr(node_id, 1, 1)

    if (type_prefix == "R") {
      researcher_id <- sub("^R_", "", node_id)
      rd$researchers %>% dplyr::filter(researcher_id == !!researcher_id)
    } else if (type_prefix == "C") {
      company_id <- sub("^C_", "", node_id)
      rd$companies %>% dplyr::filter(company_id == !!company_id)
    } else {
      project_id <- sub("^P_", "", node_id)
      rd$projects %>% dplyr::filter(project_id == !!project_id)
    }
  })
}
