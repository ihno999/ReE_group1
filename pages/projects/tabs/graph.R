### Parameters
p_entity_type <- "Researcher"

### UI
ui_graph_projects_page <- sidebarLayout(
  sidebarPanel(
    selectInput(
      "projects_page_graph_entity_type",
      "Select type:",
      choices = c("Researcher", "Company"),
      selected = p_entity_type
    ),
    uiOutput("projects_page_graph_entity_name_ui"),
    uiOutput("projects_page_graph_field_ui"),
    hr(),
    h4("Node Information"),
    tableOutput("projects_page_graph_node_info_output"),
    width = 3
  ),
  mainPanel(
    card(
      visNetworkOutput("projects_page_graph_network_output", height = "650px"),
      full_screen = TRUE
    )
  )
)

### SERVER
server_graph_projects_page <- function(input, output, session) {
  # ---- Dynamic UI: Researcher / Company names ----
  output$projects_page_graph_entity_name_ui <- renderUI({
    req(input$projects_page_graph_entity_type)

    if (input$projects_page_graph_entity_type == "Researcher") {
      selectInput(
        "projects_page_graph_entity_name",
        "Select Researcher:",
        choices = sort(unique(df_for_project_graph_network$researcher_name)),
        selected = sort(unique(df_for_project_graph_network$researcher_name))[1]
      )
    } else {
      selectInput(
        "projects_page_graph_entity_name",
        "Select Company:",
        choices = sort(unique(df_for_project_graph_network$company_name)),
        selected = sort(unique(df_for_project_graph_network$company_name))[1]
      )
    }
  })

  output$projects_page_graph_field_ui <- renderUI({
    selectInput(
      "projects_page_graph_field",
      "Field of new project:",
      choices = sort(unique(df_for_project_graph_network$project_type)),
      selected = sort(unique(df_for_project_graph_network$project_type))[1]
    )
  })

  # ============================================================================
  # === REACTIVE: LOCALIZED EGO NETWORK DATASET ================================
  # ============================================================================

  df_related_graph_data <- reactive({
    req(
      input$projects_page_graph_entity_name,
      input$projects_page_graph_entity_type,
      input$projects_page_graph_field
    )

    # Base filtered by project field
    rd <- df_for_project_graph_network %>%
      dplyr::filter(project_type == input$projects_page_graph_field)

    if (input$projects_page_graph_entity_type == "Researcher") {
      center <- input$projects_page_graph_entity_name

      # --- Level 0: Researcher ---
      L0_researcher <- center

      # --- Level 1: Companies connected to center researcher ---
      L1_companies <- rd %>%
        dplyr::filter(researcher_name == center) %>%
        dplyr::pull(company_name) %>%
        unique()

      # --- Level 2: Researchers who worked with these companies ---
      L2_researchers <- rd %>%
        dplyr::filter(company_name %in% L1_companies) %>%
        dplyr::pull(researcher_name) %>%
        unique()

      # Keep only relevant rows:
      rd %>%
        dplyr::filter(
          researcher_name %in% c(L0_researcher, L2_researchers) &
            company_name %in% L1_companies
        )
    } else {
      center <- input$projects_page_graph_entity_name

      # --- Level 0: Company ---
      L0_company <- center

      # --- Level 1: Researchers of this company ---
      L1_researchers <- rd %>%
        dplyr::filter(company_name == center) %>%
        dplyr::pull(researcher_name) %>%
        unique()

      # --- Level 2: Companies that those researchers worked with ---
      L2_companies <- rd %>%
        dplyr::filter(researcher_name %in% L1_researchers) %>%
        dplyr::pull(company_name) %>%
        unique()

      rd %>%
        dplyr::filter(
          company_name %in% c(L0_company, L2_companies) &
            researcher_name %in% L1_researchers
        )
    }
  })


  # ============================================================================
  # === NETWORK RENDERING ======================================================
  # ============================================================================

  output$projects_page_graph_network_output <- renderVisNetwork({
    req(df_related_graph_data(), input$projects_page_graph_entity_name)
    rd <- df_related_graph_data()

    # -------- Nodes --------
    project_nodes <- rd %>%
      dplyr::distinct(project_id, project_type) %>%
      dplyr::transmute(
        id = paste0("P_", project_id),
        label = project_type,
        group = "Project"
      )

    researcher_nodes <- rd %>%
      dplyr::distinct(researcher_id, researcher_name) %>%
      dplyr::filter(!is.na(researcher_id)) %>%
      dplyr::transmute(
        id = paste0("R_", researcher_id),
        label = researcher_name,
        group = "Researcher"
      )

    company_nodes <- rd %>%
      dplyr::distinct(company_id, company_name) %>%
      dplyr::filter(!is.na(company_id)) %>%
      dplyr::transmute(
        id = paste0("C_", company_id),
        label = company_name,
        group = "Company"
      )

    nodes <- dplyr::bind_rows(project_nodes, researcher_nodes, company_nodes)

    # -------- Edges --------
    researcher_edges <- rd %>%
      dplyr::filter(!is.na(researcher_id)) %>%
      dplyr::transmute(
        from = paste0("R_", researcher_id),
        to = paste0("P_", project_id)
      )

    company_edges <- rd %>%
      dplyr::filter(!is.na(company_id)) %>%
      dplyr::transmute(
        from = paste0("C_", company_id),
        to = paste0("P_", project_id)
      )

    edges <- dplyr::bind_rows(researcher_edges, company_edges)

    # -------- Determine center node ID --------
    if (input$projects_page_graph_entity_type == "Researcher") {
      selected_id <- researcher_nodes %>%
        dplyr::filter(label == input$projects_page_graph_entity_name) %>%
        dplyr::pull(id)
    } else {
      selected_id <- company_nodes %>%
        dplyr::filter(label == input$projects_page_graph_entity_name) %>%
        dplyr::pull(id)
    }

    # -------- Build the network --------
    visNetwork(nodes, edges) %>%
      visNodes(shape = "dot", size = 20) %>%
      visEdges(smooth = FALSE, arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visPhysics(stabilization = TRUE) %>%
      visEvents(
        stabilized = sprintf(
          "function() { this.focus('%s', {scale: 2.2, animation: true}); }",
          selected_id
        ),
        selectNode = "function(params) {
          Shiny.onInputChange('projects_page_graph_selected_node_id', params.nodes[0]);
        }"
      )
  })


  # ============================================================================
  # === NODE INFORMATION TABLE =================================================
  # ============================================================================

  output$projects_page_graph_node_info_output <- renderTable({
    req(input$projects_page_graph_selected_node_id)
    node_id <- input$projects_page_graph_selected_node_id
    type_prefix <- substr(node_id, 1, 1)

    rd <- df_related_graph_data()

    if (type_prefix == "R") {
      rid <- sub("^R_", "", node_id)
      rd %>%
        dplyr::filter(researcher_id == rid) %>%
        dplyr::select(researcher_name, project_name, company_name)
    } else if (type_prefix == "C") {
      cid <- sub("^C_", "", node_id)
      rd %>%
        dplyr::filter(company_id == cid) %>%
        dplyr::select(company_name, project_name, researcher_name)
    } else {
      pid <- sub("^P_", "", node_id)
      rd %>%
        dplyr::filter(project_id == pid) %>%
        dplyr::select(project_name, project_type, researcher_name, company_name)
    }
  })
}
