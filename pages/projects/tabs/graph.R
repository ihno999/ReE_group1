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

        # Companies
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:14px; height:14px; background:#E53935; border-radius:50%; margin-right:6px;"),
          "Companies"
        ),
        # Participating
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:40px; height:0; border-top:2px solid #888888; margin-right:6px;"),
          "Participating"
        ),

        # Steering Committee
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:40px; height:0; border-top:3px solid #888888; margin-right:6px;"),
          "Steering Committee"
        ),

        # Funding
        div(
          style = "display:flex; align-items:center;",
          div(style = "width:40px; height:0; border-top:6px solid #888888; margin-right:6px;"),
          "Funding"
        ),
      )
    ),

    # --- NODE DETAILS ---
    card(
      h4("Node Information"),
      div(
        dataTableOutput("projects_page_graph_node_info_output"),
        style = "font-size:90%"
      )
    ),

    # --- NODE DETAILS RELATED PROJECTS ---
    card(
      h4("Connected Projects"),
      div(
        dataTableOutput("projects_page_graph_node_info_related_projects_output"),
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
      "Type",
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
        "Researcher",
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
      "Project fields",
      choices = all_fields,
      selected = if (!is.null(rv$fields)) intersect(rv$fields, all_fields) else selected_fields
    )
  })

  # --- keep graph inputs -> shared state ---
  observeEvent(input$projects_page_graph_selection,
    {
      rv$selection <- input$projects_page_graph_selection
      rv$selected_node_researcher_name <- input$projects_page_graph_selection
      rv$selected_node_company_name <- input$projects_page_graph_selection
    },
    ignoreInit = TRUE
  )

  observeEvent(input$projects_page_graph_project_fields_checkboxes,
    {
      rv$fields <- input$projects_page_graph_project_fields_checkboxes
    },
    ignoreInit = TRUE
  )

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
        node_information_table <- df_researchers_and_groups %>%
          filter(employee_id == researcher_id) %>%
          select(c(main_research_group_name, researcher_name))
        rv$selected_node_researcher_name <- node_information_table$researcher_name
      }

      # Company node informatinon.
      company_pattern <- "^company_"
      if (grepl(company_pattern, node_id)) {
        company_id_s <- as.integer(gsub(company_pattern, "", node_id))
        node_information_table <- company_data %>%
          filter(company_id == company_id_s) %>%
          select(!(company_id))
        rv$selected_node_company_name <- node_information_table$name
      }

      # Project node infromation.
      project_pattern <- "^project_"
      if (grepl(project_pattern, node_id)) {
        project_id_s <- as.integer(gsub(project_pattern, "", node_id))
        node_information_table <- projects_data %>% filter(project_id == project_id_s)

        # Hide long description under a button.
        if ("description" %in% colnames(node_information_table)) {
          node_information_table$description <- sapply(node_information_table$description, function(text) {
            paste0(
              '<details><summary>Show</summary><p/style="width: 500px">',
              text,
              "</p></details>"
            )
          })
        }

        # Add responsible group name.
        node_information_table$responsible_group_name <- lapply(node_information_table$responsible_group, function(id) {
          responsible_group_namee <- research_groups_data %>%
            filter(group_id == id) %>%
            select(name)
          return(as.character(responsible_group_namee))
        })

        # Add responsible employee name.
        node_information_table$responsible_employee_name <- lapply(node_information_table$responsible_employee, function(id) {
          responsible_employee_namee <- researchers_data %>%
            filter(employee_id == id) %>%
            select(name)
          return(as.character(responsible_employee_namee))
        })

        node_information_table <- node_information_table %>% select(name, description, responsible_employee_name, responsible_group_name, total_budget, funding_source, type, start_date, end_date)
      }

      node_information_table
    },
    options = list(dom = "t", pageLength = 1),
    escape = FALSE
  )

  # --- Node info table of related projects (click node in graph) ---
  output$projects_page_graph_node_info_related_projects_output <- renderDataTable(
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

      sel <- input$projects_page_graph_network_output_selected
      if (is.null(sel) || is.null(sel$nodes) || length(sel$nodes) == 0) {
        return(data.frame(Message = "Click a node in the graph"))
      }

      node_id <- sel$nodes[[1]]

      # Node information
      node_information_table_related_projects <- NULL

      # Company node related projects informatinon.
      v_node_pattern <- ""
      v_node_id <- ""

      if (grepl("company_", node_id)) {
        v_node_pattern <- "company_"
        v_node_id <- "company_id"
      }

      if (grepl("researcher_", node_id)) {
        v_node_pattern <- "researcher_"
        v_node_id <- "researcher_id"
      }

      if (v_node_pattern != "" && v_node_id != "") {
        # Get all information about connected codes.
        graph_data <- prepare_network_graph_data(df_filtered_for_graph(), input$projects_page_graph_type, selected_name_for_graph)
        df_connected_nodes <- graph_data$edges %>%
          filter(from == node_id) %>%
          rename("{v_node_id}" := "from", "connected_project_id" = "to")

        # Extract IDs.
        df_connected_nodes[v_node_id] <- lapply(df_connected_nodes[v_node_id], function(id) as.numeric(sub(v_node_pattern, "", id)))
        df_connected_nodes$connected_project_id <- lapply(df_connected_nodes$connected_project_id, function(id) as.numeric(sub("project_", "", id)))

        # Add project name.
        df_connected_nodes$connected_project_name <- lapply(df_connected_nodes$connected_project_id, function(id) {
          project_name <- projects_data %>%
            select(project_id, name) %>%
            filter(project_id == id) %>%
            select(name)
          return(as.character(project_name))
        })

        node_information_table_related_projects <- df_connected_nodes
      }

      # --- If NULL (e.g., project node selected) → return an empty table silently ---
      if (is.null(node_information_table_related_projects)) {
        return(data.frame()) # <- shows empty table, no error
      }

      rv$selected_node_connected_projects <- node_information_table_related_projects$connected_project_name

      node_information_table_related_projects %>%
        select(connected_project_name) %>%
        rename("project_name" = "connected_project_name")
    },
    options = list(dom = "t"),
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

    # Adjust node size and font if All Researchers is selected ---
    all_selected <- !is.null(input$projects_page_graph_selection) && input$projects_page_graph_selection == "All researchers"
    if (all_selected) {
      # nodes$size <- 15 # smaller nodes
      nodes$size <- 25 # smaller nodes
      # nodes$font.size <- 28 # bigger labels
      nodes$font.size <- 11 # bigger labels
    } else {
      nodes$size <- 25 # default node size
      nodes$font.size <- 11 # default label size
    }

    # Focal radial layout when a specific researcher/company is selected ----
    # Behavior:
    #  - If selected_name_for_graph is NULL -> keep the original physics-based layout (All researchers).
    #  - If a specific researcher/company is selected:
    #      * nodes directly connected to the selected node are split:
    #          - inner ring (hubs): neighbors connected to multiple projects (kept close)
    #          - outer arc (leafs): neighbors connected to only one project (placed on a rotated 180° arc)
    #      * outer nodes are assigned to hubs that share projects with them; if none, they are assigned to the center
    #      * arcs are **equally spaced across 180°** and rotated so the arc faces the center
    nodes_positions_assigned <- FALSE

    # --- IMPORTANT: only run the custom focal layout when a specific selection is active.
    # If "All researchers" is selected (selected_name_for_graph == NULL for researcher mode),
    # we skip the custom layout entirely and keep the original physics-based layout.
    if (!is.null(selected_name_for_graph)) {
      try(
        {
          nds <- nodes
          eds <- edges

          # find selected node id by name (could be researcher or company)
          sel_row <- nds %>% filter(name == selected_name_for_graph)
          if (nrow(sel_row) >= 1) {
            selected_id <- as.character(sel_row$id[1])

            # Initialize coords / physics
            nds$x <- NA_real_
            nds$y <- NA_real_
            nds$fixed <- FALSE
            nds$physics <- TRUE

            # Suggested: give it an initial position but let physics move it
            idx_sel <- which(nds$id == selected_id)
            nds$x[idx_sel] <- 0 # optional starting point
            nds$y[idx_sel] <- 0 # optional starting point
            nds$fixed[idx_sel] <- FALSE # node can move
            nds$physics[idx_sel] <- TRUE # allow physics simulation

            # Identify neighbor nodes directly connected to selected node
            neighbor_edges <- eds %>% filter(from == selected_id | to == selected_id)
            neighbor_ids <- unique(c(
              neighbor_edges %>% filter(from == selected_id) %>% pull(to),
              neighbor_edges %>% filter(to == selected_id) %>% pull(from)
            ))
            neighbor_ids <- neighbor_ids[neighbor_ids != selected_id]

            # Identify project nodes and mapping of non-project -> project connections
            project_nodes <- nds %>% filter(group == "Project")
            project_ids <- as.character(project_nodes$id)
            non_project_nodes <- nds %>% filter(group != "Project")
            non_project_ids <- as.character(non_project_nodes$id)

            # Build eds_proj_links: pairs (project, non_project)
            eds_proj_links <- eds %>%
              filter((from %in% project_ids & to %in% non_project_ids) | (to %in% project_ids & from %in% non_project_ids)) %>%
              mutate(
                project = ifelse(from %in% project_ids, from, to),
                non_project = ifelse(from %in% non_project_ids, from, to)
              ) %>%
              select(project, non_project) %>%
              distinct()

            # Count how many distinct projects each non-project node is connected to
            non_proj_project_counts <- eds_proj_links %>%
              group_by(non_project) %>%
              summarise(project_count = n_distinct(project), projects = list(unique(project)), .groups = "drop")

            # For neighbors: split into inner (project_count > 1) and outer (project_count == 1)
            neighbor_project_info <- non_proj_project_counts %>% filter(non_project %in% neighbor_ids)

            inner_neighbors <- neighbor_project_info %>%
              filter(project_count > 1) %>%
              pull(non_project)
            outer_neighbors <- neighbor_project_info %>%
              filter(project_count == 1) %>%
              pull(non_project)

            # Ensure inner/outer are valid non-project ids (exclude project nodes)
            inner_neighbors <- inner_neighbors[inner_neighbors %in% non_project_ids]
            outer_neighbors <- outer_neighbors[outer_neighbors %in% non_project_ids]

            # Place inner neighbors (hubs) on a small full circle around the center
            # Place inner neighbors (hubs) on a full circle around the center
            n_inner <- length(inner_neighbors)
            if (n_inner > 0) {
              # Order inner neighbors by number of connected projects (descending)
              inner_neighbor_counts <- neighbor_project_info %>%
                filter(non_project %in% inner_neighbors) %>%
                arrange(desc(project_count)) %>%
                pull(non_project)

              inner_radius_base <- 600 # increase to create more space between projects nodes and nodes with more connections
              angles_inner <- seq(0, 2 * pi, length.out = n_inner + 1)[-1]

              jitter_inner <- 0.05 * inner_radius_base
              for (i in seq_len(n_inner)) {
                nid <- inner_neighbor_counts[i]
                idx_n <- which(nds$id == nid)
                if (length(idx_n) == 1) {
                  nds$x[idx_n] <- (inner_radius_base + runif(1, -jitter_inner, jitter_inner)) * cos(angles_inner[i])
                  nds$y[idx_n] <- (inner_radius_base + runif(1, -jitter_inner, jitter_inner)) * sin(angles_inner[i])
                  nds$fixed[idx_n] <- TRUE
                  nds$physics[idx_n] <- FALSE
                }
              }
            }


            # For outer neighbors: assign each outer neighbor to a hub (inner neighbor) if they share a project.
            # Otherwise assign it to the center hub.
            # This creates for each hub a list of leaf nodes to place on a rotated 180° arc facing the center.
            outer_assignments <- list() # named by hub id (or "center")
            # initialize lists for each inner neighbor
            for (hid in inner_neighbors) outer_assignments[[hid]] <- character(0)
            outer_assignments[["center"]] <- character(0)

            for (oid in outer_neighbors) {
              # projects connected to this outer node
              projs_oid <- eds_proj_links %>%
                filter(non_project == oid) %>%
                pull(project) %>%
                unique()
              # find inner neighbors that share any project
              candidate_hubs <- c()
              if (length(projs_oid) > 0 && length(inner_neighbors) > 0) {
                candidate_hubs <- eds_proj_links %>%
                  filter(project %in% projs_oid, non_project %in% inner_neighbors) %>%
                  pull(non_project) %>%
                  unique()
              }
              if (length(candidate_hubs) >= 1) {
                # assign to first candidate hub (deterministic)
                outer_assignments[[candidate_hubs[1]]] <- c(outer_assignments[[candidate_hubs[1]]], oid)
              } else {
                # fallback to center
                outer_assignments[["center"]] <- c(outer_assignments[["center"]], oid)
              }
            }

            # Also include neighbor nodes that are non-project nodes but not present in non_proj_project_counts
            # Treat them as outer and attach to center
            missing_neighbors <- setdiff(neighbor_ids, c(inner_neighbors, outer_neighbors))
            if (length(missing_neighbors) > 0) {
              for (mn in missing_neighbors) {
                if (mn %in% non_project_ids) {
                  outer_assignments[["center"]] <- c(outer_assignments[["center"]], mn)
                }
              }
            }

            # Place outer nodes for each hub on a rotated 180° arc facing the center
            # arc equally spaced across 180° (Option A spacing)
            # Place outer nodes for each hub on a rotated 180° arc facing the center
            # Place outer nodes on 180° arc relative to each hub
            outer_radius_base <- 400 # increased from 210
            spacing_factor <- 25 # increase spacing for more space between nodes

            for (hid in names(outer_assignments)) {
              assigned <- outer_assignments[[hid]]
              n_assigned <- length(assigned)
              if (n_assigned == 0) next

              if (hid == "center") {
                hub_x <- nds$x[idx_sel]
                hub_y <- nds$y[idx_sel]
                angle_hub <- -pi / 2
              } else {
                idx_h <- which(nds$id == hid)
                if (length(idx_h) != 1) next
                hub_x <- nds$x[idx_h]
                hub_y <- nds$y[idx_h]
                angle_hub <- atan2(0 - hub_y, 0 - hub_x)
              }

              # Dynamically scale radius for many nodes
              ring_radius <- outer_radius_base + max(0, n_assigned - 1) * spacing_factor

              theta_min <- angle_hub - pi / 2
              theta_max <- angle_hub + pi / 2
              angles_assigned <- if (n_assigned == 1) (theta_min + theta_max) / 2 else seq(theta_min, theta_max, length.out = n_assigned + 1)[-1]

              jitter_amt <- ring_radius * 0.05
              for (j in seq_len(n_assigned)) {
                oid <- assigned[j]
                idx_o <- which(nds$id == oid)
                if (length(idx_o) == 1) {
                  nds$x[idx_o] <- hub_x + (ring_radius + runif(1, -jitter_amt, jitter_amt)) * cos(angles_assigned[j])
                  nds$y[idx_o] <- hub_y + (ring_radius + runif(1, -jitter_amt, jitter_amt)) * sin(angles_assigned[j])
                  nds$fixed[idx_o] <- TRUE
                  nds$physics[idx_o] <- FALSE
                }
              }
            }

            # For any neighbor inner nodes that were not placed (edge cases), ensure smallest fallback
            for (hid in inner_neighbors) {
              idx_h <- which(nds$id == hid)
              if (length(idx_h) == 1 && (is.na(nds$x[idx_h]) || is.na(nds$y[idx_h]))) {
                nds$x[idx_h] <- runif(1, -inner_radius, inner_radius)
                nds$y[idx_h] <- runif(1, -inner_radius, inner_radius)
                nds$fixed[idx_h] <- TRUE
                nds$physics[idx_h] <- FALSE
              }
            }

            # Transfer coordinates back to nodes
            nodes <- nds
            nodes_positions_assigned <- TRUE
          } # end if sel_row found
        },
        silent = TRUE
      )
    } # end if (!is.null(selected_name_for_graph))

    determine_company_edge_width <- function(row, output) {
      from <- row$from
      to <- row$to
      width <- 1
      # If it is an edge between company and project.
      if ((grepl("company_", from) && grepl("project_", to)) || (grepl("project_", from) && grepl("company_", to))) {
        company_idd <- if (grepl("company_", from)) sub("company_", "", from) else sub("company_", "", to)
        project_idd <- if (grepl("project_", from)) sub("project_", "", from) else sub("project_", "", to)

        # Get the role from df_filtered_for_graph()
        role <- as.character(df_filtered_for_graph() %>% filter(project_id == project_idd) %>% filter(company_id == company_idd) %>% select(company_role) %>% slice(1:1))

        # Return width based on company_role
        if (role == "Participation") {
          width <- 1
        }
        if (role == "Steering Committee") {
          width <- 4
        }
        if (role == "Funding") {
          width <- 7
        }
      }

      width
    }

    edges$width <- apply(edges, 1, determine_company_edge_width)

    # Create the network
    # Note: visNetwork will respect per-node 'physics' boolean and 'fixed' attributes.
    # We enable physics globally so unplaced nodes can still move,
    # while nodes with physics = FALSE remain fixed in their assigned positions.
    network <- visNetwork(nodes, edges, height = "600px") %>%
      visNodes(shadow = TRUE, borderWidth = 1) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE) %>%
      visEvents(select = "function(nodes) { Shiny.setInputValue('projects_page_graph_network_output_selected', nodes); }")

    # If we assigned positions to nodes, keep physics enabled globally but per-node physics flags control motion.
    # This way: assigned nodes (physics = FALSE) stay fixed; unassigned nodes (physics = TRUE) move.
    network <- network %>% visPhysics(enabled = TRUE, solver = "forceAtlas2Based", stabilization = TRUE)

    # Add group styling only if groups exist
    if ("Selected Researcher" %in% nodes$group) {
      network <- network %>% visGroups(groupname = "Selected Researcher", color = list(background = "#4CAF50", border = "#4CAF50"))
    }
    if ("Other Researcher" %in% nodes$group) {
      network <- network %>% visGroups(groupname = "Other Researcher", color = list(background = "#FFC107", border = "#FFC107"))
    }
    if ("Project" %in% nodes$group) {
      network <- network %>% visGroups(groupname = "Project", color = list(background = "#2196F3", border = "#2196F3"))
    }
    if ("Funding Company" %in% nodes$group) {
      # network <- network %>% visGroups(groupname = "Funding Company", color = list(background = "#B83027", border = "#B83027"))
      network <- network %>% visGroups(groupname = "Funding Company", color = list(background = "#FF0000", border = "#FF0000"))
    }
    if ("Steering Committee Company" %in% nodes$group) {
      # network <- network %>% visGroups(groupname = "Steering Committee Company", color = list(background = "#F44336", border = "#F44336"))
      network <- network %>% visGroups(groupname = "Steering Committee Company", color = list(background = "#FF0000", border = "#FF0000"))
    }
    if ("Participating Company" %in% nodes$group) {
      # network <- network %>% visGroups(groupname = "Participating Company", color = list(background = "#F77777", border = "#F77777"))
      network <- network %>% visGroups(groupname = "Participating Company", color = list(background = "#FF0000", border = "#FF0000"))
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

    if (nrow(filtered) != 0) {
      filtered <- filtered %>%
        cbind(sum_digit = 1) %>%
        group_by(company_name) %>%
        mutate(sort_digit = n()) %>%
        ungroup() %>%
        select(!c(sum_digit, sort_digit))
    }

    filtered
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
