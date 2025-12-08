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
  observeEvent(input$projects_page_graph_selection,
    {
      rv$selection <- input$projects_page_graph_selection
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
        if ("description" %in% colnames(node_information_table)) {
          node_information_table$description <- sapply(node_information_table$description, function(text) {
            paste0(
              '<details><summary>Show</summary><p/style="width: 500px">',
              text,
              "</p></details>"
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

    # ---- NEW: Per-project circular rings around each project's current position ----
    # Behavior implemented:
    #  - Project nodes themselves are not forcibly arranged in a circle.
    #    Instead we compute sensible positions for project centers using a small layout
    #    (so centers are relatively well spaced) and then place project-unique nodes
    #    in perfect circles around those computed project center positions.
    #  - Nodes connected to multiple projects remain a single node and are left to physics
    #    (they get no forced x/y coordinates), so they float in neutral space and connect to all projects.
    #  - Collision avoidance: we scale the project-center layout / ring radii until no ring overlaps.
    #  - Fixed nodes (project nodes and project-unique ring nodes) have physics disabled so rings remain stable.
    #  - Other nodes keep physics enabled.
    #
    # Implementation notes:
    #  - We derive project centers using igraph layout on a small project-projection graph,
    #    then scale / jitter / check overlaps and adjust until rings do not overlap (Option B).
    #  - The algorithm is conservative and deterministic (seed set).
    #
    nodes_positions_assigned <- FALSE
    try(
      {
        nds <- nodes
        eds <- edges

        # Identify project nodes and non-project nodes
        project_nodes <- nds %>% filter(group == "Project")
        non_project_nodes <- nds %>% filter(group != "Project")

        # If there are project nodes and other nodes, proceed; otherwise skip
        if (nrow(project_nodes) >= 1 && nrow(nds) > 1) {
          # Build mappings
          project_ids <- as.character(project_nodes$id)
          non_project_ids <- as.character(non_project_nodes$id)

          # Edges connecting projects <-> non-projects
          eds_proj_links <- eds %>%
            filter((from %in% project_ids & to %in% non_project_ids) | (to %in% project_ids & from %in% non_project_ids)) %>%
            mutate(
              project = ifelse(from %in% project_ids, from, to),
              non_project = ifelse(from %in% non_project_ids, from, to)
            ) %>%
            select(project, non_project) %>%
            distinct()

          # Count how many projects each non-project node is connected to
          non_proj_project_counts <- eds_proj_links %>%
            group_by(non_project) %>%
            summarise(project_count = n(), projects = list(project), .groups = "drop")

          # Unique-to-project nodes (project_count == 1)
          single_project_nodes <- non_proj_project_counts %>%
            filter(project_count == 1) %>%
            pull(non_project)

          # Multi-project nodes (project_count > 1) - will be left unplaced
          multi_project_nodes <- non_proj_project_counts %>%
            filter(project_count > 1) %>%
            pull(non_project)

          # For each project, get its unique nodes (the nodes that are linked only to that project)
          project_unique_lookup <- eds_proj_links %>%
            filter(non_project %in% single_project_nodes) %>%
            group_by(project) %>%
            summarise(unique_non_projects = list(non_project), .groups = "drop") %>%
            {
              setNames(.$unique_non_projects, .$project)
            }

          # Prepare project center positions via igraph layout on project-projection graph:
          # Create adjacency between projects by shared multi-project nodes (or by 0 if none)
          # This helps spread project centers sensibly relative to each other.
          proj_adj_pairs <- eds_proj_links %>%
            # produce project-project pairs when a non-project node is connected to multiple projects
            group_by(non_project) %>%
            filter(n() > 1) %>%
            summarise(pp = combn(sort(unique(project)), 2, FUN = function(x) paste(x, collapse = "|"), simplify = TRUE), .groups = "drop") %>%
            pull(pp)

          # Expand proj_adj_pairs into edges for igraph
          proj_edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
          if (!is.null(proj_adj_pairs) && length(proj_adj_pairs) > 0) {
            pairs <- unlist(proj_adj_pairs)
            pairs_split <- strsplit(pairs, "\\|")
            proj_edges <- do.call(rbind, lapply(pairs_split, function(x) data.frame(from = x[1], to = x[2], stringsAsFactors = FALSE)))
          }

          # If there are no project-project edges, create a minimal dummy connection to avoid isolated layout degeneracy
          if (nrow(proj_edges) == 0 && length(project_ids) > 1) {
            # link consecutive projects in a chain so layout spreads them
            proj_edges <- data.frame(from = head(project_ids, -1), to = tail(project_ids, -1), stringsAsFactors = FALSE)
          }

          # Build igraph for project centers
          library(igraph)
          if (nrow(proj_edges) > 0) {
            gproj <- graph_from_data_frame(proj_edges, vertices = project_ids, directed = FALSE)
          } else {
            # single project or no project edges
            gproj <- make_empty_graph(n = length(project_ids))
            V(gproj)$name <- project_ids
          }

          # compute a 2D layout for project centers
          set.seed(123)
          # Use Fruchterman-Reingold (layout_with_fr) since it tends to space nodes well
          proj_layout_raw <- layout_with_fr(gproj)

          # Map layout rows to project ids
          proj_layout_df <- data.frame(project = V(gproj)$name, x = proj_layout_raw[, 1], y = proj_layout_raw[, 2], stringsAsFactors = FALSE)

          # Normalize & scale layout to a base spread
          if (nrow(proj_layout_df) > 0) {
            # zero-center and scale
            proj_layout_df$x <- proj_layout_df$x - mean(proj_layout_df$x)
            proj_layout_df$y <- proj_layout_df$y - mean(proj_layout_df$y)
            # scale to desired project center spread scale
            base_spread <- 300
            max_range <- max(diff(range(proj_layout_df$x)), diff(range(proj_layout_df$y)))
            if (max_range == 0) max_range <- 1
            scale_factor <- base_spread / max_range
            proj_layout_df$x <- proj_layout_df$x * scale_factor
            proj_layout_df$y <- proj_layout_df$y * scale_factor
          } else {
            proj_layout_df <- data.frame(project = project_ids, x = 0, y = 0, stringsAsFactors = FALSE)
          }

          # For each project, define ring radius based on number of unique nodes
          # and a base ring radius for minimal spacing. Also add buffer for label/visuals.
          ring_radius_base <- 110
          ring_spacing_per_node <- 18
          project_radii <- sapply(project_ids, function(pid) {
            uniques <- project_unique_lookup[[pid]]
            n_uniques <- if (is.null(uniques)) 0 else length(uniques)
            # Minimal radius grows with node count
            ring_radius_base + max(0, (n_uniques - 1)) * ring_spacing_per_node
          })
          names(project_radii) <- project_ids

          # Collision avoidance:
          # If any pair of project centers are too close (center distance < radius_i + radius_j + margin),
          # increase the overall scale until resolved (iterative scaling).
          margin_between_rings <- 40
          scale_attempt <- 1.0
          max_attempts <- 20
          attempt <- 1
          resolved <- FALSE
          while (attempt <= max_attempts && !resolved) {
            # compute scaled centers
            centers <- proj_layout_df
            centers$x <- centers$x * scale_attempt
            centers$y <- centers$y * scale_attempt

            # check all pairwise distances
            if (nrow(centers) <= 1) {
              resolved <- TRUE
              break
            }
            mat_dist <- as.matrix(dist(centers[, c("x", "y")]))
            # get project radii matched
            radii_vec <- project_radii[centers$project]
            # check overlaps
            overlap_found <- FALSE
            for (i in seq_len(nrow(mat_dist))) {
              for (j in seq_len(nrow(mat_dist))) {
                if (i >= j) next
                d <- mat_dist[i, j]
                ri <- radii_vec[i]
                rj <- radii_vec[j]
                if (d < (ri + rj + margin_between_rings)) {
                  overlap_found <- TRUE
                  break
                }
              }
              if (overlap_found) break
            }
            if (!overlap_found) {
              resolved <- TRUE
              break
            } else {
              # increase scale and try again
              scale_attempt <- scale_attempt * 1.25
              attempt <- attempt + 1
            }
          }

          # Final centers after scaling
          proj_layout_df$x <- proj_layout_df$x * scale_attempt
          proj_layout_df$y <- proj_layout_df$y * scale_attempt

          # Now assign coordinates:
          # - projects at proj_layout_df positions
          # - unique nodes for each project on a circle around the project's center
          # - multi-project nodes left without coords (NA) -> physics will place them
          nds$x <- NA_real_
          nds$y <- NA_real_
          nds$fixed <- FALSE
          nds$physics <- TRUE # default; we'll disable for fixed nodes later

          # place project centers
          for (i in seq_len(nrow(proj_layout_df))) {
            pid <- proj_layout_df$project[i]
            cx <- proj_layout_df$x[i]
            cy <- proj_layout_df$y[i]
            idx_p <- which(nds$id == pid)
            if (length(idx_p) == 1) {
              nds$x[idx_p] <- cx
              nds$y[idx_p] <- cy
              nds$fixed[idx_p] <- TRUE
              nds$physics[idx_p] <- FALSE
            }
          }

          # place project-unique nodes on rings
          for (i in seq_len(nrow(proj_layout_df))) {
            pid <- proj_layout_df$project[i]
            cx <- proj_layout_df$x[i]
            cy <- proj_layout_df$y[i]
            uniques <- project_unique_lookup[[pid]]
            if (is.null(uniques) || length(uniques) == 0) next
            ni <- length(uniques)
            angles <- seq(0, 2 * pi, length.out = ni + 1)[-1]
            ring_radius <- project_radii[[pid]]
            # small jitter to prevent perfect overlap with neighbouring visuals
            set.seed(200 + i)
            jitter_amt <- 0.04 * ring_radius
            for (j in seq_len(ni)) {
              node_id_j <- uniques[j]
              idx_n <- which(nds$id == node_id_j)
              if (length(idx_n) == 1) {
                nds$x[idx_n] <- cx + (ring_radius + runif(1, -jitter_amt, jitter_amt)) * cos(angles[j])
                nds$y[idx_n] <- cy + (ring_radius + runif(1, -jitter_amt, jitter_amt)) * sin(angles[j])
                nds$fixed[idx_n] <- TRUE
                nds$physics[idx_n] <- FALSE
              }
            }
          }

          # Multi-project nodes stay with NA coords and physics TRUE (they will be placed by vis.js)
          # For any leftover nodes that still have NA coords (edge cases), place them on a fallback outer ring
          remaining_idx <- which(is.na(nds$x) | is.na(nds$y))
          if (length(remaining_idx) > 0) {
            # We'll keep multi-project nodes floating (physics TRUE, coords NA) so skip setting coords for them.
            # But if some single nodes were missed (shouldn't happen), provide a gentle fallback around origin.
            # We'll only assign coords to those with project membership but no assigned coords AND that are single-project.
            for (ri in remaining_idx) {
              nid <- nds$id[ri]
              # only fallback for nodes that are single-project but somehow missed
              if (nid %in% single_project_nodes) {
                # find the project it belongs to
                proj_row <- eds_proj_links %>%
                  filter(non_project == nid) %>%
                  pull(project) %>%
                  unique()
                if (length(proj_row) >= 1) {
                  pid <- proj_row[1]
                  cx <- proj_layout_df$x[proj_layout_df$project == pid]
                  cy <- proj_layout_df$y[proj_layout_df$project == pid]
                  ring_radius <- project_radii[[pid]]
                  angle <- runif(1, 0, 2 * pi)
                  nds$x[ri] <- cx + ring_radius * cos(angle)
                  nds$y[ri] <- cy + ring_radius * sin(angle)
                  nds$fixed[ri] <- TRUE
                  nds$physics[ri] <- FALSE
                }
              }
            }
          }

          # Transfer coordinates back to nodes
          nodes <- nds
          nodes_positions_assigned <- TRUE
        }
      },
      silent = TRUE
    )

    # Create the network
    # Note: visNetwork will respect per-node 'physics' boolean and 'fixed' attributes.
    # We enable physics globally so multi-project nodes and other unfixed nodes can still move,
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
