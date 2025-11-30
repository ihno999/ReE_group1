# Filter data based on user inputs
filter_sales_data <- function(data, date_range, selected_regions) {
  data %>%
    filter(
      date >= date_range[1],
      date <= date_range[2],
      region %in% selected_regions
    )
}

# Calculate summary metrics
calculate_monthly_sales <- function(data) {
  data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month, region) %>%
    summarise(
      total_sales = sum(sales, na.rm = TRUE),
      avg_sales = mean(sales, na.rm = TRUE),
      .groups = "drop"
    )
}



# ----------------------------
# PROJECTS / NETWORK
# ----------------------------

# Filter projects by selected field/type
filter_projects_by_field <- function(projects_data, field) {
  projects_data %>% dplyr::filter(type == field)
}

# Get related researcher + company data for a given field
get_related_collaboration_data <- function(filtered_projects) {
  fp <- filtered_projects

  # Researchers linked to these projects
  rp <- research_participation_data %>%
    dplyr::inner_join(fp, by = "project_id") %>%
    dplyr::inner_join(researchers_data, by = c("researcher_id" = "employee_id"))

  # Companies linked to these projects
  pb <- project_board_data %>%
    dplyr::inner_join(fp, by = "project_id") %>%
    dplyr::inner_join(company_contacts_data, by = "contact_id") %>%
    dplyr::inner_join(company_data, by = "company_id")

  list(projects = fp, researchers = rp, companies = pb)
}

# ----------------------------
# NETWORK GRAPH DATA BUILDER
# ----------------------------
prepare_network_graph_data <- function(df, graph_type, selected_name = NULL) {
  # Create nodes for researchers
  researcher_nodes <- df %>%
    distinct(researcher_id, researcher_name) %>%
    filter(!is.na(researcher_id) & !is.na(researcher_name)) %>%
    transmute(
      id = as.character(paste0("researcher_", researcher_id)),
      name = researcher_name,
      type = "Researcher",
      group = if (is.null(selected_name) || selected_name == "" || selected_name == "All researchers") {
        "Other Researcher"
      } else {
        ifelse(researcher_name == selected_name, "Selected Researcher", "Other Researcher")
      }
    )
  
  # Create nodes for projects
  project_nodes <- df %>%
    distinct(project_id, project_name) %>%
    filter(!is.na(project_id) & !is.na(project_name)) %>%
    transmute(
      id = as.character(paste0("project_", project_id)),
      name = project_name,
      type = "Project",
      group = "Project"
    )
  
  # Create nodes for companies with roles - handle missing roles
  company_nodes <- df %>%
    distinct(company_id, company_name, company_role) %>%
    filter(!is.na(company_id) & !is.na(company_name)) %>%
    mutate(
      # Handle missing roles by defaulting to "Participation"
      company_role = ifelse(is.na(company_role) | company_role == "", "Participation", company_role),
      group = case_when(
        company_role == "Funding" ~ "Funding Company",
        company_role == "Steering Committee" ~ "Steering Committee Company",
        company_role == "Participation" ~ "Participating Company",
        TRUE ~ "Participating Company"  # default case
      )
    ) %>%
    transmute(
      id = as.character(paste0("company_", company_id)),
      name = company_name,
      type = "Company",
      group = group,
      role = company_role
    )
  
  # Combine all nodes and ensure unique IDs
  nodes <- bind_rows(researcher_nodes, project_nodes, company_nodes) %>%
    filter(!is.na(id) & !is.na(name)) %>%
    distinct(id, .keep_all = TRUE)
  
  # Create edges: researchers to projects
  edges_res_proj <- df %>%
    filter(!is.na(researcher_id) & !is.na(project_id)) %>%
    transmute(
      from = as.character(paste0("researcher_", researcher_id)),
      to = as.character(paste0("project_", project_id))
    ) %>%
    distinct()
  
  # Create edges: companies to projects
  edges_comp_proj <- df %>%
    filter(!is.na(company_id) & !is.na(project_id)) %>%
    transmute(
      from = as.character(paste0("company_", company_id)),
      to = as.character(paste0("project_", project_id))
    ) %>%
    distinct()
  
  # Combine edges
  edges <- bind_rows(edges_res_proj, edges_comp_proj) %>%
    filter(!is.na(from) & !is.na(to)) %>%
    distinct()
  
  list(nodes = nodes, edges = edges)
}

# Filter network data by researcher and fields
filter_network_data <- function(data, researcher_name, project_fields, company_name = NULL) {
  filtered <- data %>%
    filter(researcher_name == !!researcher_name) %>%
    filter(project_field %in% project_fields)

  if (!is.null(company_name) && company_name != "all") {
    filtered <- filtered %>% filter(company_name == !!company_name)
  }

  filtered
}

# Get unique companies for a researcher given selected fields
get_companies_for_researcher <- function(data, researcher_name, project_fields) {
  data %>%
    filter(researcher_name == !!researcher_name) %>%
    filter(project_field %in% project_fields) %>%
    pull(company_name) %>%
    unique() %>%
    na.omit() %>%
    sort()
}
