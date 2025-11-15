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

prepare_network_graph_data <- function(filtered_data, view_type = "Researcher", selected_name = NULL) {
  # ---- Researchers ----
  researchers <- filtered_data %>%
    select(id = researcher_id, name = researcher_name) %>%
    distinct() %>%
    filter(!is.na(id)) %>%
    mutate(
      id = as.character(id),
      type = "researcher",
      color = if_else(
        name == selected_name & view_type == "Researcher",
        "#4CAF50",
        "#FFC107"
      ),
      size = if_else(
        name == selected_name & view_type == "Researcher",
        20, 15
      )
    )

  # ---- Projects ----
  projects <- filtered_data %>%
    select(id = project_id, name = project_name) %>%
    distinct() %>%
    mutate(
      id = as.character(id),
      type = "project",
      color = "#2196F3",
      size = 12
    )

  # ---- Companies ----
  companies <- filtered_data %>%
    select(id = company_id, name = company_name) %>%
    distinct() %>%
    filter(!is.na(id)) %>%
    mutate(
      id = as.character(id),
      type = "company",
      color = if_else(
        name == selected_name & view_type == "Company",
        "#4CAF50",
        "#F44336"
      ),
      size = if_else(
        name == selected_name & view_type == "Company",
        20, 15
      )
    )

  nodes <- bind_rows(researchers, projects, companies)

  # ---- Edges: Researcher → Project ----
  researcher_project_edges <- filtered_data %>%
    select(from = researcher_id, to = project_id) %>%
    distinct() %>%
    mutate(from = as.character(from), to = as.character(to))

  # ---- Edges: Project → Company ----
  project_company_edges <- filtered_data %>%
    filter(!is.na(company_id)) %>%
    select(from = project_id, to = company_id) %>%
    distinct() %>%
    mutate(from = as.character(from), to = as.character(to))

  edges <- bind_rows(researcher_project_edges, project_company_edges)

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
