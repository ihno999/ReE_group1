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



# projects/tabs/graph.R
# Filter projects by selected field/type
filter_projects_by_field <- function(projects_data, field) {
  projects_data %>% dplyr::filter(type == field)
}

# Get related researcher and company data for a given field's projects
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




# df_for_project_details_stacked_bar_chart <- reactive({
#
# })