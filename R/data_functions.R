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