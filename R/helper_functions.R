clean_column_names <- function() {
    # Make all column names lowercase and consistent
    company_data <<- company_data %>%
        rename_with(tolower) %>%
        rename(
            company_id = any_of(c("company_id")),
            company_name = any_of(c("name"))
        )

    company_contacts_data <<- company_contacts_data %>%
        rename_with(tolower) %>%
        rename(
            contact_id = any_of(c("contact_id")),
            company_id = any_of(c("company_id"))
        )

    project_board_data <<- project_board_data %>%
        rename_with(tolower) %>%
        rename(
            project_id = any_of(c("project_id")),
            contact_id = any_of(c("contact_id"))
        )

    projects_data <<- projects_data %>%
        rename_with(tolower) %>%
        rename(
            project_id = any_of(c("project_id")),
            type = any_of(c("type"))
        )

    research_groups_data <<- research_groups_data %>%
        rename_with(tolower)

    research_participation_data <<- research_participation_data %>%
        rename_with(tolower) %>%
        rename(
            project_id = any_of(c("project_id")),
            researcher_id = any_of(c("researcher_id"))
        )

    researchers_data <<- researchers_data %>%
        rename_with(tolower) %>%
        rename(
            researcher_id = any_of(c("employee_id")),
            researcher_name = any_of(c("name")),
            main_research_group = any_of(c("main_research_group"))
        )

    message("All data column names have been standardized.")
}
