library(shiny)
ui_table_people <- sidebarLayout(
    sidebarPanel(
        h4("Side Panel"),
        uiOutput("companies_page_people_tab_select_input"),
        # Only show project filter when researcher is selected
        conditionalPanel(
            condition = "input.select_name != ''",
            checkboxGroupInput(
                "checkbox_projects_people",
                "Filter by Researcher's Projects:",
                choices = NULL,
                selected = NULL
            )
        ),
        width = 3
    ),
    mainPanel(
        card(
            h3("People Overview"),
            div(
                style = "overflow-y: visible; overflow-x: visible; height: auto; width: 100%; ",
                dataTableOutput("people_table")
            ),
            full_screen = TRUE,
            style = "overflow-x: hidden; overflow-y: visible;"
        )
    )
)

server_table_people <- function(input, output, session, rv) {
    # Filter for people data
    joined_people_data <- reactive({
        researchers_data %>%
            inner_join(research_participation_data, by = c("employee_id" = "researcher_id")) %>%
            inner_join(projects_data, by = "project_id") %>%
            inner_join(project_board_data, by = "project_id") %>%
            inner_join(company_contacts_data, by = "contact_id") %>%
            inner_join(company_data, by = "company_id")
    })

    # Reactive for researcher-specific projects
    researcher_projects <- reactive({
        req(input$select_name, input$select_name != "")

        joined_people_data() %>%
            filter(name.x == input$select_name) %>%
            distinct(name.y) %>%
            pull(name.y)
    })

    # Update project filter choices based on selected researcher's projects
    # observe({
    #     req(input$select_name, input$select_name != "")

    #     projects <- researcher_projects()

    #     updateCheckboxGroupInput(
    #         session,
    #         "checkbox_projects_people",
    #         choices = projects,
    #         selected = projects # Default to all selected
    #     )
    # })

    output$companies_page_people_tab_select_input <- renderUI({
        data <- joined_people_data()
        selectInput("select_name", "Select a researcher:",
            choices = c("", data$name.x %>% sort()),
            selected = rv$selected_node_researcher_name
        )
    })

    # Ensure People tab updates when a researcher is clicked in the graph
    observeEvent(
        list(rv$selected_node_researcher_name, rv$selected_node_connected_projects, rv$trigger_people_refresh),
        {
            # If no researcher in rv, do nothing
            if (is.null(rv$selected_node_researcher_name) || rv$selected_node_researcher_name == "") {
                return()
            }

            # Build the available researcher choices from the joined data (same logic as renderUI)
            data <- joined_people_data()
            researcher_choices <- c("", sort(unique(data$name.x)))

            # Debugging: check whether the name from rv exists among choices
            sel_name <- rv$selected_node_researcher_name
            if (!(sel_name %in% researcher_choices)) {
                # Try trimming whitespace or normalizing characters as a fallback
                sel_name_trim <- trimws(sel_name)
                if (sel_name_trim %in% researcher_choices) sel_name <- sel_name_trim
            }

            # If still not in choices, pick "" (no selection) and warn in console
            if (!(sel_name %in% researcher_choices)) {
                sel_name_to_use <- ""
            } else {
                sel_name_to_use <- sel_name
            }

            # Update the selectInput (also creates it if UI has been re-rendered)
            tryCatch(
                {
                    updateSelectInput(session, "select_name", selected = sel_name_to_use)
                },
                error = function(e) {}
            )

            # Now update the project checkboxes based on rv$selected_node_connected_projects if present
            # Determine the projects available for that researcher
            projects_for_researcher <- character(0)
            if (sel_name_to_use != "") {
                # researcher_projects() expects input$select_name to be set; we can compute directly from joined data
                projects_for_researcher <- data %>%
                    filter(name.x == sel_name_to_use) %>%
                    distinct(name.y) %>%
                    pull(name.y) %>%
                    as.character()
            }

            # Determine selected projects: prefer rv list, but intersect with available projects
            selected_projects <- NULL
            if (!is.null(rv$selected_node_connected_projects) && length(rv$selected_node_connected_projects) > 0) {
                selected_projects <- intersect(as.character(rv$selected_node_connected_projects), projects_for_researcher)

                if (length(selected_projects) == 0) {
                    # fallback to all projects for that researcher
                    selected_projects <- projects_for_researcher
                }
            } else {
                # No rv project list → default to all projects for researcher
                selected_projects <- projects_for_researcher
            }

            # Finally update checkboxGroupInput choices + selection
            tryCatch(
                {
                    updateCheckboxGroupInput(
                        session,
                        "checkbox_projects_people",
                        choices = projects_for_researcher,
                        selected = selected_projects
                    )
                },
                error = function(e) {}
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
    )

    filtered_data <- reactive({
        data <- joined_people_data()

        # If no researcher is selected, show all data
        if (is.null(input$select_name) || input$select_name == "") {
            return(data)
        }

        # Filter by selected researcher name
        data <- data[data$name.x == input$select_name, , drop = FALSE]

        # Only apply project filter if researcher is selected AND projects are selected
        if (!is.null(input$checkbox_projects_people)) {
            # If projects are selected, filter by them
            if (length(input$checkbox_projects_people) > 0) {
                data <- data[data$name.y %in% input$checkbox_projects_people, , drop = FALSE]
            }
            # If no projects are selected (empty checkbox), show nothing for that researcher
            else {
                data <- data[FALSE, , drop = FALSE] # Empty dataframe
            }
        }

        data
    })

    # Render the filtered people table
    output$people_table <- DT::renderDataTable({
        df <- filtered_data()

        if (nrow(df) == 0) {
            # Show empty table with message
            return(DT::datatable(
                data.frame(Message = "No data available for the selected filters"),
                options = list(pageLength = 10, scrollX = FALSE),
                escape = FALSE
            ))
        }

        if ("description.x" %in% colnames(df)) {
            df$description.x <- sapply(df$description.x, function(text) {
                paste0(
                    '<details><summary>Show</summary><p style="width: 500px">',
                    text,
                    "</p></details>"
                )
            })
        }

        # Columns to hide
        columns_to_hide <- c(
            "employee_id",
            "main_research_group",
            "project_id",
            "responsible_group",
            "responsible_employee",
            "days_allocated",
            "start_date",
            "end_date",
            "contact_id",
            "company_id",
            "sectors",
            "description.y",
            "size"
        )


        # Remove columns that exist in the dataframe
        existing_columns_to_hide <- columns_to_hide[columns_to_hide %in% colnames(df)]
        df <- df[, !colnames(df) %in% existing_columns_to_hide, drop = FALSE] %>%
            rename(
                researcher_name = name.x,
                researcher_role = role.x,
                project_name = name.y,
                project_description = description.x,
                project_total_budget = total_budget,
                project_funding_source = funding_source,
                project_type = type,
                ext_role = role.y,
                ext_contact_name = name.x.x,
                ext_email = email,
                ext_phone = phone,
                ext_job_title = job_title,
                ext_department = department,
                ext_company = name.y.y
            ) %>%
            select(project_name, researcher_role, ext_role, ext_contact_name, ext_department, ext_job_title, ext_email, ext_phone, project_description, project_total_budget, project_type, )

        DT::datatable(
            df,
            options = list(pageLength = 10, scrollX = FALSE),
            escape = FALSE
        )
    })
}
