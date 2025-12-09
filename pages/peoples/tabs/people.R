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
    observe({
        req(input$select_name, input$select_name != "")
        
        projects <- researcher_projects()
        
        updateCheckboxGroupInput(
            session,
            "checkbox_projects_people",
            choices = projects,
            selected = projects  # Default to all selected
        )
    })
    
    output$companies_page_people_tab_select_input <- renderUI({
        data <- joined_people_data()
        selectInput("select_name", "Select a researcher:",
                    choices = c("", data$name.x %>% sort()),
                    selected = rv$selected_node_researcher_name
        )
    })
    
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
                data <- data[FALSE, , drop = FALSE]  # Empty dataframe
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
                    '</p></details>'
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
        df <- df[, !colnames(df) %in% existing_columns_to_hide, drop = FALSE]


        DT::datatable(
            df,
            options = list(pageLength = 10, scrollX = FALSE),
            escape = FALSE
        )
    })
}