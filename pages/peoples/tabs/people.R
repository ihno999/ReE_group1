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
    # Use the already loaded full people data
    people_data <- people_full_data

    # Render researcher select input (completely independent)
    output$companies_page_people_tab_select_input <- renderUI({
        selectInput(
            "select_name",
            "Select a researcher:",
            choices = c("", sort(unique(people_data$name_x))),
            selected = if (!is.null(rv$selected_node_researcher_name)) rv$selected_node_researcher_name else ""
        )
    })

    # Reactive: projects for selected researcher
    researcher_projects <- reactive({
        req(input$select_name, input$select_name != "")
        unique(people_data$project_name[people_data$researcher_name == input$select_name])
    })

    # Update project checkboxes dynamically
    observe({
        req(input$select_name, input$select_name != "")

        projects <- researcher_projects()

        updateCheckboxGroupInput(
            session,
            "checkbox_projects_people",
            choices = projects,
            selected = projects # Default to all selected
        )
    })

    # Filtered data for the table
    filtered_data <- reactive({
        data <- people_data

        # Filter by selected researcher
        if (!is.null(input$select_name) && input$select_name != "") {
            data <- data[data$researcher_name == input$select_name, , drop = FALSE]
        }

        # Apply project filter if projects are selected
        if (!is.null(input$checkbox_projects_people) && length(input$checkbox_projects_people) > 0) {
            data <- data[data$project_name %in% input$checkbox_projects_people, , drop = FALSE]
        }

        data
    })

    # Render the table
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

        # Wrap project description
        if ("project_description" %in% colnames(df)) {
            df$project_description <- sapply(df$project_description, function(text) {
                paste0('<details><summary>Show</summary><p style="width:500px">', text, "</p></details>")
            })
        }

        # Columns to hide
        hide_cols <- c(
            "researcher_id", "main_research_group", "responsible_group",
            "responsible_employee", "contact_id"
        )
        existing_hide_cols <- hide_cols[hide_cols %in% colnames(df)]
        df <- df[, !colnames(df) %in% existing_hide_cols, drop = FALSE]

        DT::datatable(df, options = list(pageLength = 10, scrollX = FALSE), escape = FALSE)
    })
}
