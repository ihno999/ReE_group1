library(shiny)
ui_table_people <- sidebarLayout(
    sidebarPanel(
        h4("Side Panel"),
        selectInput("select_name", "Select a name:",
            choices = c("", researchers_data$name)
        ),
        # selectInput("select_name", "Select a name:", researchers_data$name)
        width = 2
    ),
    mainPanel(
        card(
            h3("People Overview"),
            div(
                style = "overflow: visible; height: auto;",
                dataTableOutput("people_table")
            ),
            full_screen = TRUE,
            style = "overflow: visible;"
        )
    )
)

server_table_people <- function(input, output, session) {
    # Filter for people data
    joined_people_data <- reactive({
        researchers_data %>%
            inner_join(research_groups_data, by = c("main_research_group" = "group_id")) %>%
            inner_join(research_participation_data, by = c("employee_id" = "researcher_id")) %>%
            inner_join(projects_data, by = "project_id")
    })
    filtered_data <- reactive({
        data <- joined_people_data()

        if (!is.null(input$select_name) && input$select_name != "") {
            data <- data[data$name.x == input$select_name, , drop = FALSE]
        }

        data
    })

    # Render the filtered people table
    output$people_table <- DT::renderDataTable({
        df <- filtered_data()

        if ("description" %in% colnames(df)) {
            df$description <- sapply(df$description, function(text) {
                paste0(
                    "<details><summary>Show</summary>",
                    text,
                    "</details>"
                )
            })
        }

        DT::datatable(
            df,
            options = list(pageLength = 10, scrollX = FALSE),
            escape = FALSE
        )
    })
}
