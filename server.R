source("./global.R")

# library(shiny)
# library(DT)
library(dplyr)

server <- function(input, output, session) {
  joined_data <- reactive({
    researchers_data %>%
      inner_join(research_groups_data, by = c("main_research_group" = "group_id")) %>%
      inner_join(research_participation_data, by = c("employee_id" = "researcher_id")) %>%
      inner_join(projects_data, by = "project_id")

  })


  filtered_data <- reactive({

    data <- joined_data()

    if (!is.null(input$select_name) && input$select_name != "") {
      data <- data[data$name.x == input$select_name, , drop = FALSE]
    }

    data
  })
  
  # Render the filtered table
  output$people_table <- DT::renderDataTable({
    df <- filtered_data()
    
    if("description" %in% colnames(df)) {
      df$description <- sapply(df$description, function(text) {
        paste0(
          '<details><summary>Show</summary>',
          text,
          '</details>'
        )
      })
    }
    
    DT::datatable(
      df,
      options = list(pageLength = 10, scrollX = TRUE),
      escape = FALSE
    )
  })


}
