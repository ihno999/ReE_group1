library(shiny)

fluidPage(
  
  titlePanel("RnE"),
  
  # Tabset panel with three tabs
  tabsetPanel(
    id = "mainTabs",
    type = "tabs",
    
    # Projects Tab
    tabPanel(
      "Projects",
      h3("Projects Content"),
      p("This is the Projects tab content.")
    ),
    
    # People Tab
    tabPanel(
      "People",
      sidebarLayout(
        sidebarPanel(
          h4("Side Panel"),
          selectInput("select_name", "Select a name:", 
            choices = c("", researchers_data$name))
          # selectInput("select_name", "Select a name:", researchers_data$name)
        ),
        mainPanel(
          h3("People Overview"),
          dataTableOutput("people_table")
        )
      )
    ),
    # Company Tab
    tabPanel(
      "Company",
      sidebarLayout(
        sidebarPanel(
          h4("Side Panel"),
          selectInput("select_company_name", "Select a company:", 
            choices = c("", company_data$name))
        ),
        mainPanel(
          h3("Company and Company-contacts Overview"),
          dataTableOutput("company_table")
        )
      )
    ),
    
    # Time Tab
    tabPanel(
      "Time",
      h3("Time Tracking"),
      p("This is the Time tab content.")
    )
  )
)