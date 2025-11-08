# source("./global.R")

# ui <- fluidPage(
#   # titlePanel("Hello World!!"),
#   # sidebarLayout(
#   #   sidebarPanel(sliderInput('nb_bins', 'Number of bins', 5, 10 , 5)),
#   #   mainPanel(DT::DTOutput("companies_table"))
#   # )

#   titlePanel("Research Collaboration Network"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("researcher", "Select Researcher:", choices = unique(researchers_data$researcher_name)),
#       selectInput("selected_field", "Field of new project", choices = NULL)
#     ),
#     mainPanel(
#       visNetworkOutput("network_plot", height = "600px")
#     )
#   )
# )