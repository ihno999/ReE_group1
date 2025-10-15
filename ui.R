source("./global.R")

ui <- fluidPage(
  titlePanel("Hello World!!"),
  sidebarLayout(
    sidebarPanel(sliderInput('nb_bins', 'Number of bins', 5, 10 , 5)),
    mainPanel(DT::DTOutput("companies_table"))
  )
)