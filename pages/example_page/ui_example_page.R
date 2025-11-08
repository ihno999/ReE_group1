ui_example_page <- fluidPage(
  titlePanel("Hello World!!"),
  sidebarLayout(
    sidebarPanel(sliderInput('nb_bins', 'Number of bins', 5, 10 , 5)),
    mainPanel(
      tabsetPanel(
        tabPanel('Table', DT::DTOutput("companies_table")),
        tabPanel('Graph', DT::DTOutput("companies_graph"))
      )
    )
  )
)