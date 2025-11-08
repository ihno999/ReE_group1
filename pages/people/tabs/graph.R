### UI
vars <- setdiff(names(iris), "Species")

ui_graph <- sidebarLayout(
  sidebarPanel(
    selectInput('people_page_xcol', 'X Variable', vars),
    selectInput('people_page_ycol', 'Y Variable', vars, selected = vars[[2]]),
    numericInput('people_page_clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  mainPanel(plotOutput('people_page_plot1'))
)


### Server
server_graph <- function(input, output) {
    selectedData <- reactive({
      iris[, c(input$people_page_xcol, input$people_page_ycol)]
    })

    clusters <- reactive({
      kmeans(selectedData(), input$people_page_clusters)
    })

    output$people_page_plot1 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
}