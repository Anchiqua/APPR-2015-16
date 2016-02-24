library(shiny)

source("lib/libraries.r", encoding = "UTF-8")
source("uvoz/uvoz.r", encoding = "UTF-8")

runApp("shiny")


function(input, output) {
  selectedData <- reactive({
  tabela4[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  output$kmeans <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
        col = clusters()$cluster,
        pch = 20, cex = 3)
      })
}
