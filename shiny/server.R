library(shiny)

source("lib/libraries.r", encoding = "UTF-8")
source("uvoz/uvoz.r", encoding = "UTF-8")

runApp("shiny")

fluidPage(headerPanel('Skupine'),
              sidebarPanel(
                selectInput('xcol', 'X ', names(tabela)),
                selectInput('ycol', 'Y ', names(tabela),
                              selected = names(tabela)[[2]]),
                numericInput('clusters', 'skupine', 3,
                               min = 1, max = 5)
              ),
              mainPanel(
                plotOutput('kmeans')
              ))
function(input, output) {
  selectedData <- reactive({
  tabela[, c(input$xcol, input$ycol)]
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

shinyApp(ui=ui, server = server)