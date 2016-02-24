library(shiny)

fluidPage(headerPanel('Skupine'),
          sidebarPanel(
            selectInput('xcol', 'X ', names(tabela4)),
            selectInput('ycol', 'Y ', names(tabela4),
                        selected = names(tabela4)[[2]]),
            numericInput('clusters', 'skupine', 3,
                         min = 1, max = 5)
          ),
          mainPanel(
            plotOutput('kmeans')
          ))