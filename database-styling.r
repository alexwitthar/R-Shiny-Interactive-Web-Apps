## Datatable styling

library(shiny)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  output$tableDT <- DT::renderDataTable(datatable(diamonds[1:1000,],
                                                  options = list(paging=F),
                                                  rownames=F,
                                                  filter = "top") %>%
                                          formatCurrency("price", "$") %>%
                                          formatStyle("price", color = "green") %>%
                                          formatStyle("cut",
                                                      transform = "rotateX(20deg) rotateY(5deg) rotateZ(5deg)",
                                                      backgroundColor = styleEqual(
                                                        unique(diamonds$cut), c("salmon", "lightblue",
                                                                                "grey", "lightgreen", "lightpink"))))
}

ui <- fluidPage(
  DT::dataTableOutput("tableDT")
)

shinyApp(ui = ui, server = server)
