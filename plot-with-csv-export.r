## Advanced Plot with csv export

server <- function(input,output, session) {
 
  library(ggplot2) # for the diamonds dataset, and ggplot feature
  library(DT) # for the dataTableOutput
 
  output$plot <- renderPlot({
    ggplot(diamonds, aes(price, carat)) + geom_point()
  })
 
  diam <- reactive({
    user_brush <- input$user_brush
    sel <- brushedPoints(diamonds, user_brush)
    return(sel)
  })
 
  output$table <- DT::renderDataTable(DT::datatable(diam()))
 
  output$mydownload <- downloadHandler(
    filename = "plotextract.csv",
    content = function(file) {
      write.csv(diam(), file)})
}

ui <- fluidPage(
  h3("Exporting Data as .csv"),
  plotOutput("plot", brush = "user_brush"),
  dataTableOutput("table"),
  downloadButton(outputId = "mydownload", label = "Download Table")
)

shinyApp(ui = ui, server = server)
