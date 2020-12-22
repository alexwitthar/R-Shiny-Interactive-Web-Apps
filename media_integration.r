## Media Integration

library(shiny)

server = function(input,output) {}

ui = navbarPage("Integration of different media types",
                
  tabPanel("Image sourced locally",
           tags$img(src = "logo.png", width = "100px", height = "100px")),
 
  tabPanel("Video sourced locally",
           tags$video(src = "comist.mp4", type = "video/mp4", controls = T,
                      width = "900px", height = "800px")),
 
  tabPanel("Pdf sourced online, Iframe",
           tags$iframe(style="height:600px; width:100%; scrolling=yes",
                       src="https://cran.r-project.org/web/packages/shiny/shiny.pdf")),
 
  tabPanel("Text as .txt",
           includeText("mytxt.txt"))
)

shinyApp(ui = ui, server = server)
