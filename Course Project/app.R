library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(janitor)

stockData <- read_csv2("course_proj_data.csv") %>%
  clean_names()

attach(stockData)

server = function(input, output, session) {
  output$rawDataTable <- DT::renderDataTable(DT::datatable(stockData) %>%
                                               formatCurrency("market_cap_in_m", "$") %>%
                                               formatStyle("g1", backgroundColor = "lightblue") %>%
                                               formatStyle("g2", backgroundColor = "lightblue") %>%
                                               formatStyle("g3", backgroundColor = "lightblue") %>%
                                               formatStyle("symbol", color = "grey")
                                             )
  
  plot_data <- reactive(
    # stockData %>%
    #   transmute(
    #     score = stockData$g1 * input$g1 + stockData$g2 * input$g2 + stockData$g3 * input$g3
    #   )
    
    cbind(stockData,
          score = stockData$g1 * input$g1 + stockData$g2 * input$g2 + stockData$g3 * input$g3)
  )
  
  output$plot <- renderPlot({
    ggplot(plot_data(), aes(score, market_cap_in_m)) +
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("Your Weighted Score") +
      ylab("Market Capitalization in Million USD")
  })
  
  brushData <- reactive({
    user_brush <- input$user_brush
    sel <- brushedPoints(plot_data(), user_brush)
    return(sel)
  })
  
  output$brushTable <- DT::renderDataTable(DT::datatable(brushData()))
  
  output$download <- downloadHandler(
    filename = "selected_stocks.csv",
    content = function(file){
      write.csv(brushData(), file)
    }
  )
}

# user interface
ui = navbarPage(theme = shinytheme("flatly"),          
                "The Mining Stock Scale",

  tabPanel(("Adjust your Mining Stocks"),
         wellPanel(
           sliderInput(inputId = "g1",
                       label = "Weight on Grade 1",
                       value = 1, min = 1, max = 20, step = 0.5),
           sliderInput(inputId = "g2",
                       label = "Weight on Grade 2",
                       value = 1, min = 1, max = 20, step = 0.5),
           sliderInput(inputId = "g3",
                       label = "Weight on Grade 3",
                       value = 1, min = 1, max = 20, step = 0.5)
         ),
         wellPanel(
           plotOutput("plot", brush = "user_brush"),
           DT::dataTableOutput("brushTable"),
           downloadButton(outputId = "download", label = "Download Data in Table")
         )
  ),

tabPanel("Documentation",
         h3("Video from YouTube:"),
         h4("How to Invest in Mining: Proactive Investors presents a beginners guide"),
         tags$iframe(width="1200",
                     height="800",
                     src="https://www.youtube.com/embed/wEy9ROgwOmk",
                     frameborder="0",
                     allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                     allowfullscreen=NA)
),



tabPanel("Data Table with the Underlying Data",
         DT::dataTableOutput("rawDataTable"))

)

shinyApp(ui = ui, server = server) 
