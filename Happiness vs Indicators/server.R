library(tidyverse)
library(shiny)

whrDATA <- read.csv("whrDATA.csv")

server <- function(input, output) {
  
  output$HappinessvsGDP <- renderPlot ({
    if (input$selectX == "GDP") {
        whrDATA %>%
          filter(Year == input$selectyear) %>%
            ggplot(aes(GDP, Happiness)) + geom_point(color ="blue") + ggtitle("Happiness vs GDP")
      }
    
    else if (input$selectX == "Support") {
      whrDATA %>%
        filter(Year == input$selectyear) %>%
        ggplot(aes(Support, Happiness)) + geom_point(color ="blue") + ggtitle("Happiness vs Support")
     }
      else if (input$selectX == "LE") {
        whrDATA %>%
          filter(Year == input$selectyear) %>%
          ggplot(aes(LE, Happiness)) + geom_point(color = "blue" ) + ggtitle("Happiness vs LE")
     }
    
    else if (input$selectX == "Freedom") {
      whrDATA %>%
        filter(Year == input$selectyear) %>%
        ggplot(aes(Freedom, Happiness)) + geom_point(color = "blue") + ggtitle("Happiness vs Freedom")
    }
  })
  
  output$plot_brushinfo <- renderTable({
    brush <- input$plot_brush
    df <- brushedPoints(whrDATA, brush) %>%
      filter(Year == input$selectyear)
    
    df[c("Country", "Year", "Happiness", input$selectX)]
  
  })
}

  

