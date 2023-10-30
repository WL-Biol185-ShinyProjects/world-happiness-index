




library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(DT)



function(input, output, session) {

  output$HappinessvsCountryPlot <- renderPlot({
    whrDATA %>%
      filter(Year == input$selectYear) %>%
      ggplot(aes(Country, Happiness)) + geom_bar(stat = "identity", col="lightblue1", position = position_dodge(0.92), alpha = 0.5) +
      labs(title = "Happiness by Country", x = "Country", y = "Happiness Index Value") +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme_classic()
  })
  
  lats <- -90:90
  lons <- -180:180
  
  output$worldMap <- renderLeaflet({
    
    leaflet(data = llworld2) %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addMarkers(popup = ~newcol,  clusterOptions = markerClusterOptions())
  
    })
  
  output$mytable = DT::renderDataTable({
    whrDATA
  
    })
  
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
    brushedPoints(whrDATA, brush) %>%
      filter(Year == input$selectyear)
    
  })
  
}
