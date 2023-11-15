


library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(RColorBrewer)


function(input, output, session) {

  output$HappinessvsCountryPlot <- renderPlot({
    whrDATA %>%
      filter(Year == input$selectYear) %>%
      ggplot(aes(Country, Happiness)) + geom_bar(stat = "identity", col="gray24", fill = "cornflowerblue", position = position_dodge(0.92), alpha = 0.5) +
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
    df <- brushedPoints(whrDATA, brush) %>%
      filter(Year == input$selectyear)
    df[c("Country", "Year", "Happiness", input$selectX)]
    })
  
  output$HappinessvsTime <- renderPlot ({
    whrDATA %>%
      filter(Country == input$selectcountry) %>%
      ggplot(aes(Year, Happiness)) + geom_line(color ="blue") + ggtitle("Happiness vs Time")
  })
  
  lats <- -90:90
  lons <- -180:180
  
  output$worldmapHap <- renderLeaflet({
    
    leaflet(geo) %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolygons(fillColor = ~pal1(Happiness),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal1, values = ~Happiness, opacity = 0.7, title = NULL,
                position = "bottomright")
    
  })
  
  lats <- -90:90
  lons <- -180:180
  
  output$worldmapGDP <- renderLeaflet({
    
    leaflet(geo) %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolygons(fillColor = ~pal2(GDP),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal2, values = ~GDP, opacity = 0.7, title = NULL,
                position = "bottomright")
    
  })
  
  lats <- -90:90
  lons <- -180:180
  
  output$worldmapSup <- renderLeaflet({
    
    leaflet(geo) %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolygons(fillColor = ~pal3(Support),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal3, values = ~Support, opacity = 0.7, title = NULL,
                position = "bottomright")
    
    
  })
  
  lats <- -90:90
  lons <- -180:180
  
  output$worldmapLE <- renderLeaflet({
    
    leaflet(geo) %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolygons(fillColor = ~pal4(LE),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels4,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal4, values = ~LE, opacity = 0.7, title = NULL,
                position = "bottomright")
    
    
  })
  
  lats <- -90:90
  lons <- -180:180
  
  output$worldmapFree <- renderLeaflet({
    
    leaflet(geo) %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolygons(fillColor = ~pal5(Freedom),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels5,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal5, values = ~Freedom, opacity = 0.7, title = NULL,
                position = "bottomright")
    
    
  })
  
  
  output$WWRegressionPlot <- renderPlot({
    regDATA %>%
      ggplot(aes(x = Year, y = RegCoef, group = Predictor)) +
      geom_line(aes(color = Predictor)) +
      geom_point(aes(color = Predictor)) +
      labs(title = "Correlation of Predictors and Happiness Over Time", x = "Year", y = "Regression Coefficient")
  })
  
  }
  

