


library(shiny)
library(tidyverse)
library(leaflet)


function(input, output, session) {
  
  lats <- -90:90
  lons <- -180:180
  
  output$worldMap <- renderLeaflet({
    
    leaflet(geo) %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolygons(fillColor = ~pal(Happiness),
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
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~Happiness, opacity = 0.7, title = NULL,
                position = "bottomright")
    
  })
  
}

