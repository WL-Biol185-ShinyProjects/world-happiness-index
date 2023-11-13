

library(shiny)
library(tidyverse)
library(leaflet)


function(input, output, session) {
  
  lats <- -90:90
  lons <- -180:180

  output$worldMap <- renderLeaflet({
    
    leaflet(data = llworld2) %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addMarkers(popup = ~newcol,  clusterOptions = markerClusterOptions())
    
  })

}
