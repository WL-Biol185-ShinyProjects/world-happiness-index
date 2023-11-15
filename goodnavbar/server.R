


library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(RColorBrewer)

world <- read.csv("world-happiness-report.csv")

geo <- geojson_read("countries.geo.json", what = "sp")

whrDATA <- world %>%
  select(1:7) %>%
  rename(Country = Country.name,
         Year = year,
         Happiness = Life.Ladder,
         GDP = Log.GDP.per.capita,
         Support = Social.support,
         LE = Healthy.life.expectancy.at.birth,
         Freedom = Freedom.to.make.life.choices)

whr2020 <- whrDATA %>%
  filter(Year == 2020) %>%
  filter(!is.na(GDP)) %>%
  filter(!is.na(LE)) %>%
  filter(!is.na(Freedom))

hap <- whr2020 %>%
  select(1, 3)

hap[61, 1] <- "Macedonia"
hap[68, 1] <- "Republic of Serbia"
hap[76, 1] <- "United Republic of Tanzania"
hap[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, hap, by = c("name" = "Country"))

pal1 <- colorNumeric("Set1", domain = c(0, 10))

labels1 <- sprintf(
  "<strong>%s</strong><br/>%s = Happiness", geo@data$name, geo@data$Happiness) %>% 
  lapply(htmltools::HTML)

gdp <- whr2020 %>%
  select(1, 4)

gdp[61, 1] <- "Macedonia"
gdp[68, 1] <- "Republic of Serbia"
gdp[76, 1] <- "United Republic of Tanzania"
gdp[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, gdp, by = c("name" = "Country"))

pal2 <- colorNumeric("Accent", domain = c(0, 12))

labels2 <- sprintf(
  "<strong>%s</strong><br/>%s = GDP", geo@data$name, geo@data$GDP) %>% 
  lapply(htmltools::HTML)

supp <- whr2020 %>%
  select(1, 5)

supp[61, 1] <- "Macedonia"
supp[68, 1] <- "Republic of Serbia"
supp[76, 1] <- "United Republic of Tanzania"
supp[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, supp, by = c("name" = "Country"))

pal3 <- colorNumeric("Paired", domain = c(0, 1))

labels3 <- sprintf(
  "<strong>%s</strong><br/>%s = Support", geo@data$name, geo@data$Support) %>% 
  lapply(htmltools::HTML)

le <- whr2020 %>%
  select(1, 6)

le[61, 1] <- "Macedonia"
le[68, 1] <- "Republic of Serbia"
le[76, 1] <- "United Republic of Tanzania"
le[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, le, by = c("name" = "Country"))

pal4 <- colorNumeric("Set2", domain = c(0, 100))

labels4 <- sprintf(
  "<strong>%s</strong><br/>%s = LE", geo@data$name, geo@data$LE) %>% 
  lapply(htmltools::HTML)

free <- whr2020 %>%
  select(1, 7)

free[61, 1] <- "Macedonia"
free[68, 1] <- "Republic of Serbia"
free[76, 1] <- "United Republic of Tanzania"
free[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, free, by = c("name" = "Country"))

pal5 <- colorNumeric("Spectral", domain = c(0, 1))

labels5 <- sprintf(
  "<strong>%s</strong><br/>%s = Freedom", geo@data$name, geo@data$Freedom) %>% 
  lapply(htmltools::HTML)


regDATA <- read.csv("overtime.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  arrange(Year, Predictor, RegCoef)

lnglat <- read.csv("country-capital-lat-long-population.csv") %>%
  select(Country, Longitude, Latitude)

lnglat[25, 1] <- "Bolivia"
lnglat[54, 1] <- "Ivory Coast"
lnglat[59, 1] <- "Czech Republic"
lnglat[116, 1] <- "Laos"
lnglat[169, 1] <- "South Korea"
lnglat[170, 1] <- "Moldova"
lnglat[173, 1] <- "Russia"
lnglat[206, 1] <- "North Macedonia"
lnglat[222, 1] <- "Tanzania"
lnglat[223, 1] <- "United States"

llworld <- left_join(whr2020, lnglat)

flag <- read.csv("flags_iso.csv")

flag <- flag %>%
  select(1, 4)

flag[21, 1] <- "Bolivia"
flag[46, 1] <- "Czech Republic"
flag[50, 1] <- "Dominican Republic"
flag[42, 1] <- "Ivory Coast"
flag[92, 1] <- "South Korea"
flag[95, 1] <- "Laos"
flag[115, 1] <- "Moldova"
flag[125, 1] <- "Netherlands"
flag[139, 1] <- "Philippines"
flag[143, 1] <- "North Macedonia"
flag[145, 1] <- "Russia"
flag[173, 1] <- "Tanzania"
flag[185, 1] <- "United Arab Emirates"
flag[186, 1] <- "United Kingdom"
flag[187, 1] <- "United States"


worldflag <- left_join(llworld, flag)

llworld <- worldflag

newcol <- paste0("<p><b><em>Country</b></em>", "=", llworld$Country,
                 "<p><b><em>Happiness</b></em>", "=", llworld$Happiness,
                 "<p><b><em>GDP</b></em>", "=", llworld$GDP,
                 "<p><b><em>Support</b></em>", "=", llworld$Support,
                 "<p><b><em>LE</b></em>", "=", llworld$LE,
                 "<p><b><em>Freedom</b></em>", "=", llworld$Freedom,
                 '<p><img src="', llworld$URL, '"></img></p>')

newcol <- as.data.frame(newcol)

llworld2 <- bind_cols(llworld, newcol)

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
  

