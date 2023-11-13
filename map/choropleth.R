


library(leaflet)
library(geojsonio)
library(tidyverse)
library(htmltools)
library(RColorBrewer)

## Data

geo <- geojson_read("countries.geo.json", what = "sp")

world <- read.csv("world-happiness-report.csv")

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

## Happiness

hap <- whr2020 %>%
  select(1, 3)

hap[61, 1] <- "Macedonia"
hap[68, 1] <- "Republic of Serbia"
hap[76, 1] <- "United Republic of Tanzania"
hap[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, hap, by = c("name" = "Country"))

pal <- colorNumeric("Set1", domain = c(0, 10))

labels <- sprintf(
  "<strong>%s</strong><br/>%s = Happiness", geo@data$name, geo@data$Happiness) %>% 
  lapply(htmltools::HTML)

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






## GDP

gdp <- whr2020 %>%
  select(1, 4)

gdp[61, 1] <- "Macedonia"
gdp[68, 1] <- "Republic of Serbia"
gdp[76, 1] <- "United Republic of Tanzania"
gdp[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, gdp, by = c("name" = "Country"))

pal <- colorNumeric("Set3", domain = c(0, 12))

labels <- sprintf(
  "<strong>%s</strong><br/>%s = GDP", geo@data$name, geo@data$GDP) %>% 
  lapply(htmltools::HTML)

leaflet(geo) %>%
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addPolygons(fillColor = ~pal(GDP),
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
  addLegend(pal = pal, values = ~GDP, opacity = 0.7, title = NULL,
            position = "bottomright")


## Support

supp <- whr2020 %>%
  select(1, 5)

supp[61, 1] <- "Macedonia"
supp[68, 1] <- "Republic of Serbia"
supp[76, 1] <- "United Republic of Tanzania"
supp[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, supp, by = c("name" = "Country"))

pal <- colorNumeric("Paired", domain = c(0, 1))

labels <- sprintf(
  "<strong>%s</strong><br/>%s = Support", geo@data$name, geo@data$Support) %>% 
  lapply(htmltools::HTML)

leaflet(geo) %>%
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addPolygons(fillColor = ~pal(Support),
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
  addLegend(pal = pal, values = ~Support, opacity = 0.7, title = NULL,
            position = "bottomright")




## LE

le <- whr2020 %>%
  select(1, 6)

le[61, 1] <- "Macedonia"
le[68, 1] <- "Republic of Serbia"
le[76, 1] <- "United Republic of Tanzania"
le[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, le, by = c("name" = "Country"))

pal <- colorNumeric("Accent", domain = c(0, 100))

labels <- sprintf(
  "<strong>%s</strong><br/>%s = LE", geo@data$name, geo@data$LE) %>% 
  lapply(htmltools::HTML)

leaflet(geo) %>%
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addPolygons(fillColor = ~pal(LE),
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
  addLegend(pal = pal, values = ~LE, opacity = 0.7, title = NULL,
            position = "bottomright")



## Freedom

free <- whr2020 %>%
  select(1, 7)

free[61, 1] <- "Macedonia"
free[68, 1] <- "Republic of Serbia"
free[76, 1] <- "United Republic of Tanzania"
free[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, free, by = c("name" = "Country"))

pal <- colorNumeric("Spectral", domain = c(0, 1))

labels <- sprintf(
  "<strong>%s</strong><br/>%s = Freedom", geo@data$name, geo@data$Freedom) %>% 
  lapply(htmltools::HTML)

leaflet(geo) %>%
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addPolygons(fillColor = ~pal(Freedom),
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
  addLegend(pal = pal, values = ~Freedom, opacity = 0.7, title = NULL,
            position = "bottomright")

