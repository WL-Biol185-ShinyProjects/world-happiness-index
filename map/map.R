
library(leaflet)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(psych)
library(yarrr)





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




leaflet()

leaflet() %>%
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
  setView(lng = 0, lat = 0, zoom = 2)


leaflet(data = llworld) %>%
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addMarkers(popup = ~Country)

leaflet(data = llworld) %>%
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addMarkers(popup = ~Country,  clusterOptions = markerClusterOptions())

newcol <- paste0("<p><b><em>Country</b></em>", "    =    ", llworld$Country,
              "<p><b><em>Happiness</b></em>", "    =    ", llworld$Happiness,
              "<p><b><em>GDP</b></em>", "    =    ", llworld$GDP,
              "<p><b><em>Support</b></em>", "    =    ", llworld$Support,
              "<p><b><em>LE</b></em>", "    =    ", llworld$LE,
              "<p><b><em>Freedom</b></em>", "    =    ", llworld$Freedom,
              '<p><img src="', llworld$URL, '"></img></p>')

newcol <- as.data.frame(newcol)

llworld2 <- bind_cols(llworld, newcol)

leaflet(data = llworld2) %>%
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addMarkers(popup = ~newcol,  clusterOptions = markerClusterOptions())











