

library(leaflet)
library(shiny)

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
fluidPage(
  titlePanel("Life Expectancy Choropleth"),
  mainPanel(
    p("The description paragraph"),
    leafletOutput("worldMap"))
)
