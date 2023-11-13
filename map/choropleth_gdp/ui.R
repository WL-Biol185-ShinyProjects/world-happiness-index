




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

fluidPage(
  titlePanel("GDP Choropleth"),
  mainPanel(
    p("The description paragraph"),
    leafletOutput("worldMap"))
)



