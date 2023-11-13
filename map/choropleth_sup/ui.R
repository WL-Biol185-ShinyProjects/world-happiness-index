



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

fluidPage(
  titlePanel("Support Choropleth"),
  mainPanel(
    p("The description paragraph"),
    leafletOutput("worldMap"))
)

