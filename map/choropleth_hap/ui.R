


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


fluidPage(
  titlePanel("Happiness Choropleth"),
  mainPanel(
    p("The description paragraph"),
    leafletOutput("worldMap"))
)

