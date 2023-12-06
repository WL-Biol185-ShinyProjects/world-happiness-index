

library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(geojsonio)
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
         Freedom = Freedom.to.make.life.choices) %>%
  arrange(Year, Country, Happiness, GDP, Support, LE, Freedom)

write.csv(whrDATA, "whrDATA.csv", row.names = FALSE)

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


regASIA <- read.csv("asia.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  arrange(Year, Predictor, RegCoef)



regEUROPE <- read.csv("europe.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  arrange(Year, Predictor, RegCoef)


regAFRICA <- read.csv("africa.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  arrange(Year, Predictor, RegCoef)



regAMERICAS <- read.csv("americas.csv") %>%
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


navbarPage("World Happiness Report",
           tabPanel("Introduction",
                    titlePanel("World Happiness Report: An Introduction"),
                    setBackgroundColor("aliceblue"),
                    mainPanel(p("This app explores the happiness index data across the world and throughout time (2005-2020) published by the World Gallup Poll. The data features different countries along with their happiness levels as predicted by 4 different variables. These 4 variables include: GDP, Social Support, Life Expectancy, and Freedom.
              GDP data was collected from a variety of economic sources, Life Expectancy data was collected from the World Health Organization (WHO), and all other values were self-reported and used to compile national averages."),
                              
                              p(strong("Happiness"), "is measured on a scale of 0-10, where 0 represents the worst possible life for you, and 10 represents the best possible life for you."),
                              
                              p(strong("GDP"), "stands for Gross Domestic Product. It measures the monetary value of a country's goods and services. Wealthier countries tend to exhibit higher GDP values."),
                              
                              p(strong("Social Support"), "represents a person's percieved levels of social support. Social Support is measured on a scale of 0-1, where 0 represents the feeling of not having anyone to count on during times of trouble, and 1 represents the sense of having this support."),
                              
                              p(strong("Life Expectancy (LE)"), "is the measure of the average life span of a country's population (in years)."),
                              
                              p(strong("Freedom"), "represents the perception of one's ability to make autonomous life choices. It is measured on a scale of 0-1, where a 0 represents dissatisfaction with one's freedom to make life choices, and a 1 represents satisfaction with one's autonomy."),
                              titlePanel("How have worldwide Happiness values changed over the years, since 2005?"), 
                              p("This is a bar graph demonstrating the happiness index value of each country. You may choose a specific year to evaluate the happiness index scores for countries in that year. By choosing different years, you may then visualize the trends of happiness scores as time changes. Note that not all countries have data for each year included in the study."),
                              selectInput("selectYear", 
                                          "Year", 
                                          choices = unique(whrDATA$Year)),
                              plotOutput("HappinessvsCountryPlot", width = ("250vh")))),
           
           
           tabPanel("Map",
                    titlePanel("Map of World Happiness Indicies in 2020"),
                    fluidPage(
                      p("This map displays a synopsis of each country's data. Data from 2020 was used as this was the most recent year included in this study. The pop-ups, placed at the capital city of each country, feature 7 different pieces of information: the country name, its happiness index, GDP, perception of social support, life expectency, perception of freedom, and lastly, the country's flag."),
                      leafletOutput("worldMap", height = ("80vh")))),
           
           
           tabPanel("Raw Data Table",
                    titlePanel("World Happiness Index Raw Data"),
                    setBackgroundColor("aliceblue"),
                    downloadButton("downloadData", "Download"),
                    mainPanel(p("This table depicts the complete dataset used for our project. To compile this dataset, the raw World Happiness Index data was manipulated 
                                to exclude variable not considered in our research project. In this interactive table, you can search for a particular country, year, or value and also rearrange the data based on variable of interest.
                                You also have the option to download this dataset for further personal analysis.")),
                    basicPage(
                      DT::dataTableOutput("mytable"))),
           
           
           tabPanel("Happiness vs Indicators",
                    titlePanel("What is the relationship between individual predicators and Happiness in a given year?"),
                    setBackgroundColor("aliceblue"),
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("selectyear", "Year:",
                                    min = 2005, max = 2020,
                                    value = 1, step = 1,
                                    animate =
                                      animationOptions(interval = 1000, loop = TRUE)),
                        hr(),
                        helpText("Available Years within the Dataset"),
                        selectInput("selectX", "X:",
                                    choices=colnames(select(whrDATA, 4:7)))),
                      mainPanel (
                        p("This interactive graph explores the relationship between a country's Happiness score and the different predictors. 
             You can choose the specific indicator (x-axis) to see how it compares to Happiness values (y-axis). You may also select a specific year to see how the predictor related to Happiness in that year. 
             Drag a box over each point(s) to display the specific country represeted by the point, as well as its corresponding data.", style = 'times'),
                        
                        plotOutput("HappinessvsGDP",
                                   height = ("60vh"),
                                   brush = brushOpts(
                                     id = "plot_brush",
                                   )), 
                        tableOutput("plot_brushinfo")))),
           
           
           tabPanel("Happiness over Time",
                    setBackgroundColor("aliceblue"),
                    titlePanel("How have Happiness scores changed for each country over time?"),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("selectcountry", "Choose a Country:", 
                                    multiple = TRUE, 
                                    choices=unique(whrDATA$Country)),
                        hr(),
                      ),
                      mainPanel (
                        p("This line graph demonstrates how Happiness values (y-axis) have changed over time, in years (x-axis), for an individual country. Please choose a country from the dropdown to visualize how its Happiness score fluctuates over time.", 
                          style = 'times'),
                        plotOutput("HappinessvsTime", height = ("80vh"))))),
           
           
           
           navbarMenu("Choropleth Maps",
                      
                      tabPanel("Happiness Choropleth",
                               titlePanel("Happiness Choropleth Map"),
                               setBackgroundColor("aliceblue"),
                               mainPanel(p("This map utilizes color to display and compare the Happiness scores across countries. As a reminder, Happiness is measured on a scale of 0-10, where 0 represents the worst possible life and a 10 represents the best possible life. 
                                           The key on the bottom right shows the Happiness score associated with each color. Countries that have missing data will appear gray. Hover over each country to see its exact Happiness value.")),
                               leafletOutput("worldmapHap", height = ("80vh"))),
    
                      tabPanel("GDP Choropleth",
                               titlePanel("GDP Choropleth Map"),
                               setBackgroundColor("aliceblue"),
                               mainPanel(p("This map utilizes color to display and compare GDP values across countries. GDP measures the monetary value of a country's goods and services. 
                                           The key on the bottom right shows the GDP values associated with each color. Countries that have missing data will appear gray. Hover over each country to see its exact GDP value.")),
                               leafletOutput("worldmapGDP", height = ("80vh"))),
                      
                      tabPanel("Socal Support Choropleth",
                               titlePanel("Social Support Choropleth Map"),
                               setBackgroundColor("aliceblue"),
                               mainPanel(p("This map utilizes color to display and compare Support scores across countries. Support represents perceived levels of social support. It is measured on a scale from 0-1, where 0 represents the feeling
                                           of not having anyone to count on during times of trouble, and 1 represents the sense of having this support. 
                                           The key on the bottom right shows the Support values associated with each color. Countries that have missing data will appear gray. Hover over each country to see its exact Support score.")),
                               leafletOutput("worldmapSup", height = ("80vh"))),
                      
                      tabPanel("Life Expectancy Choropleth",
                               titlePanel("Life Expectancy Choropleth Map"),
                               setBackgroundColor("aliceblue"),
                               mainPanel(p("This map utilizes color to display and compare Life Expectancy (LE) values across countries. LE is the measure of the average life span of a country's population (in years). 
                                           The key on the bottom right shows the LE values associated with each color. Countries that have missing data will appear gray. Hover over each country to see its exact LE value.")),
                               leafletOutput("worldmapLE", height = ("80vh"))),
                      
                      tabPanel("Freedom Choropleth",
                               titlePanel("Freedom Choropleth Map"),
                               setBackgroundColor("aliceblue"),
                               mainPanel(p("This map utilizes color to display and compare Freedom scores across countries. Freedom represents the perception of one's ability to make autonomous life choices. It is measured on a scale of 0-1,
                                           where 0 represents dissatisfaction with one's freedom to make life choices and 1 represents satisfaction with one's autonomy. 
                                           The key on the bottom right shows the Freedom scores associated with each color. Countries that have missing data will appear gray. Hover over each country to see its exact Freedom score.")),
                               leafletOutput("worldmapFree", height = ("80vh")))),
           
           
           tabPanel("Regression Plot",
                    titlePanel("How does the strength of each variable in predicting Happiness change over time?"),
                    setBackgroundColor("aliceblue"),
                    mainPanel(p("This line graph presents worldwide regression data over time. 
                                To attain this data, multiple regressions were run between Happiness scores and predictor variables (GPD, LE, Freedom, Support) across all countries, for each year included in the study.
                                Regression coefficients were then standardized to account for differences in scales of measurement of the predictor variables.
                                The resulting regression coefficients indicate how strongly each of the predictor variables correspond with the Happiness outcome.
                                Larger regression coefficients indicate that the predictor variable had a greater influence on the Happiness score.
                                Negative regression coefficients suggest that the predictor is negatively correlated with Happiness -- e.g., as the predictor increases, Happiness decreases."),
                              fluidRow(
                                selectInput("selectRegion",
                                            "Region",
                                            choices = c("Worldwide",
                                                        "Africa",
                                                        "Americas",
                                                        "Asia",
                                                        "Europe"), 
                                            width = "50%"),
                              plotOutput("WWRegressionPlot", height = ("80vh"))))),
           
           
           tabPanel("About",
                    titlePanel("The Origins of our Project!"),
                    setBackgroundColor("aliceblue"),
                    mainPanel(p(strong("Thank you for exploring our app! We hope you discovered more about the World Happiness Index and the factors that influence it. We chose this topic because we thought it would be interesting to analyze happiness trends across countries and time along with understanding why some countries tend to be happier than others. We hope to grow in our happiness and spread some positivity!")),
                              img(src = "Image.jpg", height = "500", width = "500"),
                              p(strong("Cami Fischmann, Zainab Madan, Emily Sansbury")),
                              a(href = "https://worldhappiness.report/data/", "Explore the World Happiness Report here!"))))
                             
                                


                      
           
           







           
           
           
        
             