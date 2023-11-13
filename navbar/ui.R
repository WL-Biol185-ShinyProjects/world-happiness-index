

library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(geojsonio)
library(htmltools)
library(RColorBrewer)

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

regDATA <- read.csv("overtime.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  arrange(Year, Predictor, RegCoef)

geo <- geojson_read("countries.geo.json", what = "sp")

hap <- whr2020 %>%
  select(1, 3)

hap[61, 1] <- "Macedonia"
hap[68, 1] <- "Republic of Serbia"
hap[76, 1] <- "United Republic of Tanzania"
hap[84, 1] <- "United States of America"

geo@data <- left_join(geo@data, hap, by = c("name" = "Country"))

pal <- colorNumeric("Set1", domain = c(0, 10))

labels1 <- sprintf(
  "<strong>%s</strong><br/>%s = Happiness", geo@data$name, geo@data$Happiness) %>% 
  lapply(htmltools::HTML)

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


navbarPage("NavBar",
           tabPanel("Introduction",
                    titlePanel("World Happiness Report: An Introduction"),
                    setBackgroundColor("aliceblue"),
                    mainPanel(p("This app explores the happiness index data across the world and throughout time (2005-2020) published by the World Gallup Poll. The data features different countries along with their happiness levels as predicted by 4 different variables. These 4 variables include: GDP, Social Support, Life Expectancy, and Freedom.
              GDP data was collected from a variety of economic sources, Life Expectancy data was collected from the World Health Organization (WHO), and all other values were self-reported and used to compile national averages."),
                              
                              p(strong("Happiness"), "is measured on a scale of 0-10, where a 0 represents the worst possible life for you, and a 10 represents the best possible life for you."),
                              
                              p(strong("GDP"), "stands for Gross Domestic Product. It measures the monetary value of a country's goods and services. Wealthier countries tend to exhibit higher GDP values."),
                              
                              p(strong("Social Support"), "represents a person's percieved levels of social support. Social Support is measured on a scale from 0-1, where a 0 represents the feeling of not having anyone to count on during times of trouble, and a 1 represents the sense of having this support."),
                              
                              p(strong("Life Expectancy (LE)"), "is the measure of the average life span of a country's population (in years)."),
                              
                              p(strong("Freedom"), "represents the perception of one's ability to make autonomous life choices. It is measured on a scale of 0-1, where a 0 represents dissatisfaction with one's freedom to make life choices, and a 1 represents satisfaction with one's autonomy."),
                              titlePanel("Plot of Happiness by Country Over Time"), 
                              p("This is a bar graph demonstrating the happiness index value of each country. You may choose a specific year to evaluate the happiness index scores for countries in that year. By choosing different years, you may then visualize the trends of happiness scores as time changes."),
                              selectInput("selectYear", 
                                          "Year", 
                                          choices = unique(whrDATA$Year)),
                              plotOutput("HappinessvsCountryPlot"))),
           
           tabPanel("Map",
                    titlePanel("Map of World Happiness Indicies in 2020"),
                    fluidPage(
                      p("This map shows different countries around the world. 
      The pop-ups, which are placed at the capital city of each country, feature 7 different pieces of information per country: 
      the country name, 
      its happiness index, 
      GDP, 
      perception of social support, 
      life expectency, 
      perception of freedom, 
      and lastly, the country's flag."),
<<<<<<< HEAD
            leafletOutput("worldMap", height = ("100vh")))
                    ),
=======
           
                      leafletOutput("worldMap", height = ("100vh")))
                    ),

>>>>>>> ad536fd0d1d96e331181403f80042a0372945c8c
           tabPanel("Raw Data Table",
                    titlePanel("World Happiness Index Raw Data"),
                    setBackgroundColor("aliceblue"),
                    mainPanel(p("This table depicts the raw data table of the World Happiness Index which includes the country name, year, happiness score, and all the happiness predictors. You can search for a certain country, year, or value")),
                    basicPage(
                      DT::dataTableOutput("mytable"))),
           
           tabPanel("Happiness vs Indicators Plots", 
                    titlePanel("Happiness vs Different Indicators"),
                    setBackgroundColor("aliceblue"),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("selectyear", "Year:",
                                    choices=unique(whrDATA$Year)),
                        hr(),
                        helpText("Available Years within the Dataset"),
                        selectInput("selectX", "X:",
                                    choices=colnames(select(whrDATA, 4:7)))),
                      mainPanel (
                        p("Please explore this interactive graph to discover the relationship between a country's happiness score and the different predictors. 
             You can choose the specific indicator (x-axis) to see how this impacts a country's happiness level as well as choose a specific year of interest. 
             Drag a box over each point(s) to display the corresponding data.", style = 'times'),
                        
                        plotOutput("HappinessvsGDP",
                                   brush = brushOpts(
                                     id = "plot_brush",
                                   )), 
                        tableOutput("plot_brushinfo"))),
           ),
           
           tabPanel("Happiness over Time",
                    setBackgroundColor("aliceblue"),
                    titlePanel("Happiness Over Time By Country"),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("selectcountry", "Country:",
                                    choices=unique(whrDATA$Country)),
                        hr(),
                        helpText("Choose a Country"),
                      ),
                      mainPanel (
                        p("Please choose a country from the dropdown to visualize how its happiness score fluctuates over time.", 
                          style = 'times'),
                        plotOutput("HappinessvsTime")
                      )
                    )
                    
           ),
           
           navbarMenu("Chloropleth Graphs",
                      
           tabPanel("Happiness Chloropleth",
                     titlePanel("Happiness Chloropleth Map"),
                    fluidPage(
                      titlePanel("Happiness Choropleth"),
                      mainPanel(
                        p("This map utilizes color to display and compare the happiness scores for world countries. The darker the shade 
                                  of the color equates to a higher happiness score. Use the key on the side to compare the shade of color
                                  to its correspoding happiness number"),
                        leafletOutput("worldMap"))
                    ),
                      setBackgroundColor("aliceblue"),
                      
           ),
                    
                     tabPanel("GDP Chloropleth"
                      ),
                     tabPanel("Socal Support Cloropleth"
                      ),
                     tabPanel("Life Expectancy Chloropleth"
                      ),
                     tabPanel("Freedom Chloropleth")
           ),
           
           tabPanel("Regression Plot",
                    fluidPage(
                      fluidRow(
                        column(width = 12),
                    titlePanel("Strength of Variables in Predicting Happiness Over Time"),
                    setBackgroundColor("aliceblue"),
                    mainPanel(p("This line graph presents worldwide regression data over time. 
                                To attain this data, multiple regressions were run between Happiness scores and predictor variables (GPD, LE, Freedom, Support) across all countries, for each year included in the study.
                                Regression coefficients were then standardized to account for differences in measurement scale of the predictor variables.
                                The resulting regression coefficients indicate how strongly each of the predictor variables correspond with the Happiness outcome.
                                Larger regression coefficients indicate that the preictor variable had a greater influence on the Happiness score.
                                Negative regression coefficients suggest that the predictor is negatively correlated with Happiness -- e.g., as the predictor increases, Happiness decreases."),
                              plotOutput("WWRegressionPlot")))
                    )))

           
           
          

           


                             
### changes to push
  

           
           
           
        
             