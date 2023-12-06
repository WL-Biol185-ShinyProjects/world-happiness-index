library(shiny)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(ggplot2)

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


ui <- fluidPage(
  titlePanel("Strength of Variables in Predicting Happiness Over Time"),
  setBackgroundColor("aliceblue"),
  
  mainPanel(p("This line graph presents worldwide regression data over time. 
              To attain this data, multiple regressions were run between Happiness scores and predictor variables (GPD, LE, Freedom, Support) across all countries, for each year included in the study. 
              Regression coefficients were then standardized to account for differences in measurement scale of the predictor variables. 
              The resulting regression coefficients indicate how strongly each of the predictor variables correspond with the Happiness outcome.
              Larger regression coefficients indicate that the preictor variable had a greater influence on the Happiness score.
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
              
  plotOutput("RegressionPlots"))))



