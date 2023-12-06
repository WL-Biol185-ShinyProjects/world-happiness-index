library(tidyverse)
library(shiny)
library(shinyWidgets)
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


regLIST <- list("Worldwide" = regDATA,
                "Africa" = regAFRICA,
                "Americas" = regAMERICAS,
                "Asia" = regASIA,
                "Europe" = regEUROPE)


server <- function(input, output) {
  
  output$RegressionPlots <- renderPlot({
    
    if (input$selectRegion == "Worldwide") {
      regDATA %>%
        ggplot(aes(x = Year, y = RegCoef, group = Predictor)) +
        geom_line(aes(color = Predictor)) +
        geom_point(aes(color = Predictor)) +
        labs(title = "Correlation of Predictors and Happiness Over Time (Worldwide)", x = "Year", y = "Regression Coefficient")
    }
    
    else if (input$selectRegion == "Asia") {
      regASIA %>%
        ggplot(aes(x = Year, y = RegCoef, group = Predictor)) +
        geom_line(aes(color = Predictor)) +
        geom_point(aes(color = Predictor)) +
        labs(title = "Correlation of Predictors and Happiness Over Time (Asia)", x = "Year", y = "Regression Coefficient")
    }
    
    else if (input$selectRegion == "Europe") {
      regEUROPE %>%
        ggplot(aes(x = Year, y = RegCoef, group = Predictor)) +
        geom_line(aes(color = Predictor)) +
        geom_point(aes(color = Predictor)) +
        labs(title = "Correlation of Predictors and Happiness Over Time (Europe)", x = "Year", y = "Regression Coefficient")
    }
    
    else if (input$selectRegion == "Africa") {
      regAFRICA %>%
        ggplot(aes(x = Year, y = RegCoef, group = Predictor)) +
        geom_line(aes(color = Predictor)) +
        geom_point(aes(color = Predictor)) +
        labs(title = "Correlation of Predictors and Happiness Over Time (Africa)", x = "Year", y = "Regression Coefficient")
    }
    
    else if (input$selectRegion == "Americas") {
      regAMERICAS %>%
        ggplot(aes(x = Year, y = RegCoef, group = Predictor)) +
        geom_line(aes(color = Predictor)) +
        geom_point(aes(color = Predictor)) +
        labs(title = "Correlation of Predictors and Happiness Over Time (Americas)", x = "Year", y = "Regression Coefficient")
    }

  })
}

