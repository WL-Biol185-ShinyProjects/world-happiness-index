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

server <- function(input, output) {
  output$WWRegressionPlot <- renderPlot({
    regDATA %>%
      ggplot(aes(x = Year, y = RegCoef, group = Predictor)) +
      geom_line(aes(color = Predictor)) +
      geom_point(aes(color = Predictor)) +
      labs(title = "Correlation of Predictors and Happiness Over Time", x = "Year", y = "Regression Coefficient") + 
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme_classic()
  })
}

