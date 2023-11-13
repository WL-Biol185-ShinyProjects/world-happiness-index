library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)

whrDATA <- read.csv("whrDATA.csv")

server <- function(input, output) {
  output$HappinessvsTime <- renderPlot ({
      whrDATA %>%
        filter(Country == input$selectcountry) %>%
        ggplot(aes(Year, Happiness)) + geom_line(color ="blue") + ggtitle("Happiness vs Time")
    })
}
  