library(tidyverse)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)

whrDATA <- read.csv("whrDATA.csv")

server <- function(input, output) {
  output$HappinessvsCountryPlot <- renderPlot({
    whrDATA %>%
      filter(Year == input$selectYear) %>%
      ggplot(aes(Country, Happiness)) + geom_bar(stat = "identity", col="lightblue1", position = position_dodge(0.92), alpha = 0.5) +
      labs(title = "Happiness by Country", x = "Country", y = "Happiness Index Value") +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme_classic()
  })
}
