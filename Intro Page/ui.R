library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)

whrDATA <- read.csv("whrDATA.csv")

ui <- fluidPage(
  titlePanel("World Happiness Report: An Introduction"),
  mainPanel(p("This app explores the happiness index data across the world and throughout time (2005-2020) published by the World Gallup Poll. The data features different countries along with their happiness levels as predicted by 4 different variables. These 4 variables include: GDP, Social Support, Life Expectancy, and Freedom.
              GDP data was collected from a variety of economic sources, Life Expectancy data was collected from the World Health Organization (WHO), and all other values were self-reported and used to compile national averages."),
          
            p("Happiness is measured on a scale of 0-10, where a 0 represents the worst possible life for you, and a 10 represents the best possible life for you."),
            
            p("GDP stands for Gross Domestic Product. It measures the monetary value of a country's goods and services. Wealthier countries tend to exhibit higher GDP values."),
            
            p("Support represents the percieved levels of social support. Support of measured on a scale of 0-1, where a 0 represents the feeling of not having anyone to count on during times of trouble, and a 1 represents the sense of having this support."),
            
            p("Life Expectancy (LE) is the measure of the average life span of a country's population (in years)."),
            
            p("Freedom represents the perception of one's ability to make autonomous life choices. It is measured on a scale of 0-1, where a 0 represents dissatisfaction with one's freedom to make life choices, and a 1 represents satisfaction with one's autonomy."),
  
  titlePanel("Plot of Happiness by Country Over Time"),          
  selectInput("selectYear", 
              "Year", 
              choices = unique(whrDATA$Year)),
  plotOutput("HappinessvsCountryPlot")))


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

shinyApp(ui, server)

