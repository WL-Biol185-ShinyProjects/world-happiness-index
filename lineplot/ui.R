library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

whrDATA <- read.csv("whrDATA.csv")

fluidPage(
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
 
)