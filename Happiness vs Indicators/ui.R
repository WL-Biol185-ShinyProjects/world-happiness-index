library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

whrDATA <- read.csv("whrDATA.csv")

fluidPage(
  setBackgroundColor("aliceblue"),
  titlePanel("Happiness vs Different Indicators"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectyear", "Year:",
              choices=unique(whrDATA$Year)),
  hr(),
  helpText("Available Years within the Dataset"),
  selectInput("selectX", "X:",
              choices=colnames(select(whrDATA, 4:7)))
  ),
  
  mainPanel (
  p("Please explore this interactive graph to discover the relationship between a country's happiness and different indicators. 
             You can alter the specific x-axis indicator to see how this impacts a country's happiness level as well as choose a specific year of interest. 
             Drag a box over each point(s) to display its corresponding data.", style = 'times'),
             plotOutput("HappinessvsGDP",
              brush = brushOpts(
                id = "plot_brush",
                        )), 
  tableOutput("plot_brushinfo"),



)
)
)

  