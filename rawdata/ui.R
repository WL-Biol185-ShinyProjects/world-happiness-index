



library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(DT)

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


fluidPage(
  titlePanel("World Happiness Index Raw Data"),
  mainPanel(p("This table depicts the raw data table of the World Happiness Index that we obtained, which includes the country name, year, happiness, and all the happiness predictors.")),
  basicPage(
    DT::dataTableOutput("mytable")))
