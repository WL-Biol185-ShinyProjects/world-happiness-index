

library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(DT)



function(input, output) {
  output$mytable = DT::renderDataTable({
    whrDATA
  })
}

