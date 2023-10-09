library(shiny)
library(nycflights13)
library(tidyverse)

ui <- fluidPage(
  titlePanel("NYC Flights"),
  selectInput("selectAirport", 
              "Airports", 
              choices = unique(flights$origin)),
  sliderInput("xAxisRange", "Range of Delays", 0, max(flights$dep_time, na.rm = TRUE), 500),
  plotOutput("departureDelayPlot"))
#titlePanel gives app title; use ?selectInput and ?selectOutput in console for help figuring out what to code in these lines
#done with ui; ui sets up where stuff is on page with relation to one another

server <- function(input, output) {
  output$departureDelayPlot <- renderPlot({
    flights %>%
      filter(origin == input$selectAirport) %>%
      ggplot(aes(dep_delay)) + geom_histogram() +
      xlim(c(0, input$xAxisRange))
  })
}

shinyApp(ui, server)
