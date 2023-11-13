


library(tidyverse)
library(psych)
library(yarrr)
library(ggplot2)


whr <- read.csv("world-happiness-report.csv")

whr <- whr %>%
  select(1:7)

whrDATA <- read.csv("world-happiness-report.csv")

whrDATA <- whrDATA %>%
  select(1:7) %>%
  rename(Country = Country.name,
         Year = year,
         Happiness = Life.Ladder,
         GDP = Log.GDP.per.capita,
         Support = Social.support,
         LE = Healthy.life.expectancy.at.birth,
         Freedom = Freedom.to.make.life.choices)

whr <- whr %>%
  select(1:7)

whr2020 <- whr %>%
  pivot_wider(names_from = "year", values_from = "Country.name") %>%
  select(1:5, "2020")

whr2020 <- whr %>%
  filter(year == 2020) %>%
  filter(!is.na(Log.GDP.per.capita)) %>%
  filter(!is.na(Healthy.life.expectancy.at.birth))


library(ggplot2)
HappinessvsGDP <- ggplot(whrDATA, aes(GDP, Happiness)) + geom_point() + ggtitle("Happiness vs GDP")
cols <- names(whrDATA)


print ()
Control shift c (to turn off the line)
Debug 


  
  Fluid row (p and pass the string the through it
    columns= x columns )
  
  ), 
verbatimTextOutput("plot_hoverinfo")


output$plot_hoverinfo <- renderText({
  hover <- input$plot_hover
  row <- nearPoints(whrDATA, hover)
  row$Country
  
  Ui
  hover = hoverOpts(
    id = "plot_hover",
  )), 
verbatimTextOutput("plot_hoverinfo"),
)
)
)


Server
output$plot_hoverinfo <- renderText({
  hover <- input$plot_hover
  row <- nearPoints(whrDATA, hover)
  row$Country
  
})
}


read.csv(../) then put in folder name after /