# world-happiness-index

Analyzing happiness measures throughout time across countries in the world

library(dplyr)
library(tidyverse)
library(readr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(psych)
library(yarrr)


Opening Page: Map (use sapply)
Tab 1: Entire data set
Tab 2: Comparison plot between different predictors of happiness (life ladder (y-axis) vs every other variable (x-axis); this is a scatter plot where each dot is a country - have to use brush tool for this - and we will have a slider to choose the year))
Tab 3: Life ladder and all the indicators (y-axis) over time (x-axis) (drop down where we can choose specific countries and have a simple line plot)
Tab 4: 
Tab 5: 

whr <- read.csv("world-happiness-report.csv")

whr <- whr %>%
  select(1:7)

whr2020 <- whr %>%
  pivot_wider(names_from = "year", values_from = "Country.name") %>%
  select(1:5, "2020")

whr2020 <- whr %>%
  filter(year == 2020) %>%
  filter(!is.na(Log.GDP.per.capita)) %>%
  filter(!is.na(Healthy.life.expectancy.at.birth))
  

  

# myTable$newColumn <- gsub(r"([ab]d)", ".", myTable$column)
## use format above for changing country names 


