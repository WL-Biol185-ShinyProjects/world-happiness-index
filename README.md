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

Tab 1: Entire data set; basically a beautified chart --> make interactive so people can search for a country, select highest/lowest; find particular year,...etc.

Tab 2: Bar graph of happiness rating (life ladder) as a function of country. Life ladder scores on y-axis and country names on x-axis. Drop down for each year --> shows happiest/least happy countries in each year.

Tab 3: Scatter plots (x4) of life ladder (y-axis) as a function of predictors (x-axis; 1 predictor/plot). All 4 plots together on one screen, with slider to change year. Each dot in scatter plot is a country -- will use brush tool to see which country each represents --> shows worldwide trends of how different predictors correlate with happiness over time

Tab 4: Simple line plot of life ladder (y-axis) as a function of time (x-axis). Drop down to select each country --> shows how happiness in each country has changed over time.

Tab 5: Averaged regression plot; average life ladder and each predictor across all countries for each year. Run multiple regression. Plot slope of regression (y-axis) as a function of time; slope signifies how closely each predictor correlates with the life ladder score. Shows changes over time in which predictors most strongly influence happiness worldwide.

Tab 6: Country-specific regression plot; Run multiple regression for each country. Plot slope of regression (y-axis) as a function of time; drop down menu to select country. Shows changes in how strongly each predictor correlates with happiness, over time, for each country

Tab 7: Chloropleth maps for happiness and each indicator

Finish going over navbar and tabs. Make sure format is correct and the information is correct and present. For example, have the map take up the full page. Finish aesthetics. 

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


