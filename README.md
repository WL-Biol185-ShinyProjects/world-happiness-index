# world-happiness-index

Analyzing happiness measures throughout time across countries in the world


Tab 1: Map
Tab 2: Entire data set
Tab 3: Comparison plot between different predictors of happiness
Tab 4: Over time


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


