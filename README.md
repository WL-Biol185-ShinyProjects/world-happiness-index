# world-happiness-index

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
