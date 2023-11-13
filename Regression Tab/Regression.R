library(tidyverse)
library(apaTables)
library(table1)
library(ggpubr)
library(corrr)
library(lm.beta)

whrDATA <- read.csv("whrDATA.csv")


## CORRELATIONS CODE
whrCORRELS <- whrDATA %>%
  select(3:7)

table1(~., data = whrCORRELS)
correls <- correlate(whrCORRELS)
apa.cor.table(whrCORRELS, landscape = TRUE, filename = "corr table.doc")


## SIMPLE REGRESSIONS BY PREDICTOR
SUPsimplereg <- lm(Happiness ~ Support, data = whrDATA)  
summary(SUPsimplereg)
apa.reg.table(SUPsimplereg)

FREEsimplereg <- lm(Happiness ~ Freedom, data = whrDATA)  
summary(FREEsimplereg)
apa.reg.table(FREEsimplereg)

LEsimplereg <- lm(Happiness ~ LE, data = whrDATA)  
summary(LEsimplereg)
apa.reg.table(LEsimplereg)

GDPsimplereg <- lm(Happiness ~ GDP, data = whrDATA)  
summary(GDPsimplereg)
apa.reg.table(GDPsimplereg)


## MULTIPLE REGRESSION WITH ALL VARIABLES
multireg <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA)  
apa.reg.table(multireg, filename = "AllVar reg table.doc")
stdFit <- lm.beta(multireg)
stdFit


#multiple regression (mr) and standard fit (sf) for all countries in 2005
mr2005 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2005)
sf2005 <- lm.beta(mr2005)
sf2005

mr2006 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2006)
sf2006 <- lm.beta(mr2006)
sf2006

mr2007 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2007)
sf2007 <- lm.beta(mr2007)
sf2007

mr2008 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2008)
sf2008 <- lm.beta(mr2008)
sf2008


mr2009 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2009)
sf2009 <- lm.beta(mr2009)
sf2009

mr2010 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2010)
sf2010 <- lm.beta(mr2010)
sf2010

mr2011 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2011)
sf2011 <- lm.beta(mr2011)
sf2011

mr2012 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2012)
sf2012 <- lm.beta(mr2012)
sf2012

mr2013 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2013)
sf2013 <- lm.beta(mr2013)
sf2013

mr2014 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2014)
sf2014 <- lm.beta(mr2014)
sf2014

mr2015 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2015)
sf2015 <- lm.beta(mr2015)
sf2015

mr2016 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2016)
sf2016 <- lm.beta(mr2016)
sf2016

mr2017 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2017)
sf2017 <- lm.beta(mr2017)
sf2017

mr2018 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2018)
sf2018 <- lm.beta(mr2018)
sf2018

mr2019 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2019)
sf2019 <- lm.beta(mr2019)
sf2019

mr2020 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = whrDATA, subset = Year == 2020)
sf2020 <- lm.beta(mr2020)
sf2020


regDATA <- read.csv("overtime.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  #creates age group variable and sorts into old or young based
  arrange(Year, Predictor, RegCoef) #sorts data


ggplot(data=regDATA, aes(x = Year, y = RegCoef, group = Predictor)) +
  geom_line(aes(color = Predictor)) +
  geom_point(aes(color = Predictor)) +
  labs(title = "Correlation of Predictors and Happiness Over Time", x = "Year", y = "Regression Coefficient")


YearData <- whrDATA%>%
  group_by(Year) %>%
  summarise(avgLE = mean(LE, na.rm = TRUE),
            avgSUP = mean(Support, na.rm = TRUE),
            avgGDP = mean(GDP, na.rm = TRUE),
            avgFREE = mean(Freedom, na.rm = TRUE),
            avgHAP = mean(Happiness, na.rm = TRUE))

CountryData <- whrDATA%>%
  group_by(Country) %>%
  summarise(avgLE = mean(LE, na.rm = TRUE),
            avgSUP = mean(Support, na.rm = TRUE),
            avgGDP = mean(GDP, na.rm = TRUE),
            avgFREE = mean(Freedom, na.rm = TRUE),
            avgHAP = mean(Happiness, na.rm = TRUE))

