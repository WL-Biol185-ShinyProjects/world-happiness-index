library(tidyverse)
library(apaTables)
library(table1)
library(ggpubr)
library(corrr)
library(lm.beta)
library(choroplethrMaps)
library(rworldmap)


whrDATA <- read.csv("whrDATA.csv")

allregions <- read.csv("regions.csv") %>%
  rename (Country = name) %>% 
  rename (Region = region) %>%
  rename (ABR = alpha.3) %>%
  select (Country, Region)

allregions[52, 1] <- "Congo (Kinshasa)"
allregions[51, 1] <- "Congo (Brazzaville)"
allregions[27, 1] <- "Bolivia"
allregions[60, 1] <- "Czech Republic"
allregions[101, 1] <- "Hong Kong S.A.R. of China"
allregions[106, 1] <- "Iran"
allregions[55, 1] <- "Ivory Coast"
allregions[123, 1] <- "Laos"
allregions[146, 1] <- "Moldova"
allregions[59, 1] <- "Cyprus"
allregions[171, 1] <- "Palestinian Territories"
allregions[184, 1] <- "Russia"
allregions[206, 1] <- "Somalia"
allregions[120, 1] <- "South Korea"
allregions[71, 1] <- "Swaziland"
allregions[217, 1] <- "Syria"
allregions[218, 1] <- "Taiwan Province of China"
allregions[220, 1] <- "Tanzania"
allregions[235, 1] <- "United Kingdom"
allregions[236, 1] <- "United States"
allregions[241, 1] <- "Venezuela"
allregions[242, 1] <- "Vietnam"

  

url<-("http://api.dhsprogram.com/rest/dhs/countries?f=json")

library(jsonlite) # for fromJSON
library(data.table) # for data.table
library(dplyr)

# read DHS API country list 
jsondata <- fromJSON(url) 
# create data frame with countries 
ctry_DHS <- data.table(jsondata$Data)
# tidy up
regions <- ctry_DHS %>%
  rename (Country =   CountryName) %>% 
  rename (DHSregion1  =   RegionName) %>%
  rename (DHSregion2  =   SubregionName) %>%
  select (Country, DHSregion1, DHSregion2)

whrREGIONS <- merge(whrDATA, regions, by = c("Country")) %>%
  select (!DHSregion1) %>%
  rename (Region = DHSregion2)

whrMERGE <- merge(whrDATA, allregions, by = c("Country")) 

whrJOIN <- left_join(whrDATA, allregions, by = c("Country"))

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
  labs(title = "Correlation of Predictors and Happiness Over Time (Worldwide)", x = "Year", y = "Regression Coefficient")


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



## ASIA ##
Asia <- whrMERGE %>%
  filter(Region == "Asia") %>%
  pivot_wider(names_from = "Year",
              values_from = "Happiness") %>%
  mutate(hapmean = mean(`2008`, na.rm = TRUE))
summarise(hapmean = mean(`2008`, na.rm = TRUE))
  #filter(Year == "2005")



Asia <- whrMERGE %>%
  filter(Region == "Asia")

AS2005 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2005)
ASs2005 <- lm.beta(AS2005)
ASs2005

AS2006 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2006)
ASs2006 <- lm.beta(AS2006)
ASs2006

AS2007 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2007)
ASs2007 <- lm.beta(AS2007)
ASs2007

AS2008 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2008)
ASs2008 <- lm.beta(AS2008)
ASs2008

AS2009 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2009)
ASs2009 <- lm.beta(AS2009)
ASs2009

AS2010 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2010)
ASs2010 <- lm.beta(AS2010)
ASs2010

AS2011 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2011)
ASs2011 <- lm.beta(AS2011)
ASs2011

AS2012 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2012)
ASs2012 <- lm.beta(AS2012)
ASs2012

AS2013 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2013)
ASs2013 <- lm.beta(AS2013)
ASs2013

AS2014 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2014)
ASs2014 <- lm.beta(AS2014)
ASs2014

AS2015 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2015)
ASs2015 <- lm.beta(AS2015)
ASs2015

AS2016 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2016)
ASs2016 <- lm.beta(AS2016)
ASs2016

AS2017 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2017)
ASs2017 <- lm.beta(AS2017)
ASs2017

AS2018 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2018)
ASs2018 <- lm.beta(AS2018)
ASs2018

AS2019 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2019)
ASs2019 <- lm.beta(AS2019)
ASs2019

AS2020 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Asia, subset = Year == 2020)
ASs2020 <- lm.beta(AS2020)
ASs2020





Europe <- whrMERGE %>%
  filter(Region == "Europe")

EU2005 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2005)
EUs2005 <- lm.beta(EU2005)
EUs2005

EU2006 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2006)
EUs2006 <- lm.beta(EU2006)
EUs2006

EU2007 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2007)
EUs2007 <- lm.beta(EU2007)
EUs2007

EU2008 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2008)
EUs2008 <- lm.beta(EU2008)
EUs2008

EU2009 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2009)
EUs2009 <- lm.beta(EU2009)
EUs2009

EU2010 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2010)
EUs2010 <- lm.beta(EU2010)
EUs2010

EU2011 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2011)
EUs2011 <- lm.beta(EU2011)
EUs2011

EU2012 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2012)
EUs2012 <- lm.beta(EU2012)
EUs2012

EU2013 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2013)
EUs2013 <- lm.beta(EU2013)
EUs2013

EU2014 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2014)
EUs2014 <- lm.beta(EU2014)
EUs2014

EU2015 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2015)
EUs2015 <- lm.beta(EU2015)
EUs2015

EU2016 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2016)
EUs2016 <- lm.beta(EU2016)
EUs2016

EU2017 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2017)
EUs2017 <- lm.beta(EU2017)
EUs2017

EU2018 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2018)
EUs2018 <- lm.beta(EU2018)
EUs2018

EU2019 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2019)
EUs2019 <- lm.beta(EU2019)
EUs2019

EU2020 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Europe, subset = Year == 2020)
EUs2020 <- lm.beta(EU2020)
EUs2020




Africa <- whrMERGE %>%
  filter(Region == "Africa")

AF2005 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2005)
AFs2005 <- lm.beta(AF2005)
AFs2005

AF2006 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2006)
AFs2006 <- lm.beta(AF2006)
AFs2006

AF2007 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2007)
AFs2007 <- lm.beta(AF2007)
AFs2007

AF2008 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2008)
AFs2008 <- lm.beta(AF2008)
AFs2008

AF2009 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2009)
AFs2009 <- lm.beta(AF2009)
AFs2009

AF2010 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2010)
AFs2010 <- lm.beta(AF2010)
AFs2010

AF2011 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2011)
AFs2011 <- lm.beta(AF2011)
AFs2011

AF2012 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2012)
AFs2012 <- lm.beta(AF2012)
AFs2012

AF2013 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2013)
AFs2013 <- lm.beta(AF2013)
AFs2013

AF2014 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2014)
AFs2014 <- lm.beta(AF2014)
AFs2014

AF2015 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2015)
AFs2015 <- lm.beta(AF2015)
AFs2015

AF2016 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2016)
AFs2016 <- lm.beta(AF2016)
AFs2016

AF2017 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2017)
AFs2017 <- lm.beta(AF2017)
AFs2017

AF2018 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2018)
AFs2018 <- lm.beta(AF2018)
AFs2018

AF2019 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2019)
AFs2019 <- lm.beta(AF2019)
AFs2019

AF2020 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Africa, subset = Year == 2020)
AFs2020 <- lm.beta(AF2020)
AFs2020





Americas <- whrMERGE %>%
  filter(Region == "Americas")

AM2005 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2005)
AMs2005 <- lm.beta(AM2005)
AMs2005

AM2006 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2006)
AMs2006 <- lm.beta(AM2006)
AMs2006

AM2007 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2007)
AMs2007 <- lm.beta(AM2007)
AMs2007

AM2008 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2008)
AMs2008 <- lm.beta(AM2008)
AMs2008

AM2009 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2009)
AMs2009 <- lm.beta(AM2009)
AMs2009

AM2010 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2010)
AMs2010 <- lm.beta(AM2010)
AMs2010

AM2011 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2011)
AMs2011 <- lm.beta(AM2011)
AMs2011

AM2012 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2012)
AMs2012 <- lm.beta(AM2012)
AMs2012

AM2013 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2013)
AMs2013 <- lm.beta(AM2013)
AMs2013

AM2014 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2014)
AMs2014 <- lm.beta(AM2014)
AMs2014

AM2015 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2015)
AMs2015 <- lm.beta(AM2015)
AMs2015

AM2016 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2016)
AMs2016 <- lm.beta(AM2016)
AMs2016

AM2017 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2017)
AMs2017 <- lm.beta(AM2017)
AMs2017

AM2018 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2018)
AMs2018 <- lm.beta(AM2018)
AMs2018

AM2019 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2019)
AMs2019 <- lm.beta(AM2019)
AMs2019

AM2020 <- lm(Happiness ~ Support + Freedom + LE + GDP, data = Americas, subset = Year == 2020)
AMs2020 <- lm.beta(AM2020)
AMs2020





summarise(hapmean = mean(`2008`, na.rm = TRUE))
mutate(Happiness2008 = mean(2008))
na.omit

  
dataX <- whrMERGE %>%
  group_by(Region, Year) %>%
  lm(Happiness ~ Support + Freedom + LE + GDP, data = whrMERGE)
  


regAFRICA <- read.csv("africa.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  arrange(Year, Predictor, RegCoef) 


ggplot(data = regAFRICA, aes(x = Year, y = RegCoef, group = Predictor)) +
  geom_line(aes(color = Predictor)) +
  geom_point(aes(color = Predictor)) +
  labs(title = "Correlation of Predictors and Happiness Over Time (Africa)", x = "Year", y = "Regression Coefficient")




regEUROPE <- read.csv("europe.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  arrange(Year, Predictor, RegCoef)


ggplot(data = regEUROPE, aes(x = Year, y = RegCoef, group = Predictor)) +
  geom_line(aes(color = Predictor)) +
  geom_point(aes(color = Predictor)) +
  labs(title = "Correlation of Predictors and Happiness Over Time (Europe)", x = "Year", y = "Regression Coefficient")




regASIA <- read.csv("asia.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  arrange(Year, Predictor, RegCoef)


ggplot(data = regASIA, aes(x = Year, y = RegCoef, group = Predictor)) +
  geom_line(aes(color = Predictor)) +
  geom_point(aes(color = Predictor)) +
  labs(title = "Correlation of Predictors and Happiness Over Time (Asia)", x = "Year", y = "Regression Coefficient")




regAMERICAS <- read.csv("americas.csv") %>%
  pivot_longer(cols = !(c(Year)),
               names_to = 'Predictor',
               values_to = 'RegCoef') %>% 
  arrange(Year, Predictor, RegCoef) 


ggplot(data = regAMERICAS, aes(x = Year, y = RegCoef, group = Predictor)) +
  geom_line(aes(color = Predictor)) +
  geom_point(aes(color = Predictor)) +
  labs(title = "Correlation of Predictors and Happiness Over Time (Americas)", x = "Year", y = "Regression Coefficient")



