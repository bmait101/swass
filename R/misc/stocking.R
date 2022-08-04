library(tidyverse)
library(stringr)

data <- All_Stocking_Data %>% filter(Species%in% c("BROOK TROUT", "BROWN TROUT", "RAINBOW TROUT"))

data$CountyName <- tolower(data$CountyName)
data$CountyName <- as.factor(data$CountyName)

data <- data %>% filter(Year >= 2015) %>% droplevels()


table(data$Species, data$CountyName)

bkt <- data %>% filter(Species =="BROOK TROUT")
bnt <- data %>% filter(Species=="BROWN TROUT")
rbt <- data %>% filter(Species=="RAINBOW TROUT")

bkt %>% distinct(StockedWaterbodyName)
bnt %>% distinct(StockedWaterbodyName)
rbt %>% distinct(StockedWaterbodyName)

