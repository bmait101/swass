
# St. Croix County explore

# packages
library(tidyverse)
library(scales)
theme_set(theme_bw())

# load xwalk
xwalk_full <- readRDS("Data/xwalk_station_hydro_reach.rds")
# load whd data
load("Data/WHDPlus_data.Rdata")
whd_base_atribs <- 
  whd_base_atribs %>% 
  as_tibble() %>% 
  rename(REACH_ID = REACHID) %>% 
  select(REACH_ID, C_LAT, C_LONG, C_ECO, GRADIENT, SINUOUS, C_HUC12, STREAM_ORDER, WET_WIDTH)

# load cpe data
surveys <- readRDS("Data/surveys_2020_12_03.rds")
efforts <- readRDS("Data/efforts_2020_12_03.rds")
cpe_by_length <- readRDS("Data/cpe_by_length_2020_12_08.rds")

cpe_by_length <- 
  cpe_by_length %>% 
  drop_na(length_class) %>% 
  filter(cpe < 10000) %>% 
  left_join(surveys %>% 
              select(county, survey.seq.no, site.seq.no, swims.station.id), 
            by = "survey.seq.no") %>% 
  # rename to match xwalk and convert to character
  #rename(STATION_ID = swims.station.id) %>% 
  mutate(STATION_ID = as.character(swims.station.id), 
         county = as_factor(county)) %>% 
  left_join(xwalk_full, by = "STATION_ID") %>% 
  left_join(whd_base_atribs, by = "REACH_ID") %>% 
  drop_na(REACH_ID) 
cpe_by_length

cpe_by_length %>% 
  filter(species == "brook_trout" & 
           county == "st_croix" & 
           survey.year %in% 1994:2020) %>% 
  group_by(survey.year, species, length_class) %>%
  summarise(mean = mean(cpe),
            sd = sd(cpe)/sqrt(length(cpe))) %>%
  ggplot(aes(x = survey.year, y = mean, color = length_class)) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
  scale_x_continuous(breaks = seq(1995,2020,5)) +
  # lemon::facet_rep_wrap(vars(species)) +
  scale_color_brewer(palette = "Paired") + 
  labs(x = "Survey Year", y = "Trout / mile of electrofishing", color = "Length Class",
       title = "St. Croix Brook Trout pop trends") 
ggsave("Misc/twitter/stcroix_brookies.png", dpi = 300, width = 4, height = 2.25)


# -----------------

surveys %>% 
  filter(county == "st_croix" & survey.year == 1995) %>% 
  filter(survey.seq.no %in% c(31520659, 31520658)) %>% 
  as_tibble() %>% 
  select(1:16)

efforts %>% 
  filter(county == "st_croix" & survey.year == 1995) %>% 
  filter(survey.seq.no %in% c(31520659, 31520658)) %>% 
  as_tibble() %>% 
  select(1:16) %>% 
  select(county,waterbody.name,survey.year,gear,field.person)

cpe_by_length %>% 
  filter(survey.seq.no %in% c(31520659, 31520658))
  