library(tidyverse)
library(here)

df_trout <- read_rds(here("data","df_trout_cln.rds"))

df_trout

# plot to visualize
df_trout %>% 
  filter(wbic==883700) %>% 
  filter(length >= 10) %>%
  ggplot(aes(x = length)) + 
  geom_histogram(binwidth = 1, 
                 color = "black", fill = "gray") + 
  labs(title = "Histrogram of trout lengths (1-inch bins)") + 
  scale_y_continuous(labels = scales::comma) +
  lemon::facet_rep_wrap(vars(species))


# Big browns
bbs <- 
  df_trout %>% 
  # filter(species == "brown_trout") %>% 
  filter(length > 18) %>% 
  # filter(year > 2010) %>% 
  filter(wbic==883800)
  # filter(county %in% c("vernon")) =

bbs %>% 
  group_by(county, waterbody.name) %>% 
  tally() %>% arrange(desc(n)) %>% 
  print(n = Inf)


bbs %>% 
  # filter(str_detect(waterbody.name, "^sugar")) %>% 
  as_tibble() %>% 
  select(county, waterbody.name, year, site.seq.no,species, length) %>% 
  arrange(desc(length)) %>% 
  print(n = Inf)

# Big brookies
bbks <- df_troutdata %>% 
  filter(species == "brook_trout") %>% 
  filter(length > 15) %>% 
  filter(year > 2010) %>% 
  filter(county %in% c("vernon", "iowa", "richland","dane", "green")) %>%
  as_tibble() 

bbks %>% group_by(county, waterbody.name) %>% tally() %>% print(n = Inf)

df_troutdata %>% 
  filter(species == "brook_trout") %>% 
  filter(length > 12) %>% 
  filter(survey.year > 2015) %>% 
  filter(county %in% c("vernon", "iowa", "richland","dane", "green")) %>% 
  filter(waterbody.name == "cave_hollow_creek") %>% 
  as_tibble() %>% 
  select(county, waterbody.name, survey.year, species, length, station.name)

