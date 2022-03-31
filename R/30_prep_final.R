
# Prep datasets for analyses

library(tidyverse)
library(here)

# Data =========================================================================

df_drivers <- readRDS(here("output","data", "df_drivers.rds"))
# df_surveys <- readRDS(here("output", "data", "df_surveys.rds"))


# Wrangle ======================================================================

# Calculate number of yr-observation for each stream reach and group
yr_obs <- df_drivers %>%
  distinct(year, reach_id, species, yoy) %>%
  group_by(reach_id, species, yoy) %>%
  summarise(yr_obs = n()) %>%
  ungroup()

# add yr_obs to data
df_drivers <- df_drivers %>%
  left_join(yr_obs, by = c("reach_id", "species", "yoy")) %>%
  relocate(yr_obs, .before=year)
rm(yr_obs)

# rename and add factors
df_drivers <- df_drivers %>%
  rename(size_class = yoy) %>% 
  mutate(size_class = if_else(size_class=="N","adult","yoy")) %>% 
  mutate(across(where(is.character), as.factor))

# # List of survey purposes to keep for analysis
# targs.survey.purpose.cpe <- c(
#   "fisheries_assessments_trout_trend",
#   "fisheries_assessments_trout_rotation",
#   "fisheries_assessments_trout_potential",
#   "fisheries_assessments_rivers",
#   "baseline_monitoring",
#   "comprehensive_survey",
#   "general_survey",
#   "natural_community_reference",
#   "targeted_watershed_assessment",
#   "watershed_comprehensive_sites",
#   "watershed_long_term_reference_site_monitoring"
# )



# Filter data ============================================================

new.dat <- df_drivers %>%
  # filter(primary.survey.purpose %in% targs.survey.purpose.cpe)  %>%
  filter(yr_obs >= 5)

# new.dat %>%
#   group_by(species,trout_class) %>%
#   count()

# Remove NAs
# map(new.dat, ~sum(is.na(.)))
new.dat <- new.dat %>% 
  filter(!is.na(total.prcp_summer)) %>% 
  filter(!is.na(total.prcp_autumn)) %>% 
  filter(!is.na(total.prcp_winter)) %>% 
  filter(!is.na(total.prcp_spring))
  
# new.dat %>% group_by(species,size_class) %>% count()
# 
# new.dat %>% distinct(survey.seq.no)
# new.dat %>% distinct(visit.fish.seq.no)
# new.dat %>% distinct(site.seq.no)
# new.dat %>% distinct(reach_id)


# new.dat <- new.dat %>%
#   filter(trout_class %in% c("CLASS I","CLASS II"))


# Subset data for analysis ================================================

# YOY Brook trout
df_trends_bkt0 <- new.dat %>% 
  filter(species == "brook_trout", size_class=="yoy") %>%
  droplevels()

# Adult brook trout
df_trends_bkt1 <- new.dat %>% 
  filter(species == "brook_trout", size_class=="adult") %>% 
  droplevels()

# YOY brown trout
df_trends_bnt0 <- new.dat %>%
  filter(species == "brown_trout", size_class=="yoy") %>% 
  droplevels()

# Adult brown trout
df_trends_bnt1 <- new.dat %>%
  filter(species == "brown_trout", size_class=="adult")  %>% 
  droplevels()
