#========================================#
# Linking covariates to trout trend data & Prepare dataset for analysis
# Dr. Bryan M. Maitland
# 2022-Aug-03
#========================================#

## Set up ----

# Libraries
library(tidyverse)
library(here)


## Data ----

df_cpes_va <- readRDS(here("output","data","df_cpe_va.rds"))
df_whd_cln_slct_std <- readRDS(here("output","data","whd_cln_slct_std.rds"))
df_daymet_seasonal_w_std <- readRDS(here("output","data","daymet_seasonal_w_std.rds"))
df_daymet_longterm_std <- readRDS(here("output","data","daymet_longterm_std.rds"))


## Join datasets ----

# Merge
df_drivers <- df_cpes_va |> 
  rename(year = survey.year) |>  
  left_join(df_whd_cln_slct_std, by = c("reach_id")) |> 
  left_join(df_daymet_seasonal_w_std, by = c("reach_id", "year")) |> 
  left_join(df_daymet_longterm_std, by = c("reach_id"))

# Add factors
df_drivers <- df_drivers |>
  mutate(across(where(is.character), as.factor)) |> 
  mutate(
    year_f = factor(year), # year as a factor for ranef
    year_s = scale(year)[,1],  
    latitude_s = scale(latitude)[,1]
  ) |> 
  relocate(c(year_f, year_s), .after=year)

# Calculate yr-observation
tmp <- df_drivers %>%
  distinct(year, reach_id, species, yoy) %>%
  group_by(reach_id, species, yoy) %>%
  summarise(yr_obs = n()) %>%
  ungroup()

df_drivers <- df_drivers %>%
  left_join(tmp, by = c("reach_id", "species", "yoy")) %>%
  relocate(yr_obs, .before=year)

rm(tmp)

## Export dataset ----

# df_drivers %>% write_rds(here("output","data", "df_drivers.rds"))



## Prep for analysis -------------------

### Filter >5 yr-obs ----
df_analysis <- df_drivers %>% filter(yr_obs >= 5)


### Missing data ----

map(df_analysis, ~sum(is.na(.)))
df_analysis <- df_analysis %>% 
  filter(!is.na(total.prcp_summer)) %>% 
  filter(!is.na(total.prcp_autumn)) %>% 
  filter(!is.na(total.prcp_winter)) %>% 
  filter(!is.na(total.prcp_spring))
map(df_analysis, ~sum(is.na(.)))


### Tidy ----
df_analysis <- df_analysis %>%
  rename(size_class = yoy) %>% 
  mutate(size_class = if_else(size_class=="N","adult","yoy")) %>% 
  mutate(across(where(is.character), as.factor))


### Subsets ----

# YOY Brook trout
df_analysis_bkt0 <- df_analysis %>% 
  filter(species == "brook_trout", size_class=="yoy") %>% droplevels()

# Adult brook trout
df_analysis_bkt1 <- df_analysis %>% 
  filter(species == "brook_trout", size_class=="adult") %>% droplevels()

# YOY brown trout
df_analysis_bnt0 <- df_analysis %>%
  filter(species == "brown_trout", size_class=="yoy") %>% droplevels()

# Adult brown trout
df_analysis_bnt1 <- df_analysis %>%
  filter(species == "brown_trout", size_class=="adult")  %>% droplevels()
