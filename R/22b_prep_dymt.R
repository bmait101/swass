#========================================#
# Compile DAYMET data for trout analysis
# Dr. Bryan M. Maitland
# 2022-Aug-03
#========================================#

# DAYMET data has been processed from RAW in another script (22a_)
# and now only contains observations for each catchment in the trout dataset

# Here, we make the offsets for recruitment year, and summarize by season

## Set up ----

# libraries
library(tidyverse)
library(lubridate)
library(here)


## Data ----

df_daymet_cln <- read_rds(here("data","daymet","daymet_cln.rds"))


## Recruit year offsets ----

df_daymet_offset <- df_daymet_cln %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date), 
  ) %>% 
  # add year lags for appropriate months
  mutate(
    year_yoy = case_when(
      month %in% c(6,7,8,9,10,11,12) ~ year + 1,
      TRUE ~ year
    )) %>% 
  # now assign seasonal groupings
  mutate(
    season = case_when(
      month %in% c(6,7,8) ~ "summer",
      month %in% c(9,10,11) ~ "autumn",
      month %in% c(12,1,2) ~ "winter", 
      month %in% c(3,4,5) ~ "spring"
    )) %>% 
  # clean it up
  relocate(c(year, month, year_yoy, season), .after = date) %>%
  relocate(year_yoy, .after=year)



## Seasonal metrics ----

### Calculate ----
df_daymet_seasonal <- df_daymet_offset %>%
  group_by(year_yoy, season, catchid) %>%
  summarise(
    total.prcp = sum(prcp),  
    mean.tmax  = mean(tmax),
    .groups = "drop") %>% 
  rename(year = year_yoy, reach_id = catchid) %>% 
  mutate(reach_id =  as.character(reach_id))

# Make wide df
df_daymet_seasonal_w <- 
  df_daymet_seasonal %>% 
  pivot_longer(
    -c(year, reach_id, season), 
    names_to = "covar", 
    values_to="value"
    ) %>% 
  pivot_wider(
    id_cols = c("year", "reach_id"), 
    names_from = c("covar", "season"), 
    values_from = c("value")
    ) 

### Standardize ----

# Mean-variance (z) standardize the covariates
df_daymet_seasonal_w_std <- df_daymet_seasonal_w %>% 
  mutate(across(3:ncol(df_daymet_seasonal_w), ~(scale(.) %>% as.vector))) 


## Save ----

write_rds(df_daymet_seasonal, here("output","data","daymet_seasonal.rds"))
write_rds(df_daymet_seasonal_w, here("output","data","daymet_seasonal_w.rds"))
write_rds(df_daymet_seasonal_w_std, here("output","data","daymet_seasonal_w_std.rds"))


## Long-term metrics ----

### Calculate ----
df_daymet_longterm <- df_daymet_offset %>%
  group_by(catchid) %>%
  summarise(
    lt_mean.daily.prcp = mean(prcp),  
    lt_mean.daily.tmean  = mean(tmean),
    .groups = "drop") %>% 
  rename(reach_id = catchid) %>% 
  mutate(reach_id =  as.character(reach_id))


### Standardize ----

# Mean-variance (z) standardize the covariates
df_daymet_longterm_std <- df_daymet_longterm %>% 
  mutate(across(2:3, ~(scale(.) %>% as.vector))) 


## Save objects ----

write_rds(df_daymet_longterm, here("output","data","daymet_longterm.rds"))
write_rds(df_daymet_longterm_std, here("output","data","ddaymet_longterm_std.rds"))

