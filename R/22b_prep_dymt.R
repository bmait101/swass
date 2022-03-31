
# Compile DAYMET data for trout analysis
# Bryan Maitland, 
# 2021-03-10
# 2021-08-25

# DAYMET data has been processed from RAW in another script
# and now only contains data for each catchment in the trout dataset
# from 1990-2020

# Here, we make the offsets for recruitment year, and summarize by season

# libraries
library(tidyverse)
library(lubridate)
library(here)


# Data ================================================================

df_daymet <- read_rds(here("data","daymet","daymet_cln.rds"))

# Recruitment year offsets ============================================

# First test it out:
# df_daymet %>%
#   # subset for testing
#   slice_head(n=5000) %>% select(-tmax, -tmin, -tmean) %>%
#   # add year and month columns
#   mutate(
#     year = lubridate::year(date),
#     month = lubridate::month(date),
#   ) %>%
#   relocate(c(year, month), .after = date) %>%
#   # get rid of the early dates
#   filter(date >= "1990-05-01") %>%
#   # now keep only the first day of each month to test
#   group_by(year, month) %>%
#   slice_head(n=1) %>%
#   ungroup() %>% 
#   # add year lags for appropriate months
#   mutate(
#     year_yoy = case_when(
#       month %in% c(6,7,8,9,10,11,12) ~ year + 1,
#       TRUE ~ year
#     )) %>%
#   mutate(
#     season = case_when(
#       month %in% c(6,7,8) ~ "summer",
#       month %in% c(9,10,11) ~ "autumn",
#       month %in% c(12,1,2) ~ "winter",
#       month %in% c(3,4,5) ~ "spring")
#     ) %>%
#   # pull out one year to check
#   filter(year_yoy=="2000") %>% 
#   print(n=Inf)


# Now in full:
df_daymet <- df_daymet %>%
  # get year and month
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date), 
  ) %>% 
  # new year column that puts jun-dec data in following year to link to yoy 
  # i.e., add year lags for appropriate months
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


# Calculate seasonal metrics =========================

# Summarize total precip and mean max temp by yoy-year, season, and catchment
df_daymet_covars <- df_daymet %>%
  group_by(year_yoy, season, catchid) %>%
  summarise(
    total.prcp     = sum(prcp),  
    mean.tmax      = mean(tmax),   # mean 90-day max temp
    .groups = "drop") %>% 
  rename(
    year = year_yoy,
    reach_id = catchid
    ) %>% 
  mutate(reach_id =  as.character(reach_id))

# Wrangle 
df_daymet_covars_w <- df_daymet_covars %>% 
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

# Mean-variance (z) standardize the covariates
df_daymet_covars_s <- df_daymet_covars_w %>% 
  mutate(across(3:ncol(df_daymet_covars_w), ~(scale(.) %>% as.vector))) 


# Save intermediaries
write_rds(df_daymet_covars, here("output","data","df_covar_dmt.rds"))
write_rds(df_daymet_covars_std, here("output","data","df_covar_dmt_std.rds"))


