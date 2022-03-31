# ===============================================================================#
# Code to clean FMDB flow-temp surveys data 
# by Dr. Bryan M Maitland, December 2020
# ===============================================================================#

# 1) load downloaded FMDB flow-temp survey data
# 2) clean flow data and xwalk for hydroids/reachids
# 3) clean temp data and xwalk for hydroids/reachids

# libraries
library(tidyverse)
library(magrittr)
library(wdnr.fmdb)

# set FMBD credentials
set_fmdb_credentials()


# custom functions
cfs_to_cms <- function (value) {
  return(value / 35.315)
}

c_to_f <- function(value) {
  (value * (9/5)) + 32
}

f_to_c <- function(value) {
  (value - 32) * (5/9)
}

# ===============================================================================#
# load data
# ===============================================================================#

# flow data
flow_temp <- 
  read_csv("./data/data-raw/fmdb_flow-temp.csv", na = "-") %>% 
  rename(
    STATION_ID = 'Swims Station Id', 
    survey.seq.no = 'Srvy Seq No', 
    sample_year = "Survey Year"
    )

# clean reach_id xwalk
reachids <- 
  read_csv("./data/data-raw/xwalk_flow-stations.csv") %>% 
  mutate(across(where(is.double), as.character))

# ===============================================================================#
# flow
# ===============================================================================#

# flow is mainly stored in 'Discharge' and 'FLow cfs'

flow_dirty <- 
  flow_temp %>% 
  # select the flow variables
  select(
    survey.seq.no, 
    STATION_ID, sample_year, 
    flow_mixed = `Discharge Amt`, 
    unit_flow_mixed = `Discharge Units`,
    flow_cms = `Flow m3ps`, 
    flow_cfs = `Flow cfs`
  ) 

# --------------------------------------------------------------------------------# 
# clean


flow_clean <-
  flow_dirty %>% 
  # update types and clean the weird shit out
  mutate(unit_flow_mixed = as.factor(unit_flow_mixed)) %>% 
  mutate(across(starts_with("flow"), as.double)) %>% 
  # clean up the mixed colummn
  mutate(
    flow_mixed_cms = case_when(
      unit_flow_mixed == "CFS" ~ cfs_to_cms(flow_mixed), 
      unit_flow_mixed == "CMS" ~ flow_mixed)
    ) %>% 
  # get rid of those un-needed columns
  select(-flow_mixed, -unit_flow_mixed) %>% 
  # merge the columns into one if NAs
  mutate(
    flow_cms = case_when(
      is.na(flow_cms) ~ cfs_to_cms(flow_cfs), 
      TRUE ~ flow_cms)) %>% 
  mutate(
    flow_cms = case_when(
      is.na(flow_cms) ~ flow_mixed_cms, 
      TRUE ~ flow_cms)) %>% 
  # get rid of NAs
  filter(!is.na(flow_cms)) %>% 
  # deal with 9999s
  filter(!str_detect(flow_cms, "^99")) %>% 
  # 9 surveys do not have SWIMS stations
  filter(!is.na(STATION_ID))  %>% 
  # tidy up
  mutate(across(c(survey.seq.no), as.character)) %>% 
  select(survey.seq.no, sample_year, STATION_ID, flow_cms) %>% 
  # join with hydroIDs
  left_join(reachids, by = "STATION_ID")

flow_clean


# ===============================================================================#
# temp
# ===============================================================================#

# these two surveys only have water temp in c, but the values are very high, 
# and we cannot confirm wither way - so removing from data

temp_dirty <- 
  flow_temp %>% 
  # select the flow variables
  select(
    survey.seq.no,
    STATION_ID, sample_year, 
    temp_air_c = `Ambient Air Temp Field`,
    temp_water_unkn = `Temp`, 
    temp_water_c = `Water sample temp field c`
  )

# --------------------------------------------------------------------------------# 
# list records with all NAs for temps

temp_NAs <- 
  temp_dirty %>% 
  filter(is.na(temp_air_c) & is.na(temp_water_unkn) & is.na(temp_water_c)) %>% 
  pull(survey.seq.no)

# --------------------------------------------------------------------------------# 
# clean

temp_clean <- 
  temp_dirty %>% 
  # remove records with NAs for all temp values
  filter(!survey.seq.no %in% temp_NAs) %>% 
  #make conversions
  mutate(
    temp_water_c = case_when(
      temp_water_c >= 35 ~ f_to_c(temp_water_c), 
      TRUE ~ temp_water_c), 
    temp_water_unkn = case_when(
      temp_water_unkn >= 35 ~ f_to_c(temp_water_unkn), 
      TRUE ~ temp_water_unkn),
    temp_air_c = case_when(
      temp_air_c >= 45 ~ f_to_c(temp_air_c), 
      TRUE ~ temp_air_c)) %>% 
  # if temp_c is NA, then give it the middle column value
  mutate(
    temp_water_c = case_when(
      is.na(temp_water_c) ~ temp_water_unkn, 
      TRUE ~ temp_water_c)) %>% 
  # remove the remianing rows with no water temp data (n = 45 records)
  filter(!is.na(temp_water_c)) %>% 
  # tidy up
  mutate(across(c(survey.seq.no), as.character)) %>% 
  select(survey.seq.no, sample_year, STATION_ID, temp_air_c, temp_water_c) %>% 
  # join with hydroIDs
  left_join(reachids, by = "STATION_ID") %>% 
  mutate(REACH_ID = if_else(is.na(REACH_ID), HYDRO_ID, REACH_ID))

temp_clean

# ===============================================================================#
# get survey dates
# ===============================================================================#
  
survey_seqs <- 
  flow_clean %>% 
  distinct(survey.seq.no) %>% 
  bind_rows(temp_clean %>% distinct(survey.seq.no)) %>% 
  distinct() %>% 
  pull()

# split efforts in 1000 chunks
chunks <- split(survey_seqs, ceiling(seq_along(survey_seqs)/1000))  

# set output df
full.dat <- list() 

# loop through and pull surveys
for (i in (1:length(chunks))) {  
  d <- get_fmdb_surveys(survey_seq = chunks[[i]])
  full.dat[[i]] <- d
}

coords <- 
  bind_rows(full.dat)  %>% 
  as_tibble()

# compile data and clean it
flow_stations_coords <- 
  coords %>% 
  select(survey.seq.no, survey.begin.date) %>% 
  rename(sample_date = survey.begin.date) %>% 
  mutate(across(c(survey.seq.no), as.character))

# clean up environment
rm(chunks); rm(d); rm(i)


# ===============================================================================#
# to file
# ===============================================================================#

flow_clean %>% 
  left_join(flow_stations_coords, by = "survey.seq.no") %>% 
  write_csv("./data/data-raw/fmdb_flow-clean.csv")

temp_clean %>% 
  left_join(flow_stations_coords, by = "survey.seq.no") %>% 
  write_csv("./data/data-raw/fmdb_temp-clean.csv")


# ===============================================================================#
# END
# ===============================================================================#
