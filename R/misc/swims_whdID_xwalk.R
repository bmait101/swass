# crosswalk table: SWIMS to 24k hydroIDs
# Bryan Maitland
# 2021-08-10

# prep
library(tidyverse)
library(wdnr.db)
library(here)

# NOTE: you must be on WDNR VPN
# NOTE: you need read privileges for SWIMS in SECPRD

# SWIMS_station_ID / Hydro_ID ========================

# # open connection to SWIMS (use your credentials) ORACLE 
# con <- wdnr_connect(
#   "dnr_secprd.world",
#   user = "maitlbmqzm",
#   password = "Brooklynfishes4taiman&sticks"
#   )
# 
# # get xwalk
# xref_swims_hydro <-
#   query_db(
#     con,
#     "select station_id, hydro_id
#     from w07510.wt_swims_monit_station_loc_v"
#     ) %>%
#   as_tibble() %>%
#   separate_rows(HYDRO_ID)
# 
# # closer connection
# wdnr_disconnect(con)

# xref pulled by Jake Dickmann 2021-08-14
xref_swims_hydro <- 
  read_csv(here("data","stationid-hydroid.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.double), as.character))

# Hydro_ID / Reach_ID =================================


# open connection to WHD 24K Hydro
con <- wdnr_connect("dnr_sde.world", password = "ONLY")

# get xwalk
xref_hydro_reach <- 
  query_db(
    con, 
    "select hydroid, reachid 
    from w23324.wd_hydro_va_flwln_ntwrk_ln_24k"
  ) %>% 
  as_tibble() %>%
  rename(reach_id = REACHID, 
         hydro_id = HYDROID) %>%
  mutate(across(where(is.double), as.character)) 

wdnr_disconnect(con)


# Merge and export to file =================================

xref_swims_hydro %>% arrange(station_id) %>% View()

xref_hydro_reach %>% filter(is.na(reach_id))
xref_hydro_reach %>% filter(!str_detect(reach_id, "^6"))


xref_table <- 
  left_join(
    xref_swims_hydro, 
    xref_hydro_reach,  ## 6s are lakes, 2s streams
    by = "hydro_id"
    ) %>% 
  mutate(hydro_id = if_else(is.na(hydro_id), reach_id, hydro_id)) %>% 
  rename(swims.station.id = station_id) 

# explore to file
saveRDS(xref_table, here("data","xref_swims_hydro.rds"))

# clean up
rm(list = ls())
