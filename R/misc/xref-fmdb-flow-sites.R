# ===============================================================================#
# Code for dealing with erroneous REACH and HYDROIDs
# by Bryan M Maitland, 
# March 2021
# ===============================================================================#

# 1) load and xwalk data
# 2) deal with NAs for hydroids
# 3) deal with stations with multiple hydroids
# 4) deal with stations with hydroids that begin with a 6


# libraries
library(tidyverse)
library(janitor)
library(rgdal)  # for help reading shapefiles
library(sf)  # spatial analysis
library(nngeo)  # for nn analysis

#-----------------------------------------------------------------------------#
# data
#-----------------------------------------------------------------------------#

# load 24k HYDRO flowlines
lines <- 
  readOGR("./data/databases/hydro.gdb", "WD_HYDRO_FLOWLINE_LN_24K") %>% 
  clean_names()  %>% 
  st_as_sf()

# load xwalk table
xwalk <- 
  readRDS("./data/data-raw/xwalk_all.rds") %>% 
  clean_names() %>% 
  mutate(hydro_id = na_if(hydro_id, "NA"))


# must get coords for surveys

# fmdn package
library(wdnr.fmdb)
set_fmdb_credentials()

# get list of survey seq to pull from 
survey_seqs <- 
  read_csv("./data/data-raw/fmdb_flow-temp.csv") %>% 
  select(survey.seq.no = "Srvy Seq No") %>% 
  distinct(survey.seq.no, .keep_all = TRUE) %>% 
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

stations <- 
  bind_rows(full.dat) %>% 
  as_tibble() %>% 
  select(station_id = swims.station.id, wbic, latitude, longitude) %>% 
  filter(!is.na(station_id)) %>% 
  distinct(station_id, .keep_all = TRUE) 

stations <- 
  stations %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 3071)

# clean up environment
rm(chunks); rm(d); rm(i); rm(full.dat)


#-----------------------------------------------------------------------------#
# xwalk sites
#-----------------------------------------------------------------------------#

stations_xwalk <- left_join(stations, xwalk, by = "station_id")

nrow(stations_xwalk) - nrow(stations)

# extra 709 rows


# ===============================================================================#
# Deal with hydroids NAs (no hydro_id for that station_id)
# ===============================================================================#


stations_xwalk_NAs <- filter(stations_xwalk, is.na(hydro_id)) 

#-----------------------------------------------------------------------------#
# NN analysis
#-----------------------------------------------------------------------------#

points <- stations_xwalk_NAs

# get nearest lines to stations and their distances
nn_trace <- st_nn(points, lines, k = 2, returnDist = TRUE)

# extract the flow lines indexes and distances
nn_distances <-bind_cols(
  index = unlist(nn_trace$nn),
  distance = unlist(nn_trace$dist))

# join stations to flow lines using the nearest neighbor, get distance
nn_join <- st_join(points, lines, join = st_nn, k = 2)

# bind the distances to the nn_join
nn_join_full <- 
  bind_cols(nn_join, nn_distances) %>% 
  st_drop_geometry() %>% 
  select(station_id, wbic, river_sys_wbic, hydroid, distance) %>% 
  mutate(across(where(is.integer), as.character)) %>% 
  rename(hydro_id = hydroid) %>% 
  group_by(station_id) %>% 
  slice_min(distance)

# check it (should be zero)
nn_join_full %>% filter(n() > 1)

#-----------------------------------------------------------------------------#
# Aggregate 
#-----------------------------------------------------------------------------#

stations_xwalk_NAs_cln <- 
  nn_join_full %>% 
  ungroup() %>% 
  select(station_id, hydro_id) %>% 
  # now we need the reach ids
  left_join(xwalk %>% select(-station_id), by = c("hydro_id")) %>% 
  distinct(station_id, .keep_all = TRUE) %>% 
  # give the NA reach ids the hydro id
  mutate(reach_id = if_else(is.na(reach_id), hydro_id, as.character(reach_id)))


# check for 6's in reach_ids
filter(stations_xwalk_NAs_cln, str_detect(reach_id, "^6")) 



# ===============================================================================#

# Deal with stations with multiple hydro/reachids 

# ===============================================================================#

# list which surveys have multiple hydro and reaches
stations_xwalk %>% 
  filter(! station_id %in% stations_xwalk_NAs$station_id) %>% 
  group_by(station_id) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  group_by(factor(n)) %>% 
  tally()
# 545 with duplicates; 400 with 2, 126 with 3, and 19 with 4 dups

# how many total rows with stations that have dups
stations_xwalk %>% 
  filter(! station_id %in% stations_xwalk_NAs$station_id) %>% 
  group_by(station_id) %>% 
  tally() %>% 
  filter(n > 1) %>%
  ungroup() %>% 
  summarise(sum = sum(n))
# 1254 total - 545 stations w/ dupes = 709 extra rows (perfect; should match xwalk extras)

# make a list of them
stations_xwalk_dups <-
  stations_xwalk %>% 
  filter(! station_id %in% stations_xwalk_NAs$station_id) %>% 
  group_by(station_id) %>% 
  tally() %>% 
  filter(n > 1)  %>% 
  left_join(stations_xwalk %>% 
              st_drop_geometry() %>% 
              select(station_id, wbic), by = "station_id") %>% 
  distinct(station_id, .keep_all = TRUE)
stations_xwalk_dups

#-----------------------------------------------------------------------------#
# NN anlaysis
#-----------------------------------------------------------------------------#

points <- stations_xwalk_dups

# get nearest lines to stations and their distances
nn_trace <- st_nn(points, lines, k = 3, returnDist = TRUE)

# extract the flow lines indexes and distances
nn_distances <-
  bind_cols(
    index = unlist(nn_trace$nn),
    distance = unlist(nn_trace$dist))

# join stations to flow lines using the nearest neighbor, get distance
nn_join <- st_join(points, lines, join = st_nn, k = 3)

# bind the distances to the nn_join
nn_join_full <- 
  bind_cols(nn_join, nn_distances) %>% 
  st_drop_geometry() %>% 
  select(station_id, wbic, river_sys_wbic, hydro_id = hydroid, distance) %>% 
  mutate(across(where(is.integer), as.character)) %>% 
  mutate(wbic_match = river_sys_wbic == wbic)

# after slicing minimum, see which wbic don't match up
nn_join_full %>% 
  arrange(station_id, distance) %>% 
  group_by(station_id) %>% 
  slice_min(distance) %>% 
  filter(wbic_match == FALSE) -> wbic_mismatch
wbic_mismatch
# 27 mismtch; so filter out these, slice min, then add corrected ones bacl

nn_join_full %>% filter(station_id %in% wbic_mismatch$station_id) %>% print(n = Inf)

# all of these need to be snapped in SWIMS
wbic_mismatch_cln <-
  wbic_mismatch %>% 
  mutate(hydro_id = case_when(
    station_id == 10008065 ~ 200011660, 
    station_id == 10008109 ~ 200006216, 
    station_id == 10008147 ~ 200058373,
    station_id == 10008219 ~ 200033804, 
    station_id == 10008341 ~ 200099714, 
    station_id == 10008354 ~ 200210876,
    station_id == 10008388 ~ 200112584, 
    station_id == 10008478 ~ 200081079, 
    station_id == 10008631 ~ 200211413, 
    station_id == 10008648 ~ 200130068,
    station_id == 10008862 ~ 200035468,
    station_id == 10008919 ~ 200124973,
    station_id == 10008930 ~ 200056416,
    station_id == 10009007 ~ 200053869,
    station_id == 10009048 ~ 200067588,
    station_id == 10009062 ~ 200122890,
    station_id == 10009136 ~ 200059809,
    station_id == 10009553 ~ 200073889,
    station_id == 10009911 ~ 200189893,
    station_id == 10009912 ~ 200030758,
    station_id == 10010040 ~ 200050833,
    station_id == 10010160 ~ 200072768,
    station_id == 10010169 ~ 200048123,
    station_id == 10010192 ~ 200158863,
    station_id == 10010251 ~ 200143859,
    station_id == 10011474 ~ 200168558,
    station_id == 10011809 ~ 200177428
  )) %>% 
  mutate(across(c(hydro_id), as.character))

nn_join_full <- 
  nn_join_full %>% 
  group_by(station_id) %>% 
  slice_min(distance) %>% 
  filter(!station_id %in% wbic_mismatch$station_id) %>% 
  bind_rows(wbic_mismatch_cln)

# check it (should be zero)
nn_join_full %>% filter(n() > 1)
# 2 extra rows bc the distances to hydros are equal; remove bad ones

# update correct list and clean it up
nn_join_full <- 
  nn_join_full %>% 
  filter(! (station_id == 10009176 & hydro_id == 200123980)) %>% 
  filter(! (station_id == 10011773 & hydro_id == 200104897)) 

# check it (should be zero)
nn_join_full %>% filter(n() > 1)

#-----------------------------------------------------------------------------#
# Aggregate  
#-----------------------------------------------------------------------------#

stations_xwalk_dups_cln <- 
  nn_join_full %>% 
  ungroup() %>% 
  select(station_id, hydro_id) %>% 
  # now we need the reach ids
  left_join(xwalk %>% select(-station_id), by = c("hydro_id")) %>% 
  distinct(station_id, .keep_all = TRUE) %>% 
  # give the NA reach ids the hydro id
  mutate(reach_id = if_else(is.na(reach_id), hydro_id, as.character(reach_id)))


# check for 6's in reach_ids
filter(stations_xwalk_dups_cln, str_detect(reach_id, "^6")) 
# 6 are 6's;, convert these to their hydro ids

stations_xwalk_dups_cln <- 
  stations_xwalk_dups_cln %>% 
  mutate(reach_id = case_when(
    str_detect(reach_id, "^6") ~ hydro_id, 
    TRUE ~ reach_id
  ))


# ===============================================================================#

# Deal with 6s in hydroids

# ===============================================================================#

stations_xwalk_6s <- 
  stations_xwalk %>% 
  filter(!station_id %in% stations_xwalk_NAs$station_id) %>% 
  filter(!station_id %in% stations_xwalk_dups$station_id) %>% 
  filter(str_detect(hydro_id, "^6")) %>% 
  select(-hydro_id, -reach_id)
# 54 that have 6 for hydros

#-----------------------------------------------------------------------------#
# NN anlaysis
#-----------------------------------------------------------------------------#

points <- stations_xwalk_6s

# get nearest lines to stations and their distances
nn_trace <- st_nn(points, lines, k = 3, returnDist = TRUE)

# extract the flow lines indexes and distances
nn_distances <-
  bind_cols(
    index = unlist(nn_trace$nn),
    distance = unlist(nn_trace$dist))

# join stations to flow lines using the nearest neighbor, get distance
nn_join <- st_join(points, lines, join = st_nn, k = 3)

# bind the distances to the nn_join
nn_join_full <- 
  bind_cols(nn_join, nn_distances) %>% 
  st_drop_geometry() %>% 
  select(station_id, wbic, river_sys_wbic, hydro_id = hydroid, distance) %>% 
  mutate(across(where(is.integer), as.character)) %>% 
  mutate(wbic_match = river_sys_wbic == wbic)

# after slicing minimum, see which wbic don't match up
nn_join_full %>% 
  arrange(station_id, distance) %>% 
  group_by(station_id) %>% 
  slice_min(distance) %>% 
  filter(wbic_match == FALSE) -> wbic_mismatch
wbic_mismatch %>% arrange(station_id)
# 1 mismtch; 

nn_join_full %>% 
  filter(station_id %in% wbic_mismatch$station_id)  %>% 
  arrange(station_id) %>% print(n = Inf)

wbic_mismatch_cln <-
  wbic_mismatch %>% 
  mutate(hydro_id = case_when(
    station_id == 10012019 ~ 200142691
  )) %>% 
  mutate(across(c(hydro_id), as.character))


nn_join_full <- 
  nn_join_full %>% 
  group_by(station_id) %>% 
  slice_min(distance) %>% 
  filter(!station_id %in% wbic_mismatch$station_id) %>% 
  bind_rows(wbic_mismatch_cln)


# check it (should be zero)
nn_join_full %>% filter(n() > 1)

#-----------------------------------------------------------------------------#
# Aggregate 
#-----------------------------------------------------------------------------#

stations_xwalk_6s_cln <- 
  nn_join_full %>% 
  ungroup() %>% 
  select(station_id, hydro_id) %>% 
  # now we need the reach ids
  left_join(xwalk %>% select(-station_id), by = c("hydro_id")) %>% 
  distinct(station_id, .keep_all = TRUE) %>% 
  # give the NA reach ids the hydro id
  mutate(reach_id = if_else(is.na(reach_id), hydro_id, as.character(reach_id)))


# check for 6's in reach_ids
filter(stations_xwalk_6s_cln, str_detect(reach_id, "^6")) 


# ===============================================================================#
# Update 
# ===============================================================================#


stations_cln <- 
  
  stations_xwalk %>% 
  st_drop_geometry() %>% 
  
  select(station_id, hydro_id, reach_id) %>% 
  
  filter(!station_id %in% stations_xwalk_NAs$station_id) %>% 
  filter(!station_id %in% stations_xwalk_dups$station_id) %>% 
  filter(!station_id %in% stations_xwalk_6s$station_id) %>% 
  
  mutate(reach_id = hydro_id) %>% 
  
  
  bind_rows(stations_xwalk_NAs_cln) %>% 
  bind_rows(stations_xwalk_dups_cln) %>% 
  bind_rows(stations_xwalk_6s_cln) 


# check it
length(unique(stations_xwalk$station_id))
length(unique(stations_cln$station_id))


# ===============================================================================#
# Write to file
# ===============================================================================#

stations_cln %>% write_csv("./data/data-raw/xwalk_fmdb_flow_stations.csv")

# ===============================================================================#
# END
# ===============================================================================#

