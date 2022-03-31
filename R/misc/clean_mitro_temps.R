# ===============================================================================#
# Code to tidy up Matt Mitro's HOBO temp data

# by Bryan M Maitland, January 2021
# ===============================================================================#

# libraries
library(tidyverse)  # for dplyr, pipe, purr, and other general function
library(readxl)  # for reading Excel files
library(lubridate)  # for working with dates
library(janitor)  # for cleaning up column names
library(fs)  # functions for working with file systems

library(rgdal)  # for reading shapefiles
library(sf)  # for working with spatial data
library(nngeo)  # for spatial joining



# ===============================================================================#
# load and clean data
# ===============================================================================#


# load and clean coordinates ----------------------------------------------------#

hobo_coords <- 
  readxl::read_xlsx("data/mitro/GPS Data Loggers.xlsx") %>% 
  clean_names() %>% 
  rename(latitude = n, 	longitude = w) %>% 
  separate(stream_name, 
           into = c("a","b"), 
           sep = " ", 
           extra = "merge") %>% 
  mutate(
    # fix longitude values
    longitude = longitude * -1,
    # make better site ids for merging with temp data
    site_id = case_when(
      a == "Ash" & site == "WT1" ~ "ASH_WT1",
      a == "Ash" & site == "WT2" ~ "ASH_WT2",
      a == "Ash" & site == "WT3" ~ "ASH_WT3",
      a == "Ash" & site == "Whip-poor-will Rd." ~ "ASH_Whip",
      b == "Fork Beaver Creek" ~ "sfbeaver",
      b == "Fork Hay River" ~ "sfhay", 
      TRUE ~ a) %>% tolower()) %>% 
  select(site_id, wbic, latitude, longitude)

# check it
hobo_coords



# load and clean temp data ----------------------------------------------------#

# set directory of csvs files
data_dir <- "data/mitro/water_temp/csvs"

# load and stack all the csv, then tidy and join with site coords
temp_mitro <- 
  data_dir %>% 
  # get a list of the csv files in the directory
  dir_ls(regexp = "\\.csv$") %>% 
  # map a read function over each csv file and bind them together
  map_dfr(read_csv, 
          skip = 2, 
          col_names = c("id", "date", "temp_f"), 
          .id = "source") %>% 
  # use the filenames for each to create site IDs 
  separate(source, 
           into = c("a", "file_name"), 
           sep = 36) %>% 
  select(-id, -a) %>% 
  separate(file_name, into = c("a","b","c"), sep = "_", extra = "drop") %>% 
  mutate(
    # also need to parse the date column
    date = parse_date_time(date, "mdy IMS p"),
    # make new site ids for merging with coordinates
    site_id = case_when(
      a == "ASH" & c == 1 ~ "ASH_WT1",
      a == "ASH" & c == 2 ~ "ASH_WT2",
      a == "ASH" & c == 3 ~ "ASH_WT3",
      a == "ASH" & b == "Whip-poor-will" ~ "ASH_Whip",
      a == "SF" ~ "SFHAY",
      a == "VOSSE" ~ "vossee",
      TRUE ~ a
    )
  ) %>% 
  mutate(site_id = tolower(site_id)) %>% 
  select(site_id, date, temp_f)

# check it
temp_mitro





# ===============================================================================#
# link coordinates to REACHID and potential SWIMS sites
# ===============================================================================#

# load 24K hydro layer
lines <- 
  readOGR("./data/databases/hydro.gdb", "WD_HYDRO_FLOWLINE_LN_24K") %>% 
  st_as_sf() %>% 
  clean_names() 

# load SWIMS cross-reference table
xwalk <- 
  readRDS("./data/data-raw/xwalk_all.rds") %>% 
  clean_names() %>% 
  mutate(hydro_id = na_if(hydro_id, "NA"))

# make hobo coords spatial
hobo_coords_sf <- 
  hobo_coords %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 3071)

#-----------------------------------------------------------------------------#
# nearest neighbor spatial analysis 
#-----------------------------------------------------------------------------#

points <- hobo_coords_sf

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
  select(site_id, wbic, hydroid, distance) %>% 
  mutate(across(where(is.integer), as.character)) %>% 
  rename(hydro_id = hydroid) %>% 
  group_by(site_id) %>% 
  slice_min(distance) %>% 
  select(-distance, - wbic) %>% 
  mutate(reach_id = hydro_id)


# link hydroids to our coord table
hobo_coords_xref <- left_join(hobo_coords, nn_join_full, by = "site_id")

# make a table of postial swims stations for each hydroid (one to many)
hobo_coords_xref_potential_swims <- 
  left_join(hobo_coords_xref, xwalk %>% select(-reach_id), by = "hydro_id") %>% 
  rename(potential_swims_stations = station_id)


# ===============================================================================#
# Join x-ref'ed coordinates with temp data
# ===============================================================================#

temp_mitro <- left_join(temp_mitro, hobo_coords_xref, by = "site_id")


# ===============================================================================#
# Write to file
# ===============================================================================#

write_csv(temp_mitro, "data/data-raw/mitro_temp_cln.csv")
# write_csv(hobo_coords_xref, "data/data-raw/mitro_temp_sites_xref.csv")
# write_csv(hobo_coords_xref_potential_swims, "data/data-raw/mitro_temp_sites_swims_matches.csv")

# ===============================================================================#
# END
# ===============================================================================#

temp_mitro <- read_csv(here::here("data", "stream_temp", "mitro_temp_cln.csv"))



temp_mitro %>%
  filter(site_id == "big") %>% 
  ggplot(aes(x= date, y=temp_f)) +
  geom_point(size = 1, alpha = 0.1)+ 
  theme_minimal() +
  labs(title="Raw temperature data", y="Temperature (°C)", x="")


temp_mitro %>% 
  rename(date_time = date) %>% 
  mutate(date = lubridate::date(date_time)) %>% 
  group_by(site_id, date) %>% 
  mutate(doy = yday(date)) %>% 
  # filter(doy >= 152 & doy <= 244) %>% 
  summarise(max = max(temp_f), 
            range = max(temp_f) - min(temp_f)) %>% 
  ggplot(aes(x = date, y = max)) +
  geom_point(size = 1, alpha = 0.1)+ 
  theme_minimal()
  
