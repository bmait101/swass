# Compile DAYMET data for Black Earth Creek for Dan Oele
# Bryan Maitland, Mar 2021


# libraries
library(tidyverse) 
library(wdnr.gis)
library(janitor)
library(rhdf5)
library(wdnr.fmdb)
set_fmdb_credentials()


# streams, watersheds ======================================================

watershed_lookup %>% filter(str_detect(huc_names, "Black Earth"))

huc_codes <- 
  watershed_lookup %>% 
  filter(huc_names == "Black Earth Creek") %>% 
  pull(huc_codes)

poly <- get_watershed_layer(watershed_code = huc_codes)
streams <- get_hydro_layer(watershed_code = huc_codes)

# plot to check
ggplot() +
  geom_sf(data = poly, fill = "white", color = "black") +
  geom_sf(data = streams, color = "blue") 

# # get huc12 codes for black earth
# huc_codes <- 
#   watershed_lookup %>% 
#   filter(huc_level == "HUC_12") %>% 
#   filter(str_detect(huc_codes, "0707000505")) %>% 
#   pull(huc_codes)
# 
# # download polys
# huc12_polys <-
#   huc_codes %>%  
#   map_df(~ get_watershed_layer(watershed_code = .x)) %>%
#   clean_names() 
# 
# huc10_poly <- 
#   huc12_polys %>% 
#   mutate(huc10 = "black earth creek") %>% 
#   group_by(huc10) %>% 
#   summarise()
# 
# # download stream lines for broadest extend, then clip to HUC10
# streams <-
#   get_hydro_layer(county = c("dane","iowa"), layer_type = "lines") %>% 
#   clean_names() %>% 
#   mutate(across(c(hydroid), as.character)) %>% 
#   st_intersection(huc10_poly)
# 
# # plot to check
# ggplot() +
#   geom_sf(data = huc_polys, fill = "white", color = "black") +
#   geom_sf(data = st_intersection(streams, huc10_poly), color = "blue") 

# fish surveys ==================================================================

# set argument parameters for data pull
wbics <- streams$river_sys_wbic 
years <- c(1980:2020)
waterbody_types <- c( "wadable_stream","non-wadable_stream","stream")
gear_types <- c("stream_shocker","backpack_shocker")

# pull data from fmdb
df_surveys <- 
  get_fmdb_surveys(
    year = years, 
    wbic = wbics,
    waterbody_type = waterbody_types
  ) 

# check it
df_surveys 


# list distinct sites and then make them spatial
df_surveys_sites <- 
  df_surveys %>% 
  distinct(site.seq.no, .keep_all = TRUE) %>% 
  sf::st_as_sf(
    coords = c("longitude", "latitude"), 
    crs = 4326
  )

# plot it
ggplot() + 
  geom_sf(data = poly, fill = "white", color = "black") +
  geom_sf(data = streams, color = "blue") +
  geom_sf(
    data = df_surveys_sites, 
    shape = 21, size = 2, color = "black", fill = "green", alpha = 0.75
    ) + 
  ggtitle("BEC Trout survey sites")


# spatial join for hydroids  ====================================================

points <- df_surveys_sites

# get nearest 3 lines to station and their distances
nn_trace <- nngeo::st_nn(points, streams, k = 2, returnDist = TRUE)

# join stations to flow lines using the nearest neighbor, get distance
nn_join <- st_join(points, streams, join = nngeo::st_nn, k = 2)

# extract the flow lines indexes and distances for each
nn_distances <-bind_cols(
  index = unlist(nn_trace$nn),
  distance = unlist(nn_trace$dist))

# clean it up and filter out correct data
nn_join_full <- 
  bind_cols(nn_join, nn_distances) %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, wbic, river_sys_wbic, hydroid, distance) %>% 
  mutate(across(where(is.integer), as.character))  %>% 
  mutate(wbic_match = river_sys_wbic == wbic) %>% 
  group_by(site.seq.no) %>%
  filter(wbic_match == TRUE) %>% 
  slice_min(distance) %>% 
  ungroup()

# check df
nn_join_full

# make sure everything got a hydro
points %>% 
  as_tibble() %>% 
  mutate(across(c("site.seq.no"), as.character)) %>% 
  anti_join(nn_join_full, by = "site.seq.no")

# pull out list of hydroids
target_hydros <- 
  nn_join_full %>% 
  pull(hydroid) 



# daymet indexes for all data  ==================================================

# set path to .h5 file
f_h5 <- "data/daymet_upstream_prcp_tmin_tmax_by_catchid.h5"

# list groups in h5 file
h5ls(f_h5)

# get indexes for catchids and dates
catchids <- 
  h5read(f_h5, "row_index") %>% 
  as_tibble() 

dates <- 
  h5read(f_h5, "col_index") %>% 
  as_tibble()

# check them 
head(catchids)
head(dates)


# set target catchids  ========================================================


# index for catchids
taregt_ids <- 
  catchids %>% 
  filter(catchid %in% target_hydros)

# index dates
taregt_dates <- 
  dates %>% 
  mutate(date = lubridate::as_date(date)) %>% 
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date))

#==============================================================================
# PEFF
#==============================================================================

# read in the prcp data 
peff <- 
  h5read(
    f_h5, 
    "peff", 
    index = list(
      as.integer(taregt_ids$grid_row), 
      as.integer(taregt_dates$grid_col)
    )
  )

# print first 30 days (January) for one catchid
peff[1,1:30]

# tidy data and join with dates
peff_cln <- 
  peff %>% 
  as_tibble() %>% 
  mutate(
    catchid = taregt_ids$catchid) %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "grid_col",
    names_prefix = "V",
    values_to = "peff"
  ) %>% 
  left_join(
    taregt_dates %>% mutate(across(c(grid_col), as.character)), 
    by = "grid_col"
  ) %>% 
  select(catchid, date, year, month, peff) %>% 
  mutate(across(c(catchid), as.character)) %>% 
  mutate(peff = peff/10)

# check it
peff_cln


# plot prcp ===================================================================

peff_cln %>%
  group_by(catchid, year) %>% 
  summarise(mean_prcp = sum(peff)) %>% 
  ggplot(aes(x = year, y = mean_prcp/25.4)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Year", y = "Precipitaton (in)") +
  ggtitle("Annual total of daily precipitation, Black Earth Creek, WI") + 
  theme_classic()


#==============================================================================
# PRCP
#==============================================================================

# read in the prcp data 
prcp <- 
  h5read(
    f_h5, 
    "prcp", 
    index = list(
      as.integer(taregt_ids$grid_row), 
      as.integer(taregt_dates$grid_col)
    )
  )

# print first 30 days (January) for one catchid
prcp[1,1:30]

# tidy data and join with dates
prcp_cln <- 
  prcp %>% 
  as_tibble() %>% 
  mutate(
    catchid = taregt_ids$catchid) %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "grid_col",
    names_prefix = "V",
    values_to = "prcp"
  ) %>% 
  left_join(
    taregt_dates %>% mutate(across(c(grid_col), as.character)), 
    by = "grid_col"
  ) %>% 
  select(catchid, date, year, month, prcp) %>% 
  mutate(across(c(catchid), as.character)) %>% 
  mutate(prcp = prcp/10)

# check it
prcp_cln


# plot prcp ===================================================================

prcp_cln %>%
  group_by(catchid, year) %>% 
  summarise(mean_prcp = sum(prcp)) %>% 
  ggplot(aes(x = year, y = mean_prcp/25.4)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Year", y = "Precipitaton (in)") +
  ggtitle("Annual total of daily precipitation, Black Earth Creek, WI") + 
  theme_classic()



#==============================================================================
# TMAX
#==============================================================================

# read in the prcp data 
tmax <- 
  h5read(
    f_h5, 
    "tmax", 
    index = list(
      as.integer(taregt_ids$grid_row), 
      as.integer(taregt_dates$grid_col)
    )
  )

# print first 30 days (January) for one catchid
tmax[1,1:30]

# tidy data and join with dates
tmax_cln <- 
  tmax %>% 
  as_tibble() %>% 
  mutate(
    catchid = taregt_ids$catchid) %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "grid_col",
    names_prefix = "V",
    values_to = "tmax"
  ) %>% 
  left_join(
    taregt_dates %>% mutate(across(c(grid_col), as.character)), 
    by = "grid_col"
  ) %>% 
  select(catchid, date, year, month, tmax) %>% 
  mutate(across(c(catchid), as.character)) %>% 
  mutate(tmax = tmax/10)

# check it
tmax_cln


# plot prcp ===================================================================

tmax_cln %>%
  group_by(catchid, year) %>% 
  summarise(mean_prcp = mean(tmax)) %>% 
  ggplot(aes(x = year, y = mean_prcp)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Year", y = "Temp (C)") +
  ggtitle("Annual mean of daily max temp, Black Earth Creek, WI") + 
  theme_classic()


#==============================================================================
# TMAX
#==============================================================================

# read in the prcp data 
tmin <- 
  h5read(
    f_h5, 
    "tmin", 
    index = list(
      as.integer(taregt_ids$grid_row), 
      as.integer(taregt_dates$grid_col)
    )
  )

# print first 30 days (January) for one catchid
tmin[1,1:30]

# tidy data and join with dates
tmin_cln <- 
  tmin %>% 
  as_tibble() %>% 
  mutate(
    catchid = taregt_ids$catchid) %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "grid_col",
    names_prefix = "V",
    values_to = "tmin"
  ) %>% 
  left_join(
    taregt_dates %>% mutate(across(c(grid_col), as.character)), 
    by = "grid_col"
  ) %>% 
  select(catchid, date, year, month, tmin) %>% 
  mutate(across(c(catchid), as.character)) %>% 
  mutate(tmin = tmin/10)

# check it
tmin_cln


# plot prcp ===================================================================

tmin_cln %>%
  group_by(catchid, year) %>% 
  summarise(mean_prcp = mean(tmin)) %>% 
  ggplot(aes(x = year, y = mean_prcp)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Year", y = "Temp (C)") +
  ggtitle("Annual mean of daily min temp, Black Earth Creek, WI") + 
  theme_classic()



prcp_cln
tmin_cln
tmax_cln

daymet_cln <- 
  prcp_cln %>% 
  left_join(tmin_cln, by = c("catchid", "date", "year", "month")) %>% 
  left_join(tmax_cln, by = c("catchid", "date", "year", "month"))

daymet_cln
