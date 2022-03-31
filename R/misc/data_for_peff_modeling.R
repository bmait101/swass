
# ID sites for building effective precip models


# for each ecoregion, for each stream order that trout are present in, 
# select up to top 10 catchids with most flow observations


library(tidyverse)


# rm(list=setdiff(ls(), "lines")) 
# ls() 


#==============================================================================#
# trout surveys, regions, streams, and watershed attributes
#==============================================================================#

# ----------------------------------------------------------------
# read data 
# ----------------------------------------------------------------

# swims station xwalk for linking 24k data
xref_trout_sites <- 
  read_csv(here::here("data", "xwalk_trout_stations.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.double), as.character)) %>% 
  mutate(reach_id = if_else(is.na(reach_id), hydro_id, reach_id)) %>% 
  distinct(hydro_id, .keep_all = TRUE) %>% 
  select(-station_id) 

# trout surveys 
df_surveys <-
  readRDS(here::here("data", "fish", "fmdb_surveys_cln_trout.rds")) %>%
  as_tibble() %>%
  # keep unique and make spatial
  distinct(swims.station.id, .keep_all = TRUE) %>%
  mutate(across(c(longitude, latitude), as.double)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# ecoregions
eco_regions <- 
  sf::st_read(here::here("data","spatial","Ecoregions_l3","wi_eco_l3.shp"))

# trout streams
trout_streams <- 
  sf::st_read(here::here("data","spatial","lines","Classified_Trout_Stream_Lines.shp")) %>% 
  janitor::clean_names() %>% 
  group_by(trout_clas) %>% 
  summarise(length_miles = sum(water_ty_1))

# 24K VA hydro data
df_whdplus <- 
  read_csv(here::here("data", "whdplus_bryan.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(across(c(reachid), as.character)) %>% 
  distinct(reachid, .keep_all = TRUE) %>% 
  select(reach_id = reachid, river_sys_wbic, c_eco, c_huc12, hydrotype,
         gradient, c_length, stream_order, w_area, w_perm, 
         w_lu11_wat = w_lu11_11, w_lu11_hay = w_lu11_81, w_lu11_crops = w_lu11_82,
         w_lu11_urb_open = w_lu11_21, w_lu11_urb_low = w_lu11_22, 
         w_lu11_urb_med = w_lu11_23, w_lu11_urb_high = w_lu11_24, 
         w_lu11_wetldwoody = w_lu11_90, w_lu11_wetldherb = w_lu11_95)


# load 24k HYDRO flowlines
lines <- 
  rgdal::readOGR(here::here("data", "spatial", "whd_24k", "24K_Hydro.gdb"), 
                 "WD_HYDRO_FLOWLINE_LN_24K") %>% 
  sf::st_as_sf() %>% 
  janitor::clean_names()


#==============================================================================#
# USGS flow sites
#==============================================================================#

# ----------------------------------------------------------------
# meta data and spatial xref for hydroids
# ----------------------------------------------------------------

# read usgs flow sites metadata
xref_usgs <- 
  read_csv("data/stream_flow/USGS_xref_HYDROID.csv", 
           col_types = cols(
             MONIT_STATION_SEQ_NO = col_number(),
             STATION_ID = col_double(),
             PRIMARY_STATION_NAME = col_character(),
             STATION_STATUS_CODE = col_character(),
             CALC_LL_LAT_DD_AMT = col_double(),
             CALC_LL_LONG_DD_AMT = col_double(),
             EXTERNAL_ID = col_character(),
             EXTERNAL_NAME = col_character(),
             WBIC = col_double(),
             HYDRO_ID = col_double()
           )) %>% 
  janitor::clean_names() %>%
  filter(nchar(external_id) == 8) %>% 
  distinct(external_id, .keep_all = TRUE)

  
xref_usgs %>% distinct(external_id)
xref_usgs %>% filter(is.na(hydro_id))
xref_usgs %>% filter(str_detect(hydro_id, "^6")) 

# about 35 NAs and 228 6s extra rows... 

### need to spatial ref these NAs and 6s ####


#-----------------------------------------------------------------------------#
# get proper coordinates
#-----------------------------------------------------------------------------#

library(dataRetrieval)
siteNumbers <- xref_usgs %>% pull(external_id)
siteINFO <- 
  readNWISsite(siteNumbers) %>% 
  select(external_id = site_no, dec_lat_va, dec_long_va)
siteINFO %>% head(10)

xref_usgs <- 
  xref_usgs %>% 
  left_join(siteINFO, by = "external_id") 

# any sites without coords?
xref_usgs %>% filter(is.na(dec_long_va))  # 2. one on Missippi, and just delete the other. 

# get rid of gult site and those two w/o coords, and sites outsite of WI (rock river and 4 in MI)
xref_usgs <- 
  xref_usgs %>% filter(!is.na(dec_long_va)) %>% 
  filter(external_id != "02378181") %>% 
  filter(!external_id %in% c("05437500", "04108660", "04120250", "04122150", "04122500", "04057005"))
  
xref_usgs %>% write_csv(here::here("data", "tmp", "usgs_coords.csv"))

#static map
xref_usgs %>% 
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269) %>% 
  sf::st_transform(crs = 3071) %>% 
  ggplot() +
  geom_sf(data = eco_regions %>% sf::st_transform(crs = 4269), alpha = 0) + 
  geom_sf(shape = 21, size = 2, color = "black", fill = "red", alpha = 0.75)

# dynamic map (points object CANNOT be sf)
labels <- sprintf(as.character(points$external_id)) %>% lapply(htmltools::HTML)
leaflet::leaflet(data = points) %>%
  leaflet::addPolygons(data = eco_regions %>% sf::st_transform(crs = 4269)) %>%
  leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet::addCircleMarkers(~dec_long_va,~dec_lat_va,
                   color = "black", radius=3, stroke=FALSE,
                   fillOpacity = 0.8, opacity = 0.8,
                   label=labels)


# make stations spatial 
xref_usgs_sf <- 
  xref_usgs %>% 
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269) %>% 
  sf::st_transform(crs = 3071)

#-----------------------------------------------------------------------------#
# NN analysis - NAs
#-----------------------------------------------------------------------------#

xref_usgs_sf_NAs <- filter(xref_usgs_sf, is.na(hydro_id)) 


points <- xref_usgs_sf_NAs


# get nearest 2 lines to station and their distances
nn_trace <- nngeo::st_nn(points, lines, k = 1, returnDist = TRUE)

# join stations to flow lines using the nearest neighbor, get distance
nn_join <- sf::st_join(points, lines, join = nngeo::st_nn, k = 1)

# extract the flow lines indexes and distances for each
nn_distances <-bind_cols(
  index = unlist(nn_trace$nn),
  distance = unlist(nn_trace$dist))

# check it
bind_cols(nn_join, nn_distances) %>% 
  sf::st_drop_geometry() %>% 
  select(external_id, wbic, river_sys_wbic, hydroid, distance)

# clean it up and filter out correct data
nn_join_full <- 
  bind_cols(nn_join, nn_distances) %>% 
  sf::st_drop_geometry() %>% 
  select(primary_station_name, external_id, wbic, river_sys_wbic, hydroid, distance) %>% 
  mutate(across(c(hydroid,wbic,river_sys_wbic), as.character))  %>%
  mutate(wbic_match = river_sys_wbic == wbic) 
  # group_by(external_id) %>%
  # slice_min(distance) %>%
  # ungroup()

# check df
nn_join_full

# see which wbic don't match up after slicing minimum
nn_join_full %>% filter(wbic_match == FALSE) 
# 5 mismtch; so filter out these, slice min, then add corrected ones bacl

# all of these need to be snapped in SWIMS
nn_join_full <-
  nn_join_full %>% 
  mutate(hydroid = case_when(
    external_id == "04087133" ~ 200024432, 
    external_id == "05405857" ~ 200035642,
    external_id == "05405859" ~ 200035642,
    external_id == "05406360" ~ 200023376, 
    external_id == "05544386" ~ 200015804, 
    TRUE ~ as.double(hydroid)
    )) %>% 
  mutate(across(c(hydroid), as.character))


#-----------------------------------------------------------------------------#
# Aggregate  
#-----------------------------------------------------------------------------#

xref_usgs_sf_NAs_cln <- 
  nn_join_full %>% 
  ungroup() %>% 
  select(external_id, hydroid) %>% 
  # now we need the reach ids
  left_join(xref_trout_sites, by = c("hydroid" = "hydro_id")) %>% 
  # give the NA reach ids the hydro id
  mutate(reach_id = if_else(is.na(reach_id), hydroid, as.character(reach_id)))


# ===============================================================================#

# Deal with 6s in hydroids

# ===============================================================================#

xref_usgs_sf_6s <-
  xref_usgs_sf %>%
  filter(!external_id %in% xref_usgs_sf_NAs_cln$external_id) %>%
  filter(str_detect(hydro_id, "^6")) 
# 83 that have 6 for hydros

#-----------------------------------------------------------------------------#
# NN anlaysis
#-----------------------------------------------------------------------------#


points <- xref_usgs_sf_6s

# get nearest 2 lines to station and their distances
nn_trace <- nngeo::st_nn(points, lines, k = 1, returnDist = TRUE)

# join stations to flow lines using the nearest neighbor, get distance
nn_join <- sf::st_join(points, lines, join = nngeo::st_nn, k = 1)

# extract the flow lines indexes and distances for each
nn_distances <-bind_cols(
  index = unlist(nn_trace$nn),
  distance = unlist(nn_trace$dist))

# check it
bind_cols(nn_join, nn_distances) %>% 
  sf::st_drop_geometry() %>% 
  select(external_id, wbic, river_sys_wbic, hydroid, distance)

# clean it up and filter out correct data
nn_join_full <- 
  bind_cols(nn_join, nn_distances) %>% 
  sf::st_drop_geometry() %>% 
  select(primary_station_name, external_id, wbic, river_sys_wbic, hydroid, distance) %>% 
  mutate(across(c(hydroid,wbic,river_sys_wbic), as.character))  %>%
  mutate(wbic_match = river_sys_wbic == wbic) 
# group_by(external_id) %>%
# slice_min(distance) %>%
# ungroup()

# check df
nn_join_full

# see which wbic don't match up after slicing minimum
nn_join_full %>% filter(wbic_match == FALSE) %>% print(n = Inf)
# 5 mismtch; so filter out these, slice min, then add corrected ones bacl

# all of these need to be snapped in SWIMS
nn_join_full <-
  nn_join_full %>% 
  mutate(hydroid = case_when(
    external_id == "05371920" ~ 200109538, 
    external_id == "05436010" ~ 200017420, 
    external_id == "05423500" ~ 200051122, 
    external_id == "05414000" ~ 200211242, 
    external_id == "05369500" ~ 200116004, 
    external_id == "05341500" ~ 200151187, 
    external_id == "04077400" ~ 200211113, 
    external_id == "05379400" ~ 200086094, 
    external_id == "04084411" ~ 200082628, 
    TRUE ~ as.double(hydroid)
  )) %>% 
  mutate(across(c(hydroid), as.character))




#-----------------------------------------------------------------------------#
# Aggregate  
#-----------------------------------------------------------------------------#

xref_usgs_sf_6s_cln <- 
  nn_join_full %>% 
  ungroup() %>% 
  select(external_id, hydroid) %>% 
  # now we need the reach ids
  left_join(xref_trout_sites, by = c("hydroid" = "hydro_id")) %>% 
  # give the NA reach ids the hydro id
  mutate(reach_id = if_else(is.na(reach_id), hydroid, as.character(reach_id)))


# ===============================================================================#
# Update 
# ===============================================================================#


xref_usgs_cln <- 
  
  xref_usgs %>% 
  select(external_id, hydro_id) %>% 
  
  filter(!external_id %in% xref_usgs_sf_NAs_cln$external_id) %>% 
  filter(!external_id %in% xref_usgs_sf_6s_cln$external_id) %>% 
  mutate(reach_id = hydro_id) %>% 
  mutate(across(where(is.double), as.character)) %>% 
  bind_rows(xref_usgs_sf_NAs_cln %>% rename(hydro_id = hydroid)) %>% 
  bind_rows(xref_usgs_sf_6s_cln %>% rename(hydro_id = hydroid))
xref_usgs_cln

xref_usgs_cln %>% filter(is.na(hydro_id))





# ===============================================================================#
# filter usgs sites by trout sites and link to WHD+
# ===============================================================================#



# filter usgs sites by trout sites and link to WHD+
xref_usgs_cln_VA <- 
  xref_usgs_cln %>% 
  filter(hydro_id %in% xref_trout_sites$hydro_id) %>% 
  left_join(df_whdplus, by = c("hydro_id" = "reach_id"))


# plot usgs flow sites on trout streams
xref_usgs_cln_VA %>% 
  left_join(siteINFO, by = "external_id") %>% 
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269) %>% 
  sf::st_transform(crs = 3071) %>% 
  ggplot() +
  geom_sf(data = eco_regions %>% sf::st_transform(crs = 4269), alpha = 0, size = 1) + 
  geom_sf(data = trout_streams, color = "blue", alpha = 1) +
  geom_sf(shape = 21, size = 2, color = "black", fill = "red", alpha = 0.75) +
  labs(title = "USGS Stations on Trout Streams",
       x = "", y = "", fill = "Eco Region") + 
  theme_minimal() 

ggsave(here::here("plots", "usgs_sites_on_trout_streams.png"), height = 4, width = 4)

# how many USGS sites in each region and each stream order
xref_usgs_cln_VA %>% 
  mutate(across(c("c_eco"), as.character)) %>% 
  left_join(eco_regions, by = c("c_eco" = "US_L3CODE")) %>% 
  group_by(L3_KEY, stream_order) %>% 
  tally() %>% print(n = Inf)


# list catchids to get data for 
xref_usgs_catchids <- 
  xref_usgs_cln_VA %>% 
  mutate(across(c(external_id), as.integer)) %>% 
  pull(external_id)


# ===============================================================================#
# load usgs flow data
# ===============================================================================#

# open connection to USGS stream flow sqlite db:
con <- DBI::dbConnect(RSQLite::SQLite(), 
                      here::here("data", "stream_flow", "usgs_hydrodata.sqlite"))

# list tables
DBI::dbListTables(con)

flows <- tbl(con, "flows")
flows

# generate query
flows_filtered <- 
  flows %>% 
  filter(site_number %in% xref_usgs_catchids)

# see query
flows_filtered %>% 
  show_query()

# execute query and retrieve results
df_usgs_data <- 
  flows_filtered %>% 
  collect()

# how many sites have flow data?
length(unique(df_usgs_data$site_number))  # 41 of the 186 usgs stations

# link to hydro and summarize by date
df_usgs_data_final <- 
  df_usgs_data %>% 
  # mutate(across(c(site_number), as.character))  %>% 
  left_join(xref_usgs_cln_VA %>% 
              select(external_id, hydro_id) %>% 
              mutate(across(c(external_id), as.integer)), 
            by = c("site_number" = "external_id")) %>% 
  mutate(sample_date = lubridate::date(datetime)) %>% 
  group_by(hydro_id, site_number, sample_date) %>% 
  summarise(flow_cfs = mean(discharge_cfs), .groups = "drop") %>% 
  rename(external_id = site_number) %>% 
  relocate(hydro_id, .after = external_id)

# how many measurements per site
df_usgs_data %>% group_by(external_id) %>% tally() %>% print(n = Inf)

# filter metatdata by the sites with flow data
xref_usgs_cln_VA_final <- 
  xref_usgs_cln_VA %>% 
  mutate(across(c(external_id), as.integer)) %>% 
  filter(external_id %in% df_usgs_data$external_id) 


# plot filtered sites
xref_usgs_cln_VA_final  %>% 
  left_join(siteINFO %>% mutate(across(c(external_id), as.integer)),
            by = "external_id") %>% 
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269) %>% 
  sf::st_transform(crs = 3071) %>% 
  ggplot() +
  geom_sf(data = eco_regions %>% sf::st_transform(crs = 4269), alpha = 0, size = 1) + 
  geom_sf(data = trout_streams, color = "blue", alpha = 1) +
  geom_sf(shape = 21, size = 2, color = "black", fill = "red", alpha = 0.75) +
  labs(title = "USGS Stations on Trout Streams with flows",
       x = "", y = "", fill = "Eco Region") + 
  theme_minimal() 

ggsave(here::here("plots", "usgs_sites_on_trout_streams_w_flows.png"), 
       height = 4, width = 4)




# ----------------------------------------------------------------
#  gage height data
# ----------------------------------------------------------------
# library(dataRetrieval)
# 
# sites <- read_csv("data/stream_flow/USGS_xref_HYDROID.csv", 
#          col_types = cols(
#            MONIT_STATION_SEQ_NO = col_number(),
#            STATION_ID = col_double(),
#            PRIMARY_STATION_NAME = col_character(),
#            STATION_STATUS_CODE = col_character(),
#            CALC_LL_LAT_DD_AMT = col_double(),
#            CALC_LL_LONG_DD_AMT = col_double(),
#            EXTERNAL_ID = col_character(),
#            EXTERNAL_NAME = col_character(),
#            WBIC = col_double(),
#            HYDRO_ID = col_double()
#          )) %>% 
#   janitor::clean_names()
# 
# siteNumber <- 
#   df_usgs_data %>% 
#   distinct(site_number)
#   mutate(site_number = paste0("0", site_number)) %>% 
#   pull(site_number)
# 
# siteNumber <- "01491000"
# parameterCd <- "00065"
# startDate <- "2010-01-01"  
# endDate <- "2019-01-01" 
# 
# gage_height <- readNWISdv(siteNumber, parameterCd, startDate, endDate)




#==============================================================================#
# SWIMS flow sites
#==============================================================================#

# ----------------------------------------------------------------
# metadata
# ----------------------------------------------------------------

# read usgs ref metadata
xref_swims <- 
  read_csv(here::here("data", "stream_flow", "flow_data_total_swims.csv"), 
           col_types = cols(
             DATA_SOURCE = col_character(),
             HYDRO_ID = col_double(),
             FLOW_CFS = col_double(),
             LATITUDE = col_double(),
             LONGITUDE = col_double(),
             SAMPLE_DATE = col_date(format = ""),
             STATION_ID = col_double()
           )) %>% 
  janitor::clean_names() %>% 
  distinct(station_id, .keep_all = TRUE)  %>% 
  mutate(across(c(station_id, hydro_id), as.character))  %>% 
  select(-data_source, -flow_cfs, -sample_date)
  

# filter on trout hydros and link whd+
xref_swims_VA_final <- 
  xref_swims %>% 
  filter(hydro_id %in% xref_trout_sites$hydro_id) %>% 
  left_join(df_whdplus, by = c("hydro_id" = "reach_id"))

# any hydro NAs?
xref_swims_VA %>% filter(is.na(hydro_id))  # NO!

# how many USGS sites in each region and each stream order
xref_swims_VA %>% 
  mutate(across(c("c_eco"), as.character)) %>% 
  left_join(eco_regions, by = c("c_eco" = "US_L3CODE")) %>% 
  group_by(US_L3NAME, stream_order) %>% 
  tally() %>% print(n = Inf)


# plot swims flow sites
xref_swims_VA %>% 
  left_join(df_surveys %>% select(swims.station.id, geometry), 
            by = c("station_id" = "swims.station.id")) %>% 
  sf::st_as_sf(crs = 4326) %>% 
  ggplot() + 
  geom_sf(data = eco_regions, alpha = 0) +
  geom_sf(data = trout_streams, color = "blue", alpha = 1) +
  geom_sf(shape = 21, size = 2, color = "black", fill = "red", alpha = 0.75)  +
  labs(title = "SWIMS Stations on Trout Streams with flows",
       x = "", y = "", fill = "Eco Region") +  theme_minimal() 


ggsave(here::here("plots", "swims_flow_sites_on_trout_streams.png"), 
       height = 4, width = 4)


# ----------------------------------------------------------------
# flow data
# ----------------------------------------------------------------

# load and filter swims flows by trout hydros
df_swims_data_final <- 
  read_csv(here::here("data", "stream_flow", "flow_data_total_swims.csv"), 
           col_types = cols(
             DATA_SOURCE = col_character(),
             HYDRO_ID = col_double(),
             FLOW_CFS = col_double(),
             LATITUDE = col_double(),
             LONGITUDE = col_double(),
             SAMPLE_DATE = col_date(format = ""),
             STATION_ID = col_double()
           )) %>% 
  janitor::clean_names() %>% 
  mutate(across(c(hydro_id,station_id), as.character)) %>% 
  filter(hydro_id %in% xref_trout_sites$hydro_id) %>% 
  select(hydro_id, sample_date, flow_cfs, station_id)
df_swims_data_final


# how many measurements per site
df_swims_data_final %>% group_by(station_id) %>% tally() %>% print(n = Inf)



#==============================================================================#
# save data to file
#==============================================================================#


df_flow_all <- 
  bind_rows(
    
    df_usgs_data_final %>% 
      mutate(across(c(external_id), as.character)) %>% 
      rename(station_id = external_id) %>% 
      mutate(source = "USGS"),
    
    df_swims_data_final %>% 
      relocate(station_id, .after = hydro_id) %>% 
      mutate(source = "SWIMS")
  
  )

write_csv(df_flow_all, here::here("data", "stream_flow", "peff_flow_data.csv"))
  

xref_all <- 
  bind_rows(
    xref_usgs_cln_VA_final %>% 
      mutate(across(c(external_id), as.character)) %>% 
      rename(station_id = external_id) %>% 
      mutate(source = "USGS") %>% 
      select(source, station_id, hydro_id, 
             river_sys_wbic, c_eco, c_huc12, hydrotype,
             gradient, c_length, stream_order, w_area, w_perm, 
             w_lu11_wat, w_lu11_hay, w_lu11_crops,
             w_lu11_urb_open, w_lu11_urb_low, w_lu11_urb_med, w_lu11_urb_high, 
             w_lu11_wetldwoody, w_lu11_wetldherb),
    
    xref_swims_VA_final %>% 
      mutate(source = "SWIMS") %>% 
      select(source, station_id, hydro_id, 
             river_sys_wbic, c_eco, c_huc12, hydrotype,
             gradient, c_length, stream_order, w_area, w_perm, 
             w_lu11_wat, w_lu11_hay, w_lu11_crops,
             w_lu11_urb_open, w_lu11_urb_low, w_lu11_urb_med, w_lu11_urb_high, 
             w_lu11_wetldwoody, w_lu11_wetldherb)
    
    )

write_csv(xref_all, here::here("data", "stream_flow", "peff_xref.csv"))



# test link
df_flow_all %>% left_join(xref_all, by = "station_id")







