#==============================================================================#
# USGS flow sites
#==============================================================================#

# ----------------------------------------------------------------
# meta data
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
             EXTERNAL_ID = col_double(),
             EXTERNAL_NAME = col_character(),
             WBIC = col_double(),
             HYDRO_ID = col_double()
           )) %>% 
  janitor::clean_names()

# any sites without hydros?
xref_usgs %>% filter(is.na(hydro_id))



### need to spatial ref these NAs ####




# filter usgs sites by trout sites and link to WHD+
xref_usgs_VA <- 
  xref_usgs %>% 
  mutate(across(where(is.double), as.character)) %>%
  filter(hydro_id %in% xref_trout_sites$hydro_id) %>% 
  left_join(df_whdplus, by = c("hydro_id" = "reach_id"))


# plot usgs flow sites
xref_usgs_VA %>% 
  left_join(df_surveys %>% select(swims.station.id, geometry), 
            by = c("station_id" = "swims.station.id")) %>% 
  sf::st_as_sf(crs = 4326) %>% 
  ggplot() + 
  geom_sf(data = eco_regions, alpha = 0) +
  geom_sf(data = trout_streams, color = "blue", alpha = 1) +
  geom_sf(shape = 21, size = 2, color = "black", fill = "red", alpha = 0.75)  +
  labs(x = "", y = "", fill = "Eco Region") + 
  theme_minimal() 


# how many USGS sites in each region and each stream order
xref_usgs_VA %>% 
  mutate(across(c("c_eco"), as.character)) %>% 
  left_join(eco_regions, by = c("c_eco" = "US_L3CODE")) %>% 
  group_by(L3_KEY, stream_order) %>% 
  tally() %>% print(n = Inf)


# list catchids to get data for 
xref_usgs_catchids <- xref_usgs_VA %>% pull(external_id)
# paste(driftless, collapse=",") 

# ----------------------------------------------------------------
#  flow data
# ----------------------------------------------------------------


# open connection to USGS stream flow sqlite db:
con <- DBI::dbConnect(RSQLite::SQLite(), 
                      here::here("data", "stream_flow", "usgs_hydrodata.sqlite"))

# list tables
DBI::dbListTables(con)

flows <- tbl(con, "flows")
flows

# generate query
flows_filtered <- flows %>% 
  filter(site_number %in% xref_usgs_catchids)

# see query
flows_filtered %>% show_query()

# execute query and retrieve results
df_usgs_data <- flows_filtered %>% collect()

# link to hydro and summarize by date
df_usgs_data <- 
  df_usgs_data %>% 
  mutate(across(c(site_number), as.character))  %>% 
  left_join(xref_usgs_VA %>% 
              select(external_id, hydro_id), 
            by = c("site_number" = "external_id")) %>% 
  mutate(sample_date = lubridate::date(datetime)) %>% 
  group_by(hydro_id, site_number, sample_date) %>% 
  summarise(flow_cfs = mean(discharge_cfs), .groups = "drop")

# how many measurements per site
df_usgs_data %>% group_by(site_number) %>% tally() %>% print(n = Inf)


# plot filtered sites
xref_usgs_VA %>% 
  filter(external_id %in% df_usgs_data$site_number) %>% 
  left_join(df_surveys %>% select(swims.station.id, geometry), 
            by = c("station_id" = "swims.station.id")) %>% 
  sf::st_as_sf(crs = 4326) %>% 
  ggplot() + 
  geom_sf(data = eco_regions, alpha = 0) +
  geom_sf(data = trout_streams, color = "blue", alpha = 1) +
  geom_sf(shape = 21, size = 2, color = "black", fill = "red", alpha = 0.75)  +
  labs(x = "", y = "", fill = "Eco Region") + 
  theme_minimal() 
