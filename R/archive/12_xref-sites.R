# Linking fish sites to WHD 24k Hydro Plus (reachIDs)
# Bryan Maitland
# 2021-08-10
# 2021-08-25

# ==== Overview =============================================================

# FMDB surveys should each have a SWIMS station ID, 
# and then SWIMS stations should be snapped onto WHD and have hydroids. 
# BUT, there are tons of stations that were put in at confluences 
# so they snapped to multiple hydroids, or that were snapped 
# to a polygon instead of a flowline, etc 

# also, there are like 2 fmdb sites without SWIMS station IDs

# Objectives
# 1) use SWIMS ID to xwalk with swims/hydro/reach id crosswalk
# 2) use spatial analysis to get reachids for missing/erroneous
# 3) use spatial analysis to link HUC8-12s to sites

# TO DO 
# 1. 11 REACHIDs with 6s (16 sites; 51 surveys) so no WHD stream data
# 2. 10 REACHIDs with missing data (17 surveys)


# ==== Spatial Data ========================================================
df_surveys <- read_rds(here("data", "df_trout_surveys_cln.rds"))

wi_poly <- wi_poly %>% st_transform(crs = 3071)

# read 24k VA flowlines
st_layers(dsn = here("data","spatial","WDNR_HYDRO_24K_VA.gdb"))
lines_va <-
  st_read(dsn = here("data","spatial","WDNR_HYDRO_24K_VA.gdb"), 
          layer = "WD_HYDRO_VA_FLWLN_NTWRK_LN_24K") %>% 
  st_as_sf() %>%
  st_transform(crs = 3071) %>% 
  clean_names()
# does not have WBICs

# load 24k base flowlines to get WBICS and hydrotypes for hydroids (not in VA gdb)
st_layers(dsn = here("data","spatial","24k_Hydro_Copy_20210817.gdb"))
lines <-
  readOGR(
    dsn = here("data","spatial","24k_Hydro_Copy_20210817.gdb"),
    layer = "WD_HYDRO_FLOWLINE_LN_24K") %>%
  st_as_sf() %>%
  clean_names() %>%
  select(hydroid, hydrocode, hydrotype, river_sys_wbic, stream_order) %>%
  st_drop_geometry() %>%
  as_tibble()

# links wbics to the VA lines
lines_va <- left_join(lines_va, lines, by = "hydroid")

# load classified trout stream lines
lines_classed <- 
  here("data","spatial","shapefiles","hydro","trout_water_ln.shp") %>% 
  st_read() %>% 
  st_transform(crs = 3071) 


# read 24k VA catchements (HUC16s)
sf_huc16 <-
  st_read(dsn = here("data","spatial","WDNR_HYDRO_24K_VA.gdb"),
          layer = "WD_HYDRO_VA_CATCHMENT_AR_24K") %>%
  st_as_sf() %>%
  st_transform(crs = 3071) %>%
  clean_names()


# read watershed layers
sf_huc8 <- st_read(here("data","spatial","shapefiles","hucs","huc8.shp")) %>%clean_names() %>% st_transform(crs = 3071)
sf_huc10 <- st_read(here("data","spatial","shapefiles","hucs","huc10.shp")) %>%clean_names() %>% st_transform(crs = 3071)
sf_huc12 <- st_read(here("data","spatial","shapefiles","hucs","huc12.shp")) %>%clean_names() %>% st_transform(crs = 3071)

sf_ecoreg <- st_read(here("data","spatial","shapefiles","ecoregions","wi_eco_l3.shp")) %>%clean_names() %>% st_transform(crs = 3071)

sf_wtrmgnt <- st_read(here("data","spatial","shapefiles","dnr_water_mgnt","dnr_water_mgmt_unit_ar.shp"))



# quick plots
ggplot() + 
  geom_sf(data = lines_classed) + 
  ggspatial::annotation_scale()


# ===== hydroid-reachid-SWIMSid crosswalk table =========================

# xwalk table of all swims_station_ids in SWIMS and their hydro and reach IDS
# some SWIMS stations have multiple hydroids and reach ids
# - some with multiple 6s (on edge between two features)
# - some with 2s and 6s
# - some with multiples (site on a confluence)
# - some with NAs (station is not snapped)
# - Namekegon issue: while river is a lake with 6 reach id

xwalk_swims_hydro <- 
  here("data","xref_swims_hydro.rds") %>% 
  read_rds() %>% 
  # remove hydro and reachs with a 6 (lakes); we only want stream data
  filter(!str_detect(reach_id, "^6")) %>%
  # if a station has more than two records after removing 6s, get rid of them
  # we use spatail joins to get these
  group_by(swims.station.id) %>% 
  filter(!n() > 1) %>% 
  ungroup()
 


# ===== Trout site data =======================================================

length(unique(df_surveys$site.seq.no))

# read in site locations
df_sites <- df_surveys %>%
  distinct(site.seq.no, .keep_all = TRUE) %>% 
  select(site.seq.no, swims.station.id, wbic, latitude, longitude) %>%
  mutate(across(c(swims.station.id,site.seq.no), as.character)) 
# %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   st_transform(crs = 3071)  # CRS of the 24k hydro lines

df_sites %>% write_rds(here("output","data","df_sites.rds"))







# ==== Stream class Cross Reference =====================================================

# xwalk for wbic and trout class
xwalk_wbic_strmclass <- 
  lines_classed %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  clean_names() %>% 
  distinct(wbic, .keep_all = TRUE) %>% 
  select(wbic, trout_clas)

xwalk_wbic_strmclass %>% write_csv(here("data","xwalk_wbic_strmclass.csv"))



# ==== Watershed Cross Reference =====================================================


# find points within polygons
sites_in_hucs8 <- st_join(sf_sites, sf_huc8, join = st_within) %>% st_drop_geometry() %>% select(site.seq.no, huc8_code)
sites_in_hucs10 <- st_join(sf_sites, sf_huc10, join = st_within) %>% st_drop_geometry() %>% select(site.seq.no, huc10_code)
sites_in_hucs12 <- st_join(sf_sites, sf_huc12, join = st_within) %>% st_drop_geometry() %>% select(site.seq.no, huc12_code)
sites_in_wtrmgnt <- st_join(sf_sites, sf_wtrmgnt, join = st_within) %>% st_drop_geometry() %>% select(site.seq.no, WMU_NAME, WMU_CODE)
sites_in_ecoreg <- st_join(sf_sites, sf_ecoreg, join = st_within) %>% st_drop_geometry() %>% select(site.seq.no, us_l3code, us_l3name)

# df_sites <- df_sites %>% left_join(sites_in_wtrmgnt, by = "site.seq.no")

# combine watershed info

xwalk_sites_hucs <- sf_sites %>% 
  st_drop_geometry() %>% 
  left_join(sites_in_hucs8, by = "site.seq.no") %>%
  left_join(sites_in_hucs10, by = "site.seq.no") %>%
  left_join(sites_in_hucs12, by = "site.seq.no") %>%
  left_join(sites_in_wtrmgnt, by = "site.seq.no") %>%
  left_join(sites_in_ecoreg, by = "site.seq.no") %>% 
  as_tibble()


xwalk_sites_hucs <-  xwalk_sites_hucs %>%  left_join(sites_in_ecoreg, by = "site.seq.no")

# write xwalk table to file
xwalk_sites_hucs %>% write_csv(here("data","xwalk_sites_hucs.csv"))

# clean up
rm(sites_in_hucs8); rm(sites_in_hucs10); rm(sites_in_hucs12); rm(sites_in_wtrmgnt); rm(sites_in_ecoreg)





# ==== 24k WHD Cross Refereence ================================================



# 1. merge sites with xwalk table ==============================================

# First, use xwalk table to get reachids for as many sites as possible

# now we do an inner join to link sites with a swims match in the xwalk
sites_xwalked_swims <- inner_join(sf_sites, xwalk_swims_hydro, by = "swims.station.id") 

# and an anti join to get the sites without a macth in the xwalk
sites_fix <-anti_join(sf_sites, xwalk_swims_hydro, by = "swims.station.id") 





# 2. spatial analysis ==========================================================

# identify nearest 3 whdVA flowlines to site and get their distances
nn_trace <- st_nn(sites_fix, lines_va, k = 3, returnDist = TRUE)

# extract the nn flowlines indexes and distances for each
nn_distances <- 
  bind_cols(index = unlist(nn_trace$nn), distance = unlist(nn_trace$dist))

# join sites to flowlines using the nearest neighbor
nn_join <- st_join(sites_fix, lines_va, join = nngeo::st_nn, k = 3)

# bind the distances to the nn_join
nn_join_full <-
  bind_cols(nn_join, nn_distances) %>%
  st_drop_geometry() %>%
  as_tibble() %>% 
  select(-confl2conflid, -traceid, -shape_length, -index) %>% 
  mutate(across(where(is.integer), as.character)) %>%
  mutate(wbic_match = river_sys_wbic == wbic)
nn_join_full

# save intermediary
saveRDS(nn_join_full, here("data","tmp","nn_join_full_20210919.rds"))
nn_join_full <- read_rds(here("data","tmp","nn_join_full_20210919.rds"))



# 3. filter and clean up data ==================================================

# if wbic matches, keep the hydro record closest to the point
sites_fix_matched <- 
  nn_join_full %>% 
  group_by(site.seq.no) %>% 
  mutate(rank = row_number(distance)) %>% 
  filter(rank==1 & wbic_match==TRUE) %>%
  select(site.seq.no, hydroid, reachid) 

# how many still not fixed?
anti_join(sites_fix, sites_fix_matched, by = "site.seq.no") %>% 
  count()
# 76

sites_fix_matched2 <- 
  nn_join_full %>% 
  filter(!site.seq.no %in% sites_fix_matched$site.seq.no) %>% 
  group_by(site.seq.no) %>% 
  mutate(rank = row_number(distance)) %>% 
  filter(rank==2 & wbic_match==TRUE) %>%
  select(site.seq.no, hydroid, reachid) 

# how many still not fixed?
anti_join(
  sites_fix, bind_rows(sites_fix_matched, sites_fix_matched2), by = "site.seq.no") %>%
  count()
# 4

# the final 46 hydroids need manual checks; wbics are prob messed up
manual_fixes <- 
  nn_join_full %>% 
  filter(!site.seq.no %in% sites_fix_matched$site.seq.no) %>% 
  filter(!site.seq.no %in% sites_fix_matched2$site.seq.no) %>% 
  group_by(site.seq.no) %>% 
  slice_min(distance) %>% 
  distinct(site.seq.no, .keep_all = TRUE) %>% # for one record with identicle distances
  write_csv(here("data","tmp","nn_manual_fixes.csv"))
manual_fixes %>% print(n=Inf) 
# do manual checks #

# load corrected data
manual_fixed <- read_csv(here("data","tmp","nn_manual_fixes_corrected.csv"))

# sites to remove
sites_to_remove <- 
  manual_fixed %>% 
  filter(is.na(reachid_updated)) %>%
  pull(site.seq.no)

sites_fix_matched3 <- 
  manual_fixed %>% 
  filter(!site.seq.no %in% sites_to_remove) %>% 
  select(-notes) %>% 
  mutate(across(where(is.double), as.character)) %>%
  select(site.seq.no, hydroid, reachid=reachid_updated)

# bind up all the checked fixes
sites_fix_matched_final <- 
  bind_rows(
    sites_fix_matched, 
    sites_fix_matched2, 
    sites_fix_matched3
  )

# add hydroids to the list of sites of fix
sites_fix_cln <- 
  sites_fix %>% 
  st_drop_geometry() %>% 
  select(-wbic) %>% 
  left_join(sites_fix_matched_final, by = "site.seq.no") 

# bind xwalked and fixed tables for final 
xwalk_sites_whd <- 
  sites_xwalked_swims %>%
  st_drop_geometry() %>% 
  select(-wbic) %>% 
  rename(hydroid=hydro_id, reachid=reach_id) %>% 
  bind_rows(sites_fix_cln) %>% 
  select(swims.station.id, site.seq.no, hydro_id=hydroid, reach_id=reachid) %>% 
  as_tibble()
xwalk_sites_whd

# should be 8 with NAs (remove these sites from data)
xwalk_sites_whd %>% filter(is.na(reach_id))
xwalk_sites_whd <- filter(xwalk_sites_whd, !is.na(reach_id))

# now check for 6s
xwalk_sites_whd %>% filter(str_detect(reach_id, "^6")) %>% distinct(reach_id)
# NEED TO FIGURE OUT WHAT TO DO WITH THESE decease good fish data

sixes <- xwalk_sites_whd %>% filter(str_detect(reach_id, "^6")) %>% pull(site.seq.no)

# how many surevys?
df_surveys %>% filter(site.seq.no %in% sixes)  # ~ 50

# 8 NAs
# 11 6s

# save xwalks ==================================================================

xwalk_sites_whd <- read.csv(here("data","xwalk_sites_whd.csv"))

# list of reach ids for Aaron Rusch for pulling WHDPlus data via SQL
xwalk_sites_whd %>% 
  distinct(reach_id) %>%
  write_csv(here("data","tmp","trout-reachids.csv"))

# save 
xwalk_sites_whd %>% 
  # select(-swims.station.id) %>% 
  saveRDS(here("output","data","xwalk_sites_whd.rds"))

# sites_final <- 
#   here("data","xwalk_sites_whd.csv") %>% 
#   read_csv() %>% 
#   mutate(across(c(reach_id), as.character)) %>% 
#   rename(reachid=reach_id)


sites_whdplus_4alex <- read_rds(here("data","tmp","sites_whdplus_4alex.rds"))
