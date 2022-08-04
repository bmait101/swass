
# WDNR GIS data

# Helpful stuff:
#  + WI state boarder and county boarder
#  + road layers
#  + WNHD layers (watersheds, flowlines and polys)
#  + FH layers (regulations, habitat projects)
#  + DG layers (springs)

# Packages
library(tidyverse)
library(sf)
library(arcgis.rest)  # query an ArcGIS Rest API
library(wdnr.gis)  # pulls spatial layers from WDNR ArcGIS Rest API

# See available sections wdnr.gis
list_sections()
list_services()

# helpful sections
list_layers(sections = "DG_HiCap")
list_layers(sections = "DG_Viewer")
list_layers(sections = "DG_Well_Driller")
list_layers(sections = "DW_Map_Cached")
list_layers(sections = "DW_Map_Dynamic")
list_layers(sections = "FM_Trout")

# services (also found separately in sections)
list_layers(services = "DG_HICAP_REVIEW_WTM_INT")
list_layers(services = "DG_SPRING_LOCATIONS_WTM_EXT")
list_layers(services = "DG_WATER_LVL_DNR_SITES_WTM_EXT")
list_layers(services = "DG_WATER_LVL_USGS_SITES_WTM_EXT")
list_layers(services = "DG_Water_Use_Hicap_Apps_WTM_Ext")
list_layers(services = "DG_Water_Use_Locations_WTM_Ext")
list_layers(services = "EN_Detailed_Basemap_WTM_Ext")
list_layers(services = "EN_Lt_Basic_Terrain_Basemap_WTM_Ext")
list_layers(services = "EN_SurfaceWater_WTM_Ext")
list_layers(services = "EN_WI_Land_Cover_WTM_Ext")
list_layers(services = "WY_Shoreland_Habitat_WTM_Ext")
list_layers(services = "WT_Natural_Community_Modeling_WTM_Ext")


# FM Trout Layers ==========================================================

# See what layers are available:
list_layers(sections = "FM_Trout")

# Trout habitat sites
trout_hab_sites <- 
  get_spatial_layer(list_urls(sections = "FM_Trout")[4])
glimpse(trout_hab_sites)

# Plot
ggplot(data = NULL) +
  geom_sf(data = wi_counties, color = "black", fill = "white") + 
  geom_sf(data = wi_poly, color = "black", alpha = 0, size = 1) + 
  geom_sf(data = trout_hab_sites, 
          aes(fill = factor(str_to_lower(TARGETSPECIES))), 
          shape = 21, color = "black", alpha = 0.8) + 
  labs(title = "Trout habitat improvement sites")


# Trout county-level regulations
trout_cnty_regs_100k <- 
  get_spatial_layer(list_urls(sections = "FM_Trout")[3])
glimpse(trout_cnty_regs_100k)

ggplot(data = NULL) +
  geom_sf(data = trout_cnty_regs_100k, 
          aes(fill = TROUT_REGS %>% as_factor())) + 
  scale_fill_manual(values = c("yellow", "green", "red")) + 
  labs(title = "County-level trout regulations", 
       fill = "Reg Type")



# classified trout water
trout_strms_class <- 
  sf::st_read("data/shapefiles/lines/Classified_Trout_Stream_Lines.shp")

trout_strms_class %>% 
  mutate(TROUT_CLAS = fct_recode(TROUT_CLAS, 
                                 "Class I" = "CLASS I", 
                                 "Class II" = "CLASS II", 
                                 "Class III" = "CLASS III")) %>% 
  ggplot() + 
  geom_sf(color = "dodgerblue") +
  geom_sf(data = eco_regions, alpha = 0) +
  # scale_color_manual(values = c("chartreuse3", "dodgerblue", "navyblue")) + 
  labs(x = "", y = "", color = "Stream Class") +
  # ggtitle("Wisconsin's classified trout streams") + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        title = element_text(hjust = 0.5))


# DG - HCW  layers ==================================================

# See what layers are available:
list_layers(sections = "DG_HiCap")

# Spring locations as surveyed by WGNHS between 2014 and 2017
springs_high <- 
  get_spatial_layer(list_urls(sections = "DG_HiCap")[4]) %>% 
  mutate(flow_group = "CFS > 1")
springs_mid <- 
  get_spatial_layer(list_urls(sections = "DG_HiCap")[5]) %>% 
  mutate(flow_group = ".25 < CFS < 1")
springs_low <- 
  get_spatial_layer(list_urls(sections = "DG_HiCap")[6]) %>% 
  mutate(flow_group = "CFS <= .25")

# Bind the layers together
springs <- rbind(springs_high, springs_mid, springs_low)

# PLot
ggplot() +
  geom_sf(data = wi_counties, color = "black", fill = "white") + 
  geom_sf(data = wi_poly, color = "black", alpha = 0, size = 1) + 
  geom_sf(data = springs, aes(color = flow_group)) + 
  scale_color_viridis_d() + 
  labs(color = "Discharge")

list_layers(sections = "DG_Viewer")
test <- get_spatial_layer(list_urls(sections = "DG_Viewer")[2])
list_layers(sections = "DG_Well_Driller")
test <- get_spatial_layer(list_urls(sections = "DG_Well_Driller")[1])
list_layers(sections = "DG_HiCap")
test <- get_spatial_layer(list_urls(sections = "DG_HiCap")[1])

glimpse(test)
# PLot
ggplot() +
  geom_sf(data = wi_counties, color = "black", fill = "white") + 
  geom_sf(data = wi_poly, color = "black", alpha = 0, size = 1) + 
  geom_sf(data = test) 



### Surface and GW withdrawal locations in WI

withdrawal_gw <- get_spatial_layer(list_urls(sections = "DG_HiCap")[28])
withdrawal_surf <- get_spatial_layer(list_urls(sections = "DG_HiCap")[29])
glimpse(withdrawal_surf)
# PLot
ggplot(data = NULL) +
  geom_sf(data = wi_counties, color = "black", fill = "white") + 
  geom_sf(data = wi_state, color = "black", alpha = 0, size = 1) + 
  geom_sf(data = dnr_swmims_flow) 



# DW Map Cached ======================================================

list_layers(sections = "DW_Map_Cached")

test <- get_spatial_layer(list_urls(sections = "DW_Map_Cached")[14])

glimpse(test)

ggplot() +
  geom_sf(data = wi_poly, color = "black", alpha = 0, size = 1) + 
  geom_sf(data = test) 


# 24k WHD ==================================================================

# 24k hydro geodatabase on disk
fgdb <- here::here("data","spatial","whd_24K","24K_Hydro.gdb")
# List all feature classes in a file geodatabase
subset(rgdal::ogrDrivers(), grepl("GDB", name))
fc_list <- rgdal::ogrListLayers(fgdb)
print(fc_list)



# map of WI ecoregions ======================================================

eco_regions <- 
  st_read("./data/shapefiles/Ecoregions_l3/wi_eco_l3.shp") %>% 
  mutate(US_L3CODE = as_factor(US_L3CODE), 
         US_L3CODE = fct_recode(US_L3CODE, 
           "W Corn Belt" = "47", 
           "N Lakes Forests" =  "50",
           "NC Hardwoods" = "51",
           "Driftless" = "52",
           "SW Till Plains" = "53",
           "C Corn Belt" = "54"), 
         US_L3CODE = fct_relevel(US_L3CODE, 
           "N Lakes Forests",
           "NC Hardwoods",
           "Driftless",
           "SW Till Plains",
           "C Corn Belt",
           "W Corn Belt"
           ))

eco_regions %>% 
  ggplot() + 
  geom_sf(aes(fill = US_L3CODE), alpha = 0.75) +
  geom_sf(data = wi_counties, alpha = 0) + 
  geom_sf_label(data = eco_regions, aes(label = US_L3CODE), 
                fontface = "bold") + 
  scale_fill_viridis_d("Eco Region") + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  labs(x = "", y = "",
       title = "Ecoregions of Wisconsin") 

eco_regions %>% 
  ggplot() + 
  geom_sf(aes(fill = US_L3CODE), alpha = 0.25) +
  scale_fill_viridis_d() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "none") + 
  labs(x = "", y = "") 





# Watersheds ======================================================

huc8_polys <- sf::st_read("Data/Spatial/Shapefiles/huc8_polys_wi.shp")

## HUC12 pull ----------------------------------------------------

# Get HUC12 codes
huc12_codes <- wdnr.gis::watershed_lookup %>% 
  filter(huc_level == "HUC_10") %>% 
  pull(huc_codes)

# Initialize output
huc12_polys <- list()

# For each HUC code, pull the watershed polygon
system.time(
  for (i in (1:length(huc12_codes))){
    huc12_poly <- get_watershed_layer(watershed_code = huc12_codes[i])
    huc12_polys[[i]] <- huc12_poly
    }
  )

# Combine layers into single df
(huc12_polys <- bind_rows(huc12_polys))
# 
# # Map HUC12s
ggplot(data = NULL) +
  geom_sf(data = gb_sheds) +
  geom_sf(data = wi_poly, alpha = 0, color = "black", size = 1) +
  labs(title = "WI Subbasins (HUC8)")
# st_write(huc12_polys, "Data/Spatial/Shapefiles/Polys/huc12_polys_wi.shp")

huc12_polys <- st_read("Data/Spatial/Shapefiles/HUC12/SDEDNR_EN_NRCS_WBD_HUC12_REGN_AR_24K.shp")



# DNR road layers --------------------------------------------------

roads_maj <- get_roads_layer(county = "dane", layer_type = "major_roads")

ggplot(data = NULL) +
  geom_sf(data = roads_maj)



# Map streams in Kickapoo Valley --------------------------------------

kick_shed <- get_watershed_layer(watershed_name = "Kickapoo")  # HUC8
# kick_lines <- get_hydro_layer(watershed_name = "Kickapoo", 
#                               layer_type = "lines")
kick_flines <- get_hydro_layer(watershed_name = "Kickapoo", 
                              layer_type = "flowlines")
# kick_polys <- get_hydro_layer(watershed_name = "Kickapoo",
#                               layer_type = "polygons")

ggplot(data = NULL) +
  geom_sf(data = kick_shed, color = "black", fill = "grey", alpha = 0) +
  geom_sf(data = kick_flines, color = "lightblue") + 
  geom_sf(data = huc12_polys %>% filter(HUC8_CODE == "07070006"), alpha = 0.1) +
  #geom_sf(data = kick_polys, color = "blue") + 
  labs(title = "Kickapoo Valley Streams")



# Map streams an fish sites in Dane County  ---------------------------------

fish_mon_sites <- get_fmdb_site_layer(county = "dane")
dane_lines <- get_hydro_layer(county = "dane", layer_type = "flowlines")
dane_polys <- get_hydro_layer(county = "dane", layer_type = "polygons")

ggplot(data = NULL) +
  geom_sf(data = dane_lines, color = "lightblue") + 
  geom_sf(data = dane_polys, color = "black", fill = "grey") +
  geom_sf(data = fish_mon_sites, shape = 21, color = "red") + 
  labs(title = "Dane County Streams")


