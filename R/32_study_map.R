
# Libraries
library(tidyverse)
library(lubridate)
library(here)
library(cowplot)
library(ragg)
library(sf)

# load prepped data
source(here::here("R", "50_prep_data.R"))


# Maps of study sites ==========================================================


# df of sites
df_sites <- new.dat %>%
  distinct(site.seq.no, .keep_all = TRUE) %>% 
  # select(site.seq.no, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 3071)  # CRS of the 24k hydro lines

lines_classed <- 
  here("data","spatial","shapefiles","hydro","trout_water_ln.shp") %>% 
  st_read() %>% 
  st_transform(crs = 3071) 

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) 

region <- 
  st_read(here("data","spatial","shapefiles","borders","great_lakes_shoreline_ar.shp")) %>% 
  st_transform(crs = 3071) 
region_water <- region %>% 
  filter(FEAT_TYPE_=="water")
region_state <- region %>% 
  filter(FEAT_TYPE_=="land") %>% 
  filter(FEAT_NAME %in% c("Michigan","Minnesota","Iowa","Illinois"))
region_wi <- region %>%
  filter(FEAT_TYPE_=="land") %>% 
  filter(FEAT_NAME %in% c("Wisconsin"))

miss <- st_read(here("data","spatial","shapefiles","hydro","mississippi-river.shp")) %>% 
  janitor::clean_names() %>% 
  st_transform(crs = 3071) %>% 
  st_intersection(region_wi)

sf_huc4 <- st_read(here("data","spatial","shapefiles","hucs","Major_Basins.shp")) %>% 
  janitor::clean_names() %>% 
  st_transform(crs = 3071) %>% 
  st_intersection(region_wi)

sf_huc8 <- st_read(here("data","spatial","shapefiles","hucs","huc8.shp")) %>% 
  janitor::clean_names() %>% 
  st_transform(crs = 3071) %>% 
  st_intersection(region_wi)

# drift <- st_read(here("data","spatial","shapefiles",
#                       "driftless","FHP_DARE_Boundary_2013.shp")) %>% 
#   janitor::clean_names() %>% 
#   st_transform(crs = 3071)
# 
# sites_in_drift <- df_sites %>% 
#   st_intersection(drift) %>% 
#   st_drop_geometry() %>% 
#   pull(site.seq.no)



# check plot
ggplot() + 
  # geom_sf(data = drift) +
  geom_sf(data = sf_huc8, alpha=0, size=0.1, color="grey30") +
  geom_sf(data = df_sites, shape=16, size=1.5) +
  theme_bw(base_size = 12, base_family = "sans")


# sites map
p.sites <- 
  ggplot() + 
  geom_sf(data=wdnr.gis::wi_poly, color=NA, fill=NA) +
  geom_sf(data=region_state, color="black", fill = "grey95", size=.25) +
  geom_sf(data=region_water, fill="skyblue", color="black",  size=.25) +
  geom_sf(data=region_wi, fill="grey95", color="black",  size=.25) +
  geom_sf(data=sf_huc8, alpha=1, size=0.1, color = "black", fill = "grey60") +
  geom_sf(data=sf_huc4, alpha=0, size=.75, color = "black") +
  geom_sf(data=lines_classed, color="skyblue", size=.4) +
  geom_sf(data=df_sites, shape=21, fill = "white", color="black", size=1.7) +
  coord_sf(xlim = c(-93.2,-86.5), ylim = c(42.5,47)) +
  ggspatial::annotation_scale(
    location = "bl", width_hint = 0.2,
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in")) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.3, "in"), pad_y = unit(.6, "in")) + 
  theme_bw(base_size = 12, base_family = "sans")
p.sites

# inset map
p.inset <- 
  ggplot() + 
  geom_sf(data = states, fill = "white", color = "black", size = 0.25) + 
  geom_sf(data = states %>% filter(ID=="wisconsin"), 
          fill = "grey50", color = "black", size = 0.25) + 
  theme_void() + 
  theme(
    plot.background = element_rect(color = "black", fill = "white"),
  )
p.inset


# full map
p.full.map <- 
  ggdraw() +
  draw_plot(p.sites) +
  draw_plot(p.inset, x = 0.65, y = 0.76, width = 0.25, height = 0.25)
p.full.map

ggsave(here("output","figs","fig1_map_trout_sites.png"), p.full.map,
       device=agg_png, res=300, width = 6, height = 5)
ggsave(here("output","figs","fig1_map_trout_sites.pdf"), p.full.map,
       device=cairo_pdf, height = 8, width = 7)




# sites map drfit
p.sites.dft <- 
  ggplot() + 
  geom_sf(data=wdnr.gis::wi_poly, color=NA, fill=NA) +
  geom_sf(data=region_state, color="black", fill="grey70", size=.25) +
  geom_sf(data=region_water, fill="lightblue", color="black",  size=.25) +
  geom_sf(data=region_wi, fill="grey90", color="black",  size=.25) +
  geom_sf(data=sf_huc8, alpha=0, size=0.1, color="grey30") +
  geom_sf(data=lines_classed, color="dodgerblue1", size=.4) +
  geom_sf(data = df_sites %>% filter(!site.seq.no %in% sites_in_drift), 
          shape=21, fill = "grey40", size=1.5, color="black") +
  geom_sf(data = df_sites %>% filter(site.seq.no %in% sites_in_drift),
          shape=21, fill = "red", size=1.5, color="black") +
  coord_sf(xlim = c(-93.2,-86.5), ylim = c(42.5,47)) +
  ggspatial::annotation_scale(
    location = "bl", width_hint = 0.2,
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in")) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.3, "in"), pad_y = unit(.6, "in")) + 
  theme_bw(base_size = 12, base_family = "sans")
p.sites.dft


ggsave(here("output","figs","map_trout_sites_drift.png"), p.sites.dft,
       device=agg_png, res=300, width = 8, height = 7)







# troutu streams map
p.streams <- 
  ggplot() + 
  geom_sf(data=wdnr.gis::wi_poly, color="black", fill=NA) +
  # geom_sf(data=sf_huc8, alpha=1, size=0.1, color = "black", fill = "grey60") +
  # geom_sf(data=sf_huc4, alpha=0, size=.75, color = "black") +
  geom_sf(data=lines_classed, color="skyblue", size=.4) +
  theme_void()
p.streams


ggsave(here("output","figs","map_trout_streams.png"), p.streams,
       device=agg_png, res=300, width = 6, height = 7)



