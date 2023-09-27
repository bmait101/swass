
# Maps of study sites ==========================================================

# Libraries
library(tidyverse)
library(here)
library(cowplot)
library(ragg)
library(sf)


# Data ===============================================

# load prepped data
source(here::here("R", "30_prep_final.R"))


# df of sites
df_sites <- df_analysis %>%
  distinct(site.seq.no, .keep_all = TRUE) %>% 
  # select(site.seq.no, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 3071)  # CRS of the 24k hydro lines

# read in streams
lines_classed <- 
  here("data","spatial","shapefiles","hydro","trout_water_ln.shp") %>% 
  st_read() %>% 
  st_transform(crs = 3071) 

# load states
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) 

# read in GL shorelines
region <- 
  st_read(here("data","spatial","shapefiles","borders","great_lakes_shoreline_ar.shp")) %>% 
  st_transform(crs = 3071) 
# subsets
region_water <- region %>% 
  filter(FEAT_TYPE_=="water")
region_state <- region %>% 
  filter(FEAT_TYPE_=="land") %>% 
  filter(FEAT_NAME %in% c("Michigan","Minnesota","Iowa","Illinois"))
region_wi <- region %>%
  filter(FEAT_TYPE_=="land") %>% 
  filter(FEAT_NAME %in% c("Wisconsin"))

# Mississippi river - read in and crop
miss <- st_read(here("data","spatial","shapefiles","hydro","mississippi-river.shp")) %>% 
  janitor::clean_names() %>% 
  st_transform(crs = 3071) %>% 
  st_intersection(region_wi)

# Read in HUCs and crop
sf_huc4 <- st_read(here("data","spatial","shapefiles","hucs","Major_Basins.shp")) %>% 
  janitor::clean_names() %>% 
  st_transform(crs = 3071) %>% 
  st_intersection(region_wi)

sf_huc8 <- st_read(here("data","spatial","shapefiles","hucs","huc8.shp")) %>% 
  janitor::clean_names() %>% 
  st_transform(crs = 3071) %>% 
  st_intersection(region_wi)


# Map ===========================================================

# quick plot to check layers
ggplot() + 
  geom_sf(data = sf_huc8, alpha=0, size=0.1, color="grey30") +
  geom_sf(data = df_sites, shape=16, size=1.5) +
  theme_bw(base_size = 12, base_family = "sans")

# main panel
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


# save plot
path <- here::here("output","figs1","fig1_map")
ggsave(
  glue::glue("{path}.pdf"), 
  plot = p.full.map, 
  width = 6, 
  height = 5, 
  device = cairo_pdf
)

# convert
pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  format = "png", 
  dpi = 600
)

