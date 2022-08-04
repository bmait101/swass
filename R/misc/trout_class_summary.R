# summarize WHD flow lines and trout streams


# prep
library(tidyverse)
library(sf)
library(here)
library(rgdal)

# flowlines
lines <-
  readOGR(
    dsn = here("data","spatial","24k_Hydro_Copy_20210817.gdb"),
    layer = "WD_HYDRO_FLOWLINE_LN_24K") %>%
  st_as_sf() 

lines_simple <- st_simplify(lines)

# classified torut water flow lines
trout_lines <- 
  st_read(here::here("data","spatial","shapefiles","hydro","trout_water_ln.shp")) %>% 
  st_transform(crs = 3071)

# plot
ggplot() + 
  geom_sf(data = wdnr.gis::wi_poly, color = "black", fill = "white") + 
  geom_sf(data = trout_lines, aes(color = TROUT_CLAS)) + 
  theme_minimal()

# total stream length in miles
trout_lines %>% 
  st_drop_geometry() %>% 
  summarise(total_length = sum(SHAPE_LEN)/1609)

# total stream length in miles by class
trout_lines %>% 
  st_drop_geometry() %>% 
  group_by(TROUT_CLAS) %>% 
  summarise(total_length = sum(SHAPE_LEN)/1609)

length(unique(trout_lines$WBIC))
