# Stream trout CPEs for Emma L.

library(tidyverse)
library(here)
library(wdnr.gis)

# trout cpes 
trout_cpe <- read_rds(here("data","df_cpe.rds"))
df_surveys <- read_rds(here("data", "df_trout-surveys-cln.rds"))  

# make spatial 
trout_cpe <- trout_cpe %>% 
  left_join(df_surveys %>%
              distinct(site.seq.no, .keep_all = TRUE) %>%  
              select(site.seq.no, latitude, longitude),
            by = "site.seq.no") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 3071)

# get huc layer
huc <- 
  get_watershed_layer(watershed_code = "0403010503") %>% 
  st_transform(huc, crs = 3071)

trout_cpe_clip <- trout_cpe %>% 
  st_intersection(huc)

# plot
ggplot() + 
  geom_sf(data = huc, fill = "white") +
  geom_sf(data = trout_cpe_clip, color = "red") 

# write to file with lat and long
trout_cpe_clip %>% 
  st_drop_geometry() %>% 
  left_join(df_surveys %>%
              distinct(site.seq.no, .keep_all = TRUE) %>%  
              select(site.seq.no, latitude, longitude),
            by = "site.seq.no") %>% 
  write_csv(here("data","tmp","trout_cpes_huc_0403010503.csv"))
