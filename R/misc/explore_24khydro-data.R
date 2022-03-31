# Explore available data in WHDPlus

library(tidyverse)

# Load WHDPlus data
load("Data/WHDPlus_data.Rdata")

# Overview
whd_base_atribs %>% glimpse()  # 33 x 172,865
whd_flow_temp %>% glimpse()    # 59 x 161,707
whd_watershed %>% glimpse()    # 122 x 161,937

### All organized around REACHID ###
whd_base_atribs %>% 
  filter(HYD_CAT == "stream") %>% 
  distinct(REACHID) %>% 
  tally()
 
# 24k Base attributes -----------------------------------------------

whd_base_atribs %>% glimpse()  # 33 x 172,865
# lat / long: centroid of stream
# Connectivity - dist. to lakes
# Elevation
# Sinuosity
# Gradient
 # Wet Width
# Stream order
# Network - total stream length

whd_base_atribs %>% 
  summarise(across(where(is.character), ~ length(unique(.x))))

whd_base_atribs %>% 
  group_by(HYD_CAT) %>% 
  tally()

# Filter out lakes and NA stream segments
whd_base_atribs_stms <- whd_base_atribs %>% 
  filter(HYD_CAT == "stream")

whd_base_atribs_stms %>% glimpse()

# Modeled flow and temp ----------------------------------------------

whd_flow_temp %>% glimpse()
# Natural Community class
# Temperature class
# Temp - modeled max daily
# Temp - modeled mean July
# Temp - modeled mean June-August
# Flow - exceedence flows
# Flow - June-Sep mean flow


# 24k watersheds -----------------------------------------------------
whd_watershed %>% glimpse()    
# watershed area
# bedrock depths (%)
# bedrock geology (%)
# surficial geology (%)
# LULC (%)
# climate
#  - precip 
#  - air temp
# ecoregion (west corn plains, north lakes/forest, north hardwoods, driftless, 
#   SE till plains, central corn)
# late / long: center of watershed
# soil permeability
# mean slope
# Ground water potential - Darcy value