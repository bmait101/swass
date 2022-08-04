
# Identify USGS gauge stations with long term temp and flow data

#Libraries
library(tidyverse)
library(dataRetrieval)
library(leaflet)
library(maps)
library(sf)
library(arcgis.rest)  
library(wdnr.gis) 


# Available data ----------------------------------------------
# Import a table of available parameters, period of record, and count for WI:
data_wi <- whatNWISdata(stateCd = "WI")
glimpse(data_wi)

# unique sites
n_distinct(data_wi$site_no)  # 23,333 unique monitoring sites in Wisconsin.

# Pull metadata for available WI data ---------------------------------

data_wi_streams <- 
  whatNWISdata(
    stateCd = "WI",  #  Wisconsin
    siteType = "ST",  # data for streams/rivers
    service = "dv",  # daily value data
    parameterCd = c(
      # "00010",  # temperature data
      "00060"  # discharge (flow) data
    ),
    statCd = c(
      "00001",  # max
      "00002",  # min
      "00003"  # mean
    )  
  ) %>%
  # rename the data type codes
  mutate(parm_cd = case_when(parm_cd == "00010" ~ "Temperature", 
                             parm_cd == "00060" ~ "Streamflow"), 
         stat_cd = case_when(stat_cd == "00001" ~ 'Max', 
                             stat_cd == "00002" ~ 'Min',
                             stat_cd == "00003" ~ 'Mean'))

data_wi_streams1 <- 
  data_wi_streams %>% 
  filter(end_date >= "1990-01-01") %>% 
  mutate(span = lubridate::interval(begin_date, end_date) %>% 
           lubridate::time_length(unit = "year") %>% 
           round(digits = 0))

data_wi_streams1 %>% ggplot(aes(end_date)) + geom_histogram()
data_wi_streams1 %>% ggplot(aes(begin_date)) + geom_histogram()
data_wi_streams1 %>% ggplot(aes(span)) + geom_histogram()

data_wi_streams1 <- 
  data_wi_streams1 %>% 
  mutate(span_group = case_when(span <= 2 ~ "1-2", 
                                span >= 3 & span < 5 ~ "3-5", 
                                span >= 6 & span <= 10 ~ "6-10", 
                                span >=11 ~ "11-27"))

# Tally stations by parameter
data_wi_streams %>% 
  group_by(parm_cd , stat_cd) %>% 
  tally()
# 384 stream flow
# 129 stream temperature

# Dynamic map  --------------------------------------------------

# create a color scale for leaflet by parameter
factpal <- colorFactor("viridis", data_wi_streams$parm_cd)

# render the map
data_wi_streams %>%
  # filter(parm_cd == "Streamflow") %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~dec_long_va,~dec_lat_va,
                   color = ~factpal(parm_cd),
                   radius=3, stroke=FALSE,
                   fillOpacity = 0.8, opacity = 0.8) %>%
  addMarkers(~dec_long_va,~dec_lat_va,label = ~htmlEscape(site_no)) %>% 
                   
  addLegend("bottomright", pal = factpal, values = ~parm_cd,
            title = "Parameter",
            opacity = 1)

# Static Map -----------------------------------------------------
# Make spatial
data_wi_streams_sf <- 
  st_as_sf(data_wi_streams, 
           coords = c("dec_long_va", "dec_lat_va"), 
           crs = 4326)  # WGS84 to match wdnr layers

# Plot
ggplot(data = NULL) + 
  geom_sf(data = wdnr.gis::wi_counties, fill = "white") + 
  geom_sf(data = data_wi_streams_sf, 
          aes(color = parm_cd), 
          shape = 21) + 
  scale_color_manual(values = c("blue","red"), 
                     guide = FALSE) + 
  facet_wrap(vars(parm_cd)) + 
  labs(title = "USGS Stream Gauges", subtitle = ">1 yr daily flow, 1990-2020", 
       x = "Longitude", y = "Latitude")


# Kickapoo test ===============================================

# Plot stations -----
# Get watershed and flowlines
kick_shed <- get_watershed_layer(watershed_name = "Kickapoo")
kick_lines <- get_hydro_layer(watershed_name = "Kickapoo", 
                              layer_type = "lines")
kick_flines <- get_hydro_layer(watershed_name = "Kickapoo", 
                               layer_type = "flowlines")

# Plot
ggplot(data = NULL) +
  geom_sf(data = kick_shed, color = "black", fill = "grey", alpha = 0.1) +
  geom_sf(data = kick_lines, color = "lightblue") + 
  geom_sf(data = data_wi_streams_sf) 

# Need to filter the points
data_wi_streams_sf[st_within(data_wi_streams_sf, kick_shed, sparse = FALSE),]
data_wi_streams_sf %>% filter(st_within(data_wi_streams_sf, kick_shed, sparse = FALSE))
x<- data_wi_streams_sf %>% filter(st_within(data_wi_streams_sf, kick_shed, sparse = FALSE))

x %>%
  as_tibble()

# Plot
ggplot(data = NULL) +
  geom_sf(data = kick_shed, color = "black", fill = "grey", alpha = 0.1) +
  geom_sf(data = kick_flines, aes(color = STREAM_ORDER)) + 
  geom_sf(data = x, color = "red", size = 3) 

# Now pull the data -------

# Sites
sites <- "05408480"
# Paramters
pCodes <- "00060"

# Pull data
wideMulti <- readNWISdv(sites, pCodes) 

wideMulti <- renameNWISColumns(wideMulti)

wideMulti <- select(wideMulti, -ends_with("_cd")) %>% as_tibble()

# Get metadata
siteInfo <- attr(wideMulti, "siteInfo")
paramInfo <- attr(wideMulti, "variableInfo")

levels(longMulti$variable) <- paramInfo$param_units
levels(longMulti$site_no) <- siteInfo$station_nm

gp <- ggplot(wideMulti, 
             aes(Date, Flow, color=site_no)) +
  geom_line(size=1.5) + 
  xlab("") + ylab("") 
gp


# Pull the flow and temp data -------------------------------------------

glimpse(data_wi_streams)

