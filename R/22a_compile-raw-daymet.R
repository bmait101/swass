# Compile raw DAYMET data for trout analysis
# Bryan Maitland


# libraries
library(tidyverse)
library(here)
library(rhdf5)  # R interface for HDF5 files


# Initials =====================================================================

# set path to .h5 file
f_h5 <- here("data","daymet","daymet_upstream_prcp_tmin_tmax_by_catchid.h5")

# list groups in h5 file
h5ls(f_h5)

# get indexes for dates and set targets
dymt_dates <- 
  h5read(f_h5, "col_index") %>% 
  as_tibble()
taregt_dates <- dymt_dates

# get indexes for catchids, filter by trouch reaches, then set targets  
dymt_catchids <- 
  h5read(f_h5, "row_index") %>% 
  as_tibble() 

trout_reach_ids <- read_rds(here("output","data","df_cpe_va.rds")) %>% 
  distinct(reach_id) %>% pull()

taregt_ids <-  filter(dymt_catchids, catchid %in% trout_reach_ids) 


# Compile data =================================================================

start.time <- Sys.time() # (~ 15 min)

# PRCP-------------------------------------------------------------------

# read in the prcp data (takes ~ 2 min)
prcp <- 
  h5read(f_h5, "prcp", index = list(
    as.integer(taregt_ids$grid_row), 
    as.integer(taregt_dates$grid_col)))

# tidy data and join with dates
prcp_cln <- prcp %>% 
  as_tibble() %>% 
  mutate(
    catchid = taregt_ids$catchid) %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "grid_col",
    names_prefix = "V",
    values_to = "prcp"
  ) %>% 
  left_join(
    taregt_dates %>% mutate(across(c(grid_col), as.character)), 
    by = "grid_col"
  ) %>% 
  select(catchid, date, prcp) 


# PEFF----------------------------------------------------------

# read in the prcp data (takes ~ 3.5 min)
peff <- 
  h5read(f_h5, "peff", index = list(
    as.integer(taregt_ids$grid_row), 
    as.integer(taregt_dates$grid_col)))

# tidy data and join with dates
peff_cln <- peff %>% 
  as_tibble() %>% 
  mutate(
    catchid = taregt_ids$catchid) %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "grid_col",
    names_prefix = "V",
    values_to = "peff"
  ) %>% 
  left_join(
    taregt_dates %>% mutate(across(c(grid_col), as.character)), 
    by = "grid_col"
  ) %>% 
  select(catchid, date, peff) 


# TMAX------------------------------------------------------


# read in the prcp data (takes ~3.5 min)
tmax <- 
  h5read(f_h5, "tmax", index = list(
    as.integer(taregt_ids$grid_row),
    as.integer(taregt_dates$grid_col)))

# tidy data and join with dates
tmax_cln <- tmax %>% 
  as_tibble() %>% 
  mutate(
    catchid = taregt_ids$catchid) %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "grid_col",
    names_prefix = "V",
    values_to = "tmax"
  ) %>% 
  left_join(
    taregt_dates %>% mutate(across(c(grid_col), as.character)), 
    by = "grid_col"
  ) %>% 
  select(catchid, date, tmax) 


# TMIN---------------------------------------------------------


# read in the prcp data (takes ~ 3.5 min)
tmin <- 
  h5read(f_h5, "tmin", index = list(
    as.integer(taregt_ids$grid_row), 
    as.integer(taregt_dates$grid_col)))

# tidy data and join with dates
tmin_cln <- tmin %>% 
  as_tibble() %>% 
  mutate(
    catchid = taregt_ids$catchid) %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "grid_col",
    names_prefix = "V",
    values_to = "tmin"
  ) %>% 
  left_join(
    taregt_dates %>% mutate(across(c(grid_col), as.character)), 
    by = "grid_col"
  ) %>% 
  select(catchid, date, tmin)


# Compile-------------------------------------------------------------------


dymt_comp <- prcp_cln %>% 
  left_join(tmin_cln, by = c("catchid", "date")) %>% 
  left_join(tmax_cln, by = c("catchid", "date"))
dymt_comp


# write to file
dymt_comp %>% write_rds(here("data","daymet","daymet_compiled.rds"))


# Clean--------------------------------------------------------------------

dymt_cln <- dymt_comp %>% 
  mutate(date = lubridate::date(date)) %>% 
  mutate(
    tmax = tmax / 10, 
    tmin = tmin / 10, 
    prcp = prcp / 10
    ) %>% 
  mutate(
    tmean = (tmax + tmin) / 2
  ) %>%
  select(catchid, date, tmax, tmin, tmean, prcp)


# export to file
dymt_cln %>% write_rds(here("data","daymet","daymet_cln.rds"))

end.time <- Sys.time()
(time.taken <- end.time - start.time)


# ================    END    =============================================
