
# Simple example of downloading FMDB data for Dane county

library(wdnr.fmdb)
library(tidyverse)

spp2pull <- c(
  "mottled_sculpin","white_sucker",
  "golden_redhorse","greater_redhorse", "river_redhorse","shorthead_redhorse",
  "brown_trout","brook_trout","rainbow_trout"
)

surveys <- get_fmdb_surveys(year = 2021, county = "dane")
efforts <- get_fmdb_efforts(year = 2021, county = "dane")
fish_df <- get_fmdb_fishraw(year = 2021, county = "dane", spp = spp2pull)

bnt_size_cpe <-
  fish_df %>%
  dplyr::filter(species == "brown_trout") %>%
  estimate_lengths() %>%
  length_to_length_bin(bin_vals = c(4,8,12)) %>%
  calc_cpe(surveys, efforts, .,
           grouping = c("county", "waterbody.name", "wbic", "survey.year",
                        "survey.seq.no", "length.bin"))
