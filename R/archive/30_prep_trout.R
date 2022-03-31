# Prep trout data for modeling 

# Prep

library(tidyverse)
library(here)


# ===== Fish and survey data =====================================================


# read cleaned data

df_surveys <- readRDS(here("data", "df_trout_surveys_cln.rds"))
df_efforts <- readRDS(here("data", "df_trout_efforts_cln.rds"))
df_trout_yoy_lengthed <- readRDS(here("data", "df_trout_data.rds"))
df_fish <- readRDS(here("data", "df_fish_cln.rds"))
sf_sites <- readRDS(here("data", "sf_sites.rds"))


# load xwalks

xwalk_sites_hucs <- read_csv(here("data","xwalk_sites_hucs.csv")) %>%  mutate(across(c(site.seq.no), as.character))
xwalk_sites_whd <- read_csv(here("data","xwalk_sites_whd.csv"))%>%  mutate(across(c(site.seq.no, reach_id), as.character)) %>% distinct(site.seq.no, .keep_all = TRUE)
xwalk_wbic_strmclass <- read_csv(here("data","xwalk_wbic_strmclass.csv"))%>%  mutate(across(c(wbic), as.character))


# prepare main trout data frame

df_trout_clean <- df_trout_yoy_lengthed %>% 
  as_tibble() %>% 
  mutate(across(c(site.seq.no, wbic), as.character)) %>% 
  left_join(xwalk_sites_hucs %>% select(-swims.station.id, -wbic), by = "site.seq.no") %>%
  left_join(xwalk_sites_whd %>% distinct(site.seq.no, .keep_all = TRUE), by = "site.seq.no") %>%
  left_join(xwalk_wbic_strmclass %>% mutate(across(c(wbic), as.character)), by = "wbic") %>% 
  select(
    site.seq.no, trout_clas, ecoregion=us_l3code, econame=us_l3name,county, 
    huc8=huc8_code, huc10=huc10_code, huc12=huc12_code, WMU_NAME, WMU_CODE, 
    waterbody.name, wbic, reach_id, hydro_id, swims.station.id,
    year=survey.year, sample.date, gear, survey.seq.no, visit.fish.seq.no, 
    species, yoy, number.of.fish, length, length.bin, length.unit, length.est,
    survey.status, visit.type, target.species, primary.survey.purpose
  ) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(across(c(reach_id, hydro_id, site.seq.no), as.factor)) 
df_trout_clean

# get rid of reachid NAs
df_trout_clean <- df_trout_clean %>% filter(!is.na(reach_id))


# need to subset data to legit sources of long-term relative density

df_trout_clean %>% count(primary.survey.purpose) %>% arrange(desc(n)) %>% print(n= Inf)

targs.survey.purpose.cpe <- c(
  "fisheries_assessments_trout_trend","fisheries_assessments_trout_rotation","baseline_monitoring"
)

df_trout_clean <- df_trout_clean %>% 
  filter(primary.survey.purpose %in% targs.survey.purpose.cpe) %>% 
  left_join(wdnr.gis::watershed_lookup %>% filter(huc_level == "HUC_8"), by = c("huc8"="huc_codes")) %>% 
  select(-huc_level)


# Calculate total catch and cpes   =============================================


# df of total efforts
total_effort <- df_efforts %>% 
  mutate(wbic = as.character(wbic)) %>%
  rename(year = survey.year) %>%
  group_by(visit.fish.seq.no) %>% 
  summarize(total_effort = sum(distance.shocked), .groups = "drop") 

# df of total catch (number of fish)
df_cpes <- df_trout_clean %>% 
  mutate(wbic = as.character(wbic)) %>% 
  group_by(year, sample.date, site.seq.no, county, ecoregion, WMU_NAME, WMU_CODE, huc_names, huc8, huc10, huc12, waterbody.name, wbic, reach_id, survey.seq.no, visit.fish.seq.no, gear, species, yoy) %>% 
  summarize(total_catch = sum(number.of.fish),
            mean_length = mean(length), 
            max_length = max(length),
            .groups = "drop") %>%
  
  left_join(total_effort, by =  c("visit.fish.seq.no")) %>% 
  mutate(cpe = total_catch/total_effort) %>% 
  
  left_join(df_surveys %>% select(survey.seq.no, latitude, longitude, primary.survey.purpose), by = "survey.seq.no")  %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(across(c(ecoregion, reach_id, site.seq.no, survey.seq.no, visit.fish.seq.no), as.factor)) %>% 
  # filter(! (year == 1994 & gear == "stream_shocker" & yoy == "Y")) %>% 
  filter(! (year == 2020 & gear == "stream_shocker"))




# Save new objects =============================================================
saveRDS(df_cpes, here("output","data","df_cpe.rds"))
write.csv(df_cpes,  here("output","data","df_cpe.csv"))







