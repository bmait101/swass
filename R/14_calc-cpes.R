
# Summarize trout data to counts / cpe by effort
# Bryan Maitland
# Created: 2021-12-15
# Last Updated: 2021-12-22

library(tidyverse)
library(here)

# Fish and survey data =========================================================

df_surveys <- readRDS(here("output", "data", "df_surveys.rds"))
df_efforts <- readRDS(here("output", "data", "df_efforts.rds"))
df_trout_yoy <- readRDS(here("output", "data", "df_trout_yoy.rds"))
df_sites_va <- read_rds(here("output","data","sites_list_va.rds"))


# Calculate total catch   ======================================================


## tibble of total efforts
total_effort <- df_efforts %>% 
  group_by(visit.fish.seq.no) %>% 
  summarize(total_effort = sum(distance.shocked), .groups = "drop") 
# total_effort <- df_efforts %>% select(visit.fish.seq.no, distance.shocked) 
# same thing


## tibble of total catch by age class - effort level
df_cpes <- df_trout_yoy %>% 
  group_by(waterbody.name, survey.year, survey.seq.no, visit.fish.seq.no, 
           species, yoy) %>% 
  summarize(total_catch = sum(number.of.fish), .groups = "drop") 

## add cpe column
df_cpes <- df_cpes %>% 
  left_join(total_effort, by =  c("visit.fish.seq.no")) %>% 
  mutate(cpe = total_catch/total_effort)


## add back in important metatdata
df_cpes <- df_cpes %>% 
  left_join(df_surveys %>% 
              select(survey.seq.no, wbic, site.seq.no, latitude, longitude, 
                     primary.survey.purpose), 
            by = "survey.seq.no")  %>% 
  left_join(df_efforts %>% 
              select(visit.fish.seq.no, sample.date, julian, gear), 
            by = "visit.fish.seq.no") 


# Check for NAs in CPE =========================================================

map(df_cpes, ~sum(is.na(.)))
length(unique(df_cpes$visit.fish.seq.no))  # should match effort data - 13,582 efforts
length(unique(df_cpes$survey.seq.no))  # should match survey data - 12,532 efforts


## Link to xref'ed site data =============================================

df_cpes_va <- df_cpes %>% 
  mutate(site.seq.no=as.character(site.seq.no)) %>% 
  left_join(
    df_sites_va %>% 
      select(-swims.station.id, -wbic, -latitude, -longitude), 
            by = "site.seq.no"
    )


# Check for NAs ===========================================================

map(df_cpes_va, ~sum(is.na(.)))
df_cpes_va %>% filter(is.na(reach_id))

# Save new objects =========================================================

write_rds(df_cpes_va, here("output","data","df_cpe_va.rds"))
# write_csv(df_cpes_va,  here("output","data","df_cpe_va.csv"))



# END =====================================================================