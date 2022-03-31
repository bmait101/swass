
# Linking covariates to trout trend data
# Bryan M Maitland
# 4 Oct 2021


# Libraries
library(tidyverse)
library(here)
require(corrplot)



# Data ====================================================================

df_cpes_va <- readRDS(here("output","data","df_cpe_va.rds"))
df_covar_whd_std <- readRDS(here("output","data","df_covar_whd_std.rds"))
df_covar_dmt_std <- readRDS(here("output","data","df_covar_dmt_std.rds"))

# Link covariates to catch data ===========================================

df_drivers <- df_cpes_va %>% 
  rename(year = survey.year) %>% 
  left_join(df_covar_dmt_std, by = c("reach_id", "year")) %>% 
  left_join(df_covar_whd_std, by = c("reach_id")) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(
    year_f = factor(year), # year as a factor for ranef
    year_s = scale(year)[,1],  
    latitude_s = scale(latitude)[,1]
    ) %>% 
  relocate(c(year_f, year_s), .after=year)


# Covariate correlations ===============================================

names(df_drivers)

mtx.cov <- df_drivers %>% 
  select(31:ncol(df_drivers)) %>% 
  select(
    -trw_lu11_ag,
    -trw_lu11_forests,
    -trw_lu11_wetland,
    -trw_reachlen
    )

cov.cor <- cor(mtx.cov, use='pairwise.complete.obs')
corrplot::corrplot.mixed(cov.cor, upper='ellipse', tl.pos='lt',tl.srt=45)


# Remove correlated variables ===============================================

df_drivers <- df_drivers %>% 
  select(
    -trw_lu11_ag, 
    -trw_lu11_forests,
    -trw_lu11_wetland  
  )

df_drivers


map(df_drivers, ~sum(is.na(.)))


# Save new objects =============================================================

df_drivers %>% write_rds(here("output","data", "df_drivers.rds"))
df_drivers %>% write.csv(here("output","data", "df_drivers.csv"))


