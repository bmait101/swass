
library(tidyverse)
library(here)
library(janitor)

# Data ============================================

df_cpes_va <- read_rds(here("output","data","df_cpe_va.rds"))

# load 24K VA hydro
whdplus <- read_csv(here("data", "whd", "whdplus2.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(across(c(reachid), as.character)) 

# Check for missing WHDPlus data ============================

length(unique(df_cpes_va$reach_id)) - length(unique(whdplus$reachid))

anti_join(df_cpes_va, whdplus, by = c("reach_id" = "reachid"))
# 24 records
# 3 reach ids missing from WHD data
# only two records are marginally important - so we'll call this done. 

# Prep data ======================================

# select columns
df_whdplus_covars <- whdplus %>% 
  mutate(trw_lu11_forests = trw_lu11_41+trw_lu11_42+trw_lu11_43) %>% 
  mutate(trw_lu11_wetland = trw_lu11_90+trw_lu11_95) %>% 
  mutate(trw_lu11_ag = trw_lu11_81+trw_lu11_82) %>% 
  select(-trw_lu11_41, -trw_lu11_42, -trw_lu11_43, -trw_lu11_90, -trw_lu11_95, 
         -trw_lu11_81, -trw_lu11_82) %>% 
  select(
    reach_id = reachid, 
    # w_area, 
    # w_slope,
    stream_order, 
    gradient,
    trw_reachlen, 
    trw_lu11_forests, 
    trw_lu11_wetland,
    trw_lu11_ag
    )

names(df_whdplus_covars)

# plot hostorgrams of all the covariates
df_whdplus_covars %>% 
  pivot_longer(-reach_id, names_to="var", values_to="value") %>% 
  ggplot(aes(value)) +
  geom_histogram() + 
  facet_wrap(vars(var), scales = 'free')


# Mean-variance (z) standardize the covariates
df_whdplus_covars_std <- df_whdplus_covars %>% 
  mutate(across(2:ncol(df_whdplus_covars), ~(scale(.) %>% as.vector)))

write_rds(df_whdplus_covars, here("output","data","df_covar_whd.rds"))
write_rds(df_whdplus_covars_std, here("output","data","df_covar_whd_std.rds"))
