#========================================#
# Compile WDNR 24k WHD Plus data 
# Dr. Bryan M. Maitland
# 2022-Aug-03
#========================================#


## Set up ----

# libraries
library(tidyverse)
library(lubridate)
library(here)

## Data ----

# whd data from Aaron Rusch (WDNR); also available online
df_whd <- read_csv(here("data", "whd", "whdplus2.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(across(c(reachid), as.character)) 



### Check missing data -----

# # load cpe to get unique reach ids
# df_cpes_va <- read_rds(here("output","data","df_cpe_va.rds"))
# 
# # return all rows from CPE without a match in WHD.
# df_cpes_va |> anti_join(df_whd, by = c("reach_id" = "reachid"))
# # 24 records
# 
# # how many stream reaches?
# df_cpes_va |> 
#   anti_join(df_whd, by = c("reach_id" = "reachid")) |> 
#   distinct(reach_id)
# # 3 reaches missing from WHD data


## Clean ----

# combine LU categories
df_whd_cln <- df_whd |> 
  mutate(trw_lu11_forests = trw_lu11_41+trw_lu11_42+trw_lu11_43) |> 
  mutate(trw_lu11_wetland = trw_lu11_90+trw_lu11_95) |> 
  mutate(trw_lu11_ag = trw_lu11_81+trw_lu11_82) |> 
  select(-trw_lu11_41, -trw_lu11_42, -trw_lu11_43, -trw_lu11_90, -trw_lu11_95, 
         -trw_lu11_81, -trw_lu11_82)

# check variable names
names(df_whd_cln)

## select variables ----

df_whd_cln_slct <- df_whd_cln %>% 
  select(
    reach_id = reachid, 
    # c_lat,
    stream_order, 
    gradient
    # trw_reachlen, 
    # trw_lu11_forests, 
    # trw_lu11_wetland,
    # trw_lu11_ag
    )

### Correlations ----

names(df_whd_cln_slct)
tmp <- df_whd_cln_slct %>% select(2:ncol(df_whd_cln_slct)) 
tmp <- cor(tmp, use='pairwise.complete.obs')
corrplot::corrplot.mixed(tmp, upper='ellipse', tl.pos='lt')

### Histograms ----
df_whd_cln_slct %>% 
  pivot_longer(-reach_id, names_to="var", values_to="value") %>% 
  ggplot(aes(value)) +
  geom_histogram() + 
  facet_wrap(vars(var), scales = 'free')


## Standardize ----

# Mean-variance (z) standardize the covariates
df_whd_cln_slct_std <- df_whd_cln_slct %>% 
  mutate(across(2:ncol(df_whd_cln_slct), ~(scale(.) %>% as.vector)))


## Export ----
write_rds(df_whd_cln_slct, here("output","data","whd_cln_slct.rds"))
write_rds(df_whd_cln_slct_std, here("output","data","whd_cln_slct_std.rds"))




