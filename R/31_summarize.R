
# Libraries
library(tidyverse)
library(lubridate)
library(here)
library(cowplot)
library(ragg)
library(sf)


# load prepped data
source(here::here("R", "30_prep_final.R"))


# Data summary =================================================================

df_analysis %>%  
  mutate(across(c(survey.seq.no, visit.fish.seq.no, wbic), as_factor)) %>% 
  skimr::skim()

# Plot the count of observation by survey purpose
df_analysis %>%
  distinct(survey.seq.no, .keep_all=TRUE) %>%
  group_by(primary.survey.purpose) %>% tally() %>%
  ggplot(aes(x = reorder(primary.survey.purpose, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Breakdown of surveys purposes in dataset (n=5,383 surveys)")


# Counts =======================================================================

# Histogram of year-observations for stream reaches
hist(df_analysis$yr_obs)

# Count of observations over time by species and sizeclass
df_analysis %>% 
  ggplot(aes(x = year, fill = gear)) + 
  facet_grid(size_class~species, scales = "free") + 
  geom_bar(width = 0.75, color = "black") + 
  labs(x = "Calendar year", y = "Count of observations") +
  coord_cartesian(expand = FALSE, clip = "off")


# summary of number of surveys per year
(
  survs_per_year <- df_analysis %>%
    distinct(year, survey.seq.no) %>% 
    group_by(year) %>%
    summarise(n = n()) %>%
    ungroup() %>% 
    summarise(
      mean = mean(n),
      min = min(n),
      max = max(n)
    )
)


# summary of number of efforts per year
(
  effs_per_year <- df_analysis %>%
    distinct(year, visit.fish.seq.no) %>% 
    group_by(year) %>%
    summarise(n = n()) %>%
    ungroup() %>% 
    summarise(
      mean = mean(n),
      min = min(n),
      max = max(n)
    )
)

# plot number of surveys per year
p.survs.time <- df_analysis %>%
  distinct(survey.seq.no, .keep_all = TRUE) %>% 
  mutate(gear = fct_recode(gear, 
                           "Backpack Shocker" = "backpack_shocker", 
                           "Stream Shocker" = "stream_shocker")) %>% 
  
  ggplot(aes(x = year, fill = gear)) + 
  geom_bar(width = 0.75, color = "black") + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(4, "Paired")[3:4]) + 
  scale_x_continuous(breaks = seq(1994, 2019, 1)) +
  scale_y_continuous(label = scales::comma, 
                     breaks = seq(0,400, 100), limits = c(0,400)) + 
  labs(x = "Calendar year", y = "Count of Surveys", fill = "Gear type", 
       title = "Count of surveys per year (mean = 207, range 30-375)") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_classic() +
  theme(
    legend.position = c(0.15, 0.8), 
    axis.text.x = element_text(angle = 90), 
    panel.background = element_rect(fill = NA, color = "black", size = .5)
  )
p.survs.time
ggsave(here("output","figs","supmat_survs_over_time.png"), p.survs.time,
       device=agg_png, res=600, width = 8, height = 4)



# summary of number of streams per year
(
  reach_per_year <- new.dat %>%
    distinct(year, reach_id) %>% 
    group_by(year) %>%
    summarise(n = n()) %>%
    ungroup() %>% 
    summarise(
      mean = mean(n),
      min = min(n),
      max = max(n))
)


# summary of number of sites per year
(
  site_per_year <- new.dat %>%
    filter(year >= 2004) %>% 
    distinct(year, site.seq.no) %>% 
    group_by(year) %>%
    summarise(n = n()) %>%
    ungroup() %>% 
    summarise(
      mean = mean(n),
      min = min(n),
      max = max(n))
)


# summary of number of sites per year on unfiltered data
(
  site_per_year_raw <- df_surveys %>%
    filter(survey.year>=2008) %>% 
    distinct(survey.year, site.seq.no) %>% 
    group_by(survey.year) %>%
    summarise(n = n()) %>%
    ungroup() %>% 
    summarise(
      mean = mean(n),
      min = min(n),
      max = max(n))
)


# Summary stats table ==========================================================

summary_functions <- list(
  median = ~median(.x, na.rm = TRUE),
  mean = ~mean(.x, na.rm = TRUE),
  sd = ~sd(.x, na.rm = TRUE),
  min = ~min(.x, na.rm = TRUE),
  max = ~max(.x, na.rm = TRUE)
)

# Fish cpes
desc_stats_fish <- df_analysis %>% 
  select(species, size_class, cpe) %>% 
  mutate(cpe_km = cpe / 1.609) |> 
  group_by(species, size_class) %>% 
  summarise(across(c(cpe_km), summary_functions)) %>%
  mutate(across(where(is.numeric), round, 1))
desc_stats_fish

write.csv(desc_stats_fish,
          here("output","tables","summary_stats_trout_counts_km.csv"),
          row.names = FALSE)

# Covariates (requires non-standardized data)
desc_stats_covar <- new.dat %>% 
  select(mean.tmax_summer, total.prcp_summer, 
         mean.tmax_autumn, total.prcp_autumn, 
         mean.tmax_winter, total.prcp_winter, 
         mean.tmax_spring, total.prcp_spring, 
         latitude, w_area, stream_order, gradient, trw_reachlen) %>% 
  pivot_longer(everything(), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  summarise(across(c(value), summary_functions)) %>% 
  arrange(factor(param, levels = c(
    "mean.tmax_summer", 
    "mean.tmax_autumn", 
    "mean.tmax_winter", 
    "mean.tmax_spring", 
    "total.prcp_summer", 
    "total.prcp_autumn", 
    "total.prcp_winter", 
    "total.prcp_spring", 
    "c_lat", 
    "gradient", 
    "stream_order", 
    "w_area", 
    "trw_reachlen"
  )))  %>%
  mutate(across(where(is.numeric), round, 1))
desc_stats_covar

# write.csv(desc_stats_covar,
#           here("output","tables","summary_stats_covars.csv"),
#           row.names = FALSE)





