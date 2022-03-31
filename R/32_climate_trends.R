

# Plot data trends =============================================================

df_daymet_covars <- readRDS(here("output","data", "df_covar_dmt.rds"))

# Prep data for plot
pdata <- df_daymet_covars %>% 
  filter(reach_id %in% new.dat$reach_id) %>% 
  pivot_longer(
    -c(year, reach_id, season), 
    names_to = "covar", 
    values_to="value"
  ) %>% 
  mutate(covar = case_when(
    covar == "mean.tmax" ~ "Max. Temperature (\u00B0C)", 
    covar == "total.prcp" ~ "Precipitation (mm)"
  )) %>% 
  mutate(season = case_when(
    season == "summer" ~ "Summer", 
    season == "autumn" ~ "Autumn", 
    season == "winter" ~ "Winter", 
    season == "spring" ~ "Spring"
  )) %>% 
  mutate(season = factor(season, levels = c("Summer","Autumn","Winter","Spring"))) %>% 
  mutate(covar = factor(covar, levels = c("Max. Temperature (\u00B0C)", "Precipitation (mm)")))

p.daymet <- pdata  %>% 
  ggplot(aes(x=year, group = reach_id)) + 
  facet_grid(covar~season, scales = "free") + 
  geom_line(aes(y=value, color=season), size=.05, alpha=0.5) +
  scale_color_manual(values = c("#a6611a","#dfc27d","#80cdc1","#018571")) +
  scale_x_continuous(breaks = seq(1995,2015,10)) +
  labs(x = "", y = "") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.background = element_blank(), 
    strip.text = element_text(hjust=0.5), 
    panel.border = element_rect()
  )
p.daymet

ggsave(here("output","figs","supmat_climate_trends.png"), p.daymet,
       device=agg_png, res=600, height = 4, width = 8)
