

# Statewide trend plots

library(tidyverse)
library(lubridate)
library(here)

# theme for trend plotting
theme_set(theme_bw(base_size = 16, base_family = "sans"))
theme_update(
  legend.position = "right", 
  legend.key=element_blank(), 
  axis.text.x = element_text(angle = 0, vjust = 0.5), 
  strip.background = element_rect(fill = "black"), 
  strip.text = element_text(color = "white"), 
  panel.border = element_rect(color = "black", fill = NA), 
  panel.grid = element_blank(), 
  panel.spacing = unit(.1, "lines")
)

pt_sizes <- 0.7
ln_size <- 0.2


# Load data ================================================================

df_cpes <- readRDS(here("output","data","df_cpe.rds"))


# Average for yoy and adults across state

df_statewide <- 
  df_cpes %>% 
  filter(species=="brook_trout", yoy=="Y") %>% 
  filter(huc_names %in% c("Kickapoo","Sugar","Wolf")) %>% droplevels() %>% 
  group_by(year, species, yoy) %>% 
  summarise(
    mean_cpe = mean(cpe, na.rm = TRUE),
    se = sd(cpe)/sqrt(length(cpe))
  ) %>% 
  ungroup() %>% 
  complete(year, species, yoy) 


p.cpe.statewide <- df_statewide %>% 
  ggplot(aes(x = year, y = mean_cpe, color = yoy)) +
  geom_errorbar(aes(ymin=mean_cpe-se, ymax = mean_cpe+se), width = 0, size = 1)+
  geom_point(size = 2) + 
  geom_line(size = 1) +
  lemon::facet_rep_wrap(vars(species), labeller = labeller(species = labels)) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1993, 2022)) +
  scale_y_continuous(label=scales::comma, limits = c(0,1050)) +
  scale_color_manual(values = pal.gearclass[c(1,3)], labels = c("Age 1+", "Age 0+"), guide = guide_legend(reverse = TRUE)) + 
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Year", y = "Mean CPUE (fish / mile)", 
       color = "Life stage") 
p.cpe.statewide
save_plot(path = here("output","figs","cpe_state_by_species"), height = 5, width = 12)


# Average for yoy and adults across state by reagions

df_statewide <- 
  df_cpes %>% 
  group_by(year, WMU_NAME, species, yoy) %>% 
  summarise(
    mean_cpe = mean(cpe, na.rm = TRUE),
    se = sd(cpe)/sqrt(length(cpe))
  ) %>% 
  ungroup() %>% 
  complete(year, WMU_NAME, species, yoy) 


p.cpe.statewide_bktbyreg <- df_statewide %>% filter(species=="brook_trout") %>% 
  ggplot(aes(x = year, y = mean_cpe, color = yoy)) +
  geom_point(size = 1) +
  geom_line(size = 1) +
  lemon::facet_rep_wrap(vars(WMU_NAME), labeller = labeller(species = labels)) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1993, 2022)) +
  scale_y_continuous(label=scales::comma, limits = c(0,1050)) +
  scale_color_manual(values = pal.gearclass[c(1,3)], labels = c("Age 1+", "Age 0+"), guide = guide_legend(reverse = TRUE)) + 
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Year", y = "Mean CPUE (fish / mile)", 
       color = "Life stage") + 
  theme(axis.text.x = element_text(angle=90))
p.cpe.statewide_bktbyreg
save_plot(path = here("output","figs","cpe_state_bkt_regions"), height = 12, width = 14)

p.cpe.statewide_bntbyreg <- df_statewide %>% filter(species=="brown_trout") %>% 
  ggplot(aes(x = year, y = mean_cpe, color = yoy)) +
  geom_point(size = 1) +
  geom_line(size = 1) +
  lemon::facet_rep_wrap(vars(WMU_NAME), labeller = labeller(species = labels)) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1993, 2022)) +
  scale_y_continuous(label=scales::comma, limits = c(0,1050)) +
  scale_color_manual(values = pal.gearclass[c(1,3)], labels = c("Age 1+", "Age 0+"), guide = guide_legend(reverse = TRUE)) + 
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Year", y = "Mean CPUE (fish / mile)", 
       color = "Life stage") + 
  theme(axis.text.x = element_text(angle=90))
p.cpe.statewide_bntbyreg
save_plot(path = here("output","figs","cpe_state_bnt_regions"), height = 12, width = 14)



# prep data for plots
df_statewide <- 
  df_cpes %>% 
  group_by(year, gear, species, yoy) %>% 
  summarise(
    mean_cpe = mean(cpe, na.rm = TRUE),
    se = sd(cpe)/sqrt(length(cpe))
  ) %>% 
  ungroup() %>% 
  complete(year, gear, species, yoy) %>% 
  mutate(gear_class = case_when(
    gear == "backpack_shocker" & yoy == "N" ~ "Backpack - Adult",
    gear == "backpack_shocker" & yoy == "Y" ~ "Backpack - YOY",
    gear == "stream_shocker" & yoy == "N" ~ "Stream - Adult",
    gear == "stream_shocker" & yoy == "Y" ~ "Stream - YOY")
  ) 

# gear class
df_statewide %>% 
  ggplot(aes(x = year, y = mean_cpe, group = gear_class)) +
  geom_point(aes(color = gear_class), size = 2) + 
  geom_line(aes(color = gear_class), size = 1) +
  # geom_smooth(aes(color = gear_class), method = "gam", se = FALSE, size = .75) +
  lemon::facet_rep_wrap(vars(species), nrow = 2, labeller = labeller(species = labels)) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1993, 2022)) +
  # scale_y_continuous(label=scales::comma, limits = c(0,1050)) +
  # coord_cartesian(expand = FALSE) +
  scale_color_manual("Gear - Life stage", values = pal.gearclass[c(1,2,4,3)]) +
  labs(x = "Year", y = "log mean CPUE (fish / mile)") 

save_plot(path = here("output","figs","cpe_state_by_gearstage"), height = 10, width = 8)

# yoy
df_statewide %>% 
  filter(yoy=="Y") %>% 
  ggplot(aes(x = year, y = mean_cpe, group = gear_class)) +
  geom_point(aes(color = gear_class), size = 2) + 
  geom_line(aes(color = gear_class), size = 1) +
  # geom_smooth(aes(color = gear_class), method = "gam", se = FALSE, size = .75) +
  lemon::facet_rep_wrap(vars(species), labeller = labeller(species = labels)) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1993, 2022)) +
  scale_y_continuous(label=scales::comma, limits = c(0,1050)) +
  coord_cartesian(expand = FALSE) +
  scale_color_manual("Gear - Life stage", values = pal.gearclass[c(2,3)]) +
  labs(x = "Year", y = "Mean CPUE (fish / mile)") 

save_plot(path = here("output","figs","cpe_state_by_gearstage_yoy"), height = 4.5, width = 12)

df_statewide %>% 
  filter(yoy=="N") %>% 
  ggplot(aes(x = year, y = mean_cpe, group = gear_class)) +
  geom_point(aes(color = gear_class), size = 2) + 
  geom_line(aes(color = gear_class), size = 1) +
  # geom_smooth(aes(color = gear_class), method = "gam", se = FALSE, size = .75) +
  lemon::facet_rep_wrap(vars(species), labeller = labeller(species = labels)) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1993, 2022)) +
  scale_y_continuous(label=scales::comma, limits = c(0,1050)) +
  coord_cartesian(expand = FALSE) +
  scale_color_manual("Gear - Life stage", values = pal.gearclass[c(1,4)]) +
  labs(x = "Year", y = "Mean CPUE (fish / mile)") 

save_plot(path = here("output","figs","cpe_state_by_gearstage_adult"), height = 4.5, width = 12)




# prep data for plots
df_statewide <- 
  df_cpes %>% 
  group_by(year, WMU_NAME, gear, species, yoy) %>% 
  summarise(
    mean_cpe = mean(cpe, na.rm = TRUE),
    se = sd(cpe)/sqrt(length(cpe))
  ) %>% 
  ungroup() %>% 
  complete(year,WMU_NAME, gear, species, yoy) %>% 
  mutate(gear_class = case_when(
    gear == "backpack_shocker" & yoy == "N" ~ "Backpack - Adult",
    gear == "backpack_shocker" & yoy == "Y" ~ "Backpack - YOY",
    gear == "stream_shocker" & yoy == "N" ~ "Stream - Adult",
    gear == "stream_shocker" & yoy == "Y" ~ "Stream - YOY")
  ) 


df_statewide %>% 
  filter(species == "brook_trout") %>% 
  ggplot(aes(x = year, y = mean_cpe, group = gear_class)) +
  geom_point(aes(color = gear_class), size = 1.5) + 
  # geom_line(aes(color = gear_class), size = 0.75) +
  geom_smooth(aes(color = gear_class), method = "gam", se = FALSE) +
  facet_rep_wrap(vars(WMU_NAME), ncol = 8, labeller = labeller(species = labels)) +
  scale_y_log10()  +
  scale_x_continuous(breaks = seq(2000,2020,10), limits = c(1993, 2022)) +
  # scale_y_continuous(label=scales::comma, limits = c(0,1050)) +
  scale_color_manual("Gear - Life stage", values = pal.gearclass[c(1,2,4,3)]) +
  coord_cartesian(expand = FALSE)+
  labs(x = "Year", y = "log mean CPUE (fish / mile)")


save_plot(path = here("plots","explore","cpe_state_panel_gearstage_wmu_bkt"), width = 28, height = 10)


df_statewide %>% 
  filter(species == "brown_trout") %>% 
  ggplot(aes(x = year, y = mean_cpe, group = gear_class)) +
  geom_point(aes(color = gear_class), size = 1.5) + 
  # geom_line(aes(color = gear_class), size = ln_size) +
  geom_smooth(aes(color = gear_class), method = "gam", se = FALSE) +
  facet_rep_wrap(vars(WMU_NAME), ncol = 8, labeller = labeller(species = labels)) +
  scale_x_continuous(breaks = seq(2000,2020,10), limits = c(1993, 2022)) +
  # scale_y_continuous(label=scales::comma, limits = c(0,1050)) +
  scale_y_log10()  +
  coord_cartesian(expand = FALSE) +
  scale_color_manual("Gear - Life stage", values = pal.gearclass[c(1,2,4,3)]) +
  labs(x = "Year", y = "log ean CPUE (fish / mile)")


save_plot(path = here("plots","explore","cpe_state_panel_gearstage_wmu_bnt"), width = 28, height = 10)
