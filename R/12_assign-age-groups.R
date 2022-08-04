# Add age-class assignment and lengths to fish data
# Bryan Maitland
# 2020-09-10
# 2021-08-05

library(tidyverse)
library(here)
library(patchwork)

# output
# - 1 supmat figure: length frequency


# make a theme for supplementary figures
theme_clean <- function() {
  theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.title = element_text(size = rel(1), margin = margin(0,0,0,0,"cm")),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = "black",
                                      size = .5),
      panel.border = element_rect(fill = NA, color = "black", size = .5),
      panel.grid = element_blank(),
      panel.spacing = unit(.5, "lines"),
      axis.ticks = element_line(size = 0.5, color = "black"),
      axis.ticks.length = unit(.25, 'cm'),
      strip.text = element_text(hjust = 0),
      strip.background = element_rect(color = NA, fill = "white"),
      legend.margin = margin(0,0,0,0,'cm'), 
      legend.position = "none"
    )
}


# Load data  ===================================================================

# state-wide trout data
df_trout <- read_rds(here("output", "data", "df_trout.rds"))

# la data from FDMB
df_la <- read_rds(here("data","raw_fmdb_la_20210824.rds"))

# la data from Matt Mitro (MUST RECOMPILE)
# df_mitro <-
#   readxl::read_xlsx(here("data","mitro_la_bnt.xlsx")) %>%
#   select(waterbody.name = Stream,
#          species = Species,
#          survey.date = Date,
#          length_mm = Length,
#          age = 'Known Age',
#          sex = Sex) %>%
#   mutate(survey.date = lubridate::date(survey.date),
#          survey.year = lubridate::year(survey.date),
#          species = if_else(
#            species == "Brook Trout", "brook_trout", "brown_trout"),
#          waterbody.name = tolower(str_replace(waterbody.name, " ", "_")),
#          county = "vernon",
#          length = length_mm / 25.4)

# filter la data for summer samples and remove NAs
df_la_cln <- df_la %>%
  as_tibble() %>% 
  mutate(julian = lubridate::yday(sample.date)) %>%
  filter(!is.na(length)) %>% 
  filter(!is.na(age)) %>%
  mutate(length_mm = length * 25.4) %>% 
  select(waterbody.name, county, species, length, length_mm, age)
  # bind_rows(df_mitro %>%
  #             mutate(county = "vernon") %>%
  #             select(county, species, length_mm, age)
  # ) 

# length-freq. and vbgf ========================================================

names <- c("brook_trout" = 'c) Brook trout, all fish', 
           "brown_trout" = 'd) Brown trout, all fish')

# all fish from trout data set
p.lf.all <- df_trout %>% 
  ggplot(aes(length_mm, fill = species)) + 
  geom_histogram(binwidth = 5, color = NA, na.rm = TRUE) + 
  geom_vline(data=filter(df_trout, species=="brook_trout"), 
             aes(xintercept=100), linetype = "dashed") + 
  geom_vline(data=filter(df_trout, species=="brown_trout"),
             aes(xintercept=125), linetype = "dashed") + 
  facet_wrap(vars(species), nrow = 1, scales = 'free', 
             labeller = as_labeller(names)) + 
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(limits = c(0,750), breaks = seq(0,750, 100)) +
  scale_y_continuous(limits = c(0,35000), breaks = seq(0,30000,10000),
                     label = scales::comma) +
  labs(x = "Total length (mm)", y = "Count") + 
  coord_cartesian(expand = FALSE, clip = 'off') + 
  theme_clean()+
  theme(legend.position = "none")
p.lf.all

names <- c("brook_trout" = 'a) Brook trout, known age fish', 
           "brown_trout" = 'b) Brown trout, known age fish')

# all fish from la data set
p.lf.aged <-  df_la_cln %>% 
  ggplot(aes(length_mm, fill = species)) + 
  geom_histogram(binwidth = 5, color = NA, na.rm = TRUE, size = 1) + 
  geom_vline(data=filter(df_la_cln, species=="brook_trout"), 
             aes(xintercept=100), linetype = "dashed") + 
  geom_vline(data=filter(df_la_cln, species=="brown_trout"),
             aes(xintercept=125), linetype = "dashed") + 
  facet_wrap(vars(species), nrow = 1, scales = 'free',
             labeller = as_labeller(names)) + 
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,100)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) +
  labs(x = "", y = "Count") +
  coord_cartesian(expand = FALSE, clip = 'off') + 
  theme_clean()+ 
  theme(legend.position = "none")
p.lf.aged


p.la.panel <- p.lf.aged / p.lf.all 

p.la.panel

ggsave(
  here("output","figs","supmat_length_freq.png"), 
  p.la.panel,
  device=ragg::agg_png, res=600, height = 6, width = 8
  )


# Assign YOY status ============================================================

# how many record do not have a YOY flag?
df_trout %>%
  group_by(species, yoy) %>%
  summarise(Count = n()) %>%
  mutate(
    Percent = round(((Count / sum(Count))*100), digits = 1), 
    Count = Count)
# ~80%


# based on statewide data, flags based on species and length cutoffs
df_trout_yoy <- df_trout %>% 
  # no YOYs over 150 mm (6 in), should be adults )
  mutate(yoy = case_when(
    yoy == "Y" & length_mm >= 150 ~ "N", 
    TRUE ~ yoy
    )) %>%
  mutate(yoy = case_when(
    species == "brook_trout" & yoy == "N" & length_mm <= 100 ~ "Y", 
    species == "brown_trout" & yoy == "N" & length_mm <= 120 ~ "Y", 
    TRUE ~ yoy
    )) %>% 
  mutate(yoy = case_when(
    species == "brook_trout" & is.na(yoy) & length_mm <= 100 ~ "Y", 
    species == "brook_trout" & is.na(yoy) & length_mm > 100 ~ "N", 
    species == "brown_trout" & is.na(yoy) & length_mm <= 120 ~ "Y", 
    species == "brown_trout" & is.na(yoy) & length_mm > 120 ~ "N", 
    TRUE ~ yoy
    )
    ) 

# now how many record do not have a YOY flag?
df_trout_yoy %>%
  group_by(species, yoy) %>%
  summarise(Count = n()) %>%
  mutate(
    Percent = round(((Count / sum(Count))*100), digits = 1), 
    Count = Count)
# 1.7% and 1.3%

# Estimate lengths for remaining 1.5% of fish  =================================

# this estimates lengths using man-ass and adds a yoy flag
df_trout_yoy <- df_trout_yoy %>%
  wdnr.fmdb::add_fmdb_class("fmdb_fishraw") %>% 
  wdnr.fmdb::estimate_lengths(lengthed_data = df_trout_yoy %>%
                                wdnr.fmdb::add_fmdb_class("fmdb_fishraw"), 
                              add_yoy_grouping = TRUE) %>%
  mutate(length_mm = length * 25.4)

# now how many record do not have a YOY flag?
df_trout_yoy %>%
  group_by(species, yoy) %>%
  summarise(Count = n()) %>%
  mutate(
    Percent = round(((Count / sum(Count))*100), digits = 1), 
    Count = Count)
# NONE!  


# Visualize and summarize ======================================================

# ggplot(data = filter(df_trout_yoy_lengthed, length.est == "measured"),
#        aes(x = length_mm, fill = yoy)) +
#   geom_histogram(aes(y = ..density..),
#                  na.rm = TRUE, binwidth = 5, position = "identity", alpha = 0.5) +
#   geom_density(data = filter(df_trout_yoy_lengthed, length.est != "measured"),
#                na.rm = TRUE, 
#                aes(x = length_mm, color = yoy), fill = NA) +
#   facet_wrap(vars(species), nrow = 2,  scales = 'free', 
#              labeller = as_labeller(labels)) +
#   labs(title = "", fill = "YOY",
#        x = "Total length (mm)", y = "Desntiy") + 
#   scale_x_continuous(limits = c(0,750), breaks = seq(0,750, 100)) +
#   scale_y_continuous(limits = c(0,.03), breaks = seq(0,.03,.01)) +
#   scale_color_viridis_d(option = "D", end = 0.8, guide = "none") + 
#   scale_fill_viridis_d(option = "D", end = 0.8) + 
#   theme(legend.position = "right", 
#         strip.text = element_text(hjust = 0)
#         ) + 
#   guides(fill = guide_legend(reverse=TRUE))


# Save age-assigned trout data =================================================

# save
saveRDS(df_trout_yoy, here("output", "data", "df_trout_yoy.rds"))


# clean up
rm(p.la.panel); rm(p.lf.aged); rm(p.lf.all)
rm(df_la); rm(df_la_cln)


# ===================== END ==========================

