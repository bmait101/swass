

source(here::here("R", "30_prep_final.R"))

# prep data for plots
pdata <- df_analysis %>%
  # filter(! (species=="brook_trout" & year==1994)) %>% 
  group_by(year, species, size_class) %>%
  summarise(
    mean_cpe = mean(cpe, na.rm = TRUE),
    # sd = sd(cpe, na.rm = TRUE),
    se = sd(cpe)/sqrt(length(cpe)),
    .groups = "drop"
  ) %>%
  complete(year, species, size_class) |> 
  mutate(mean_cpe_km = mean_cpe / 0.621371, 
         se_km = se / 0.621371)

pdata |> 
  ggplot(aes(x = year, y = mean_cpe, color = size_class)) +
  geom_point() +
  geom_line()  +
  geom_errorbar(aes(ymin=mean_cpe-se, ymax=mean_cpe+se), width=.1) +
  facet_wrap(vars(species), nrow = 2, scales="free")

pdata |> 
  ggplot(aes(x = year, y = mean_cpe_km, color = size_class)) +
  geom_point() +
  geom_line()  +
  geom_errorbar(aes(ymin=mean_cpe_km-se_km, ymax=mean_cpe_km+se_km), width=.1) +
  facet_wrap(vars(species), nrow = 2, scales="free")


spp_names <- c('brook_trout'="Brook Trout", 
               'brown_trout'="Brown Trout")
# plot
p.raw.state <- pdata %>%
  mutate(size_class = if_else(
    size_class=="adult","Age-1+",as.character(size_class))) %>% 
  mutate(size_class = factor(size_class, levels = c("yoy", "Age-1+"))) %>%
  ggplot(aes(x = year, y = mean_cpe_km, color = size_class)) +
  lemon::facet_rep_wrap(
    vars(species), nrow = 2, scales="free", labeller=as_labeller(spp_names)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin=mean_cpe_km-se_km, ymax=mean_cpe_km+se_km), width=.1) +
  geom_line(aes(linetype=size_class), size = .75) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1993, 2020)) + 
  # scale_y_continuous(limits = c(0,1300), breaks = seq(0,1300,250)) + 
  scale_color_manual(values = c("forestgreen", "black")) +
  labs(x = "Year", 
       y = expression(Mean~relative~abundance~(fish~km^{-1})),
       color="Age group")  + 
  guides(linetype = 'none') + 
  theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = "black", size = 1),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 1),
    panel.spacing = unit(.5, "lines"),
    axis.ticks = element_line(size = 0.5, color = "black"),
    axis.ticks.length = unit(.1, 'cm'),
    strip.text = element_text(size = rel(1), hjust = 0, 
                              margin=margin(.2,0,.2,.3,'cm')),
    strip.background = element_rect(fill = "white", color = "black", size=1),
    legend.position = 'right' , 
    legend.text = element_text(margin = margin(0,0,0,-.3,'cm')),
    legend.title = element_text(margin = margin(0,0,-.3,0,'cm')),
    legend.margin = margin(0,0,0,0,'cm')
  )
p.raw.state

ggsave(here("output","figs","supmat_cpe_trends.png"), p.raw.state,
       device=ragg::agg_png, res=600, height = 5, width = 7)


# save plot
path <- here::here("output","figs1","supmat_cpe_trends")
ggsave(
  glue::glue("{path}.pdf"), 
  plot = p.raw.state, 
  width = 7, 
  height = 5, 
  device = cairo_pdf
  )
pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  format = "png", 
  dpi = 600
  )


