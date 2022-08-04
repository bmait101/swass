

# Histrogram of standardized climate variables

df_drivers |> 
  select(starts_with(c("total.prcp","mean"))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |> 
  separate(variable, into = c("var", "season"), sep = "_") |> 
  mutate(
    var = factor(var, levels = c("mean.tmax", "total.prcp")), 
    season = factor(
      season, levels = c("spring", "summer", "autumn", "winter"))
    ) |> 
  ggplot(aes(value)) + 
  lemon::facet_rep_grid(
    rows = vars(var), cols = vars(season), scales = "free_y") + 
  geom_histogram(aes(fill = var), binwidth = .25, color = "black", size = .2) + 
  scale_fill_manual(
    values = c("red", "blue"), 
    label = c("Mean daily maximum air temperature","Total precipitation")) + 
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5,5,1)) + 
  labs(
    x = "Z-score", y = "Count", 
    title = "Histrogram of standardized climate variables by season", 
    fill = "") + 
  theme_bw() + 
  theme(
    legend.position = "bottom",
    panel.spacing.x = unit(.01, "cm"), 
    panel.spacing.y = unit(.01, "cm")
    )


ggsave(here("output","figs","hist_-climate-vars-zscore.png"),
       device=ragg::agg_png, res=300, height = 4, width = 8)
