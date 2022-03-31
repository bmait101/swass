


theme_clean <- function() {
  theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = "black",
                                      size = .5),
      panel.border = element_rect(fill = NA, color = "black", size = .5),
      panel.grid = element_blank(),
      panel.spacing = unit(.5, "lines"),
      axis.ticks = element_line(size = 0.5, color = "black"),
      axis.ticks.length = unit(.25, 'cm'),
      strip.text = element_blank(),
      strip.background = element_blank(),
      legend.margin = margin(0,0,0,0,'cm'), 
      legend.position = "none"
    )
}

theme_set(theme_clean())


p1 <- plot(conditional_smooths(bkt.mod))[[1]] +
  labs(title = "(A) Brook trout", x = "Standardized year", y = "Estimate")
p2 <- plot(conditional_smooths(bnt.mod))[[1]] +
  labs(title = "(B) Brown trout", x = "Standardized year", y = "Estimate")


p.panel <- p1 | p2
p.panel


ggsave(here("output","figs","supmat_residual_trend.png"), p.panel,
       device=ragg::agg_png, res=600, height = 4, width = 8)
