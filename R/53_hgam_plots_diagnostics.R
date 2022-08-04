

# Diagnostics  ====================================================

appraise(bkt0.m3) & 
  theme_minimal() & 
  plot_annotation(
    title = "YOY Brook Trout", 
    theme = theme(plot.title = element_text(size=16, hjust=.5)))
ggsave(here("output","figs","supmat_gam_app_bkt0.png"), plot = last_plot(), 
       device=ragg::agg_png, res=300, height = 6, width = 8)

appraise(bkt1.m3) & 
  theme_minimal() & 
  plot_annotation(
    title = "Age-1+ Brook Trout", 
    theme = theme(plot.title = element_text(size=16, hjust=.5)))
ggsave(here("output","figs","supmat_gam_app_bkt1.png"), plot = last_plot(), 
       device=ragg::agg_png, res=300, height = 6, width = 8)

appraise(bnt0.m3) & 
  theme_minimal() & 
  plot_annotation(
    title = "YOY Brown Trout", 
    theme = theme(plot.title = element_text(size=16, hjust=.5)))
ggsave(here("output","figs","supmat_gam_app_bnt0.png"), plot = last_plot(), 
       device=ragg::agg_png, res=300, height = 6, width = 8)

appraise(bnt1.m3) & 
  theme_minimal() & 
  plot_annotation(
    title = "Age-1+ Brown Trout", 
    theme = theme(plot.title = element_text(size=16, hjust=.5)))
ggsave(here("output","figs","supmat_gam_app_bnt1.png"), plot = last_plot(), 
       device=ragg::agg_png, res=300, height = 6, width = 8)


# Effect size plots =======================================================

p <- draw(bkt0.m3.y2y, select = c("s(year)","s(year,reach_id)"))  & 
  theme_minimal() & 
  plot_layout(guides='collect') &
  theme(legend.position='none') &
  plot_annotation(
    title = "YOY Brook Trout", 
    theme = theme(plot.title = element_text(size=16, hjust=.5)))

ggsave(here("output","figs","supmat_gam_dra_bkt0.png"), plot = last_plot(), 
       device=ragg::agg_png, res=300, height = 4, width = 8)

p <- draw(bkt1.m3.y2y, select = c("s(year)","s(year,reach_id)"))  & 
  theme_minimal() & 
  plot_layout(guides='collect') &
  theme(legend.position='none') &
  plot_annotation(
    title = "Age-1+ Brook Trout", 
    theme = theme(plot.title = element_text(size=16, hjust=.5)))

ggsave(here("output","figs","supmat_gam_dra_bkt1.png"), plot = last_plot(), 
       device=ragg::agg_png, res=300, height = 4, width = 8)

p <-  draw(bnt0.m3.y2y, select = c("s(year)","s(year,reach_id)")) & 
  theme_minimal() & 
  plot_layout(guides='collect') &
  theme(legend.position='none') &
  plot_annotation(
    title = "YOY Brown Trout", 
    theme = theme(plot.title = element_text(size=16, hjust=.5)))

ggsave(here("output","figs","supmat_gam_dra_bnt0.png"), plot = last_plot(), 
       device=ragg::agg_png, res=300, height = 4, width = 8)

p <- draw(bnt1.m3.y2y, select = c("s(year)","s(year,reach_id)"))  & 
  theme_minimal() & 
  plot_layout(guides='collect') &
  theme(legend.position='none') &
  plot_annotation(
    title = "Age-1+ Brown Trout", 
    theme = theme(plot.title = element_text(size=16, hjust=.5)))

ggsave(here("output","figs","supmat_gam_dra_bnt1.png"), plot = last_plot(), 
       device=ragg::agg_png, res=300, height = 4, width = 8)


