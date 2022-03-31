

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



vars_f <- tibble(
  params = factor(vars, levels = c(
    "b_mean.tmax_summer",
    "b_mean.tmax_autumn",
    "b_mean.tmax_winter",
    "b_mean.tmax_spring",
    "b_Imean.tmax_springE2",
    "b_total.prcp_summer",
    "b_total.prcp_autumn",
    "b_total.prcp_winter",
    "b_total.prcp_spring",
    "b_Itotal.prcp_springE2"
  )))


vars2_f <- tibble(
  params = factor(vars2, levels = c(
    "b_mean.tmax_summer:latitude_s",
    "b_mean.tmax_autumn:latitude_s",
    "b_mean.tmax_winter:latitude_s",
    "b_mean.tmax_spring:latitude_s",
    "b_Imean.tmax_springE2:latitude_s",
    "b_total.prcp_summer:latitude_s",
    "b_total.prcp_autumn:latitude_s",
    "b_total.prcp_winter:latitude_s",
    "b_total.prcp_spring:latitude_s",
    "b_Itotal.prcp_springE2:latitude_s"
  )))


# Extract Coefficients and posterior samples/draws -------------------
draws.b.bkt <- bkt.mod %>% gather_draws(`b_.*`, regex = TRUE)
draws.b.bnt <- bnt.mod %>% gather_draws(`b_.*`, regex = TRUE)
# draws.r.bkt <- bkt.mod %>% gather_draws(r_reach_id[reachid,covar])
# draws.r.bnt <- bnt.mod %>% gather_draws(r_reach_id[reachid,covar])
# draws.sd.bkt <- bkt.mod %>% gather_draws(`sd_.*`, regex = TRUE)
# draws.sd.bnt <- bnt.mod %>% gather_draws(`sd_.*`, regex = TRUE)
# draws.sds.bkt <- bkt.mod %>% gather_draws(`sds_.*`, regex = TRUE)
# draws.sds.bnt <- bnt.mod %>% gather_draws(`sds_.*`, regex = TRUE)
# draws.shape.bkt <- bkt.mod %>% gather_draws(`shape.*`, regex = TRUE)
# draws.shape.bnt <- bnt.mod %>% gather_draws(`shape.*`, regex = TRUE)



# Hypothesis tests
bkt.hyp.post <- c(
  hypothesis(bkt.mod, 'mean.tmax_summer<0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'mean.tmax_autumn>0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'mean.tmax_winter<0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'mean.tmax_spring<0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'Imean.tmax_springE2<0')$hypothesis[[7]],
  
  hypothesis(bkt.mod, 'total.prcp_summer>0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'total.prcp_autumn<0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'total.prcp_winter<0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'total.prcp_spring<0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'Itotal.prcp_springE2<0')$hypothesis[[7]]
)

bkt.hyp.post2 <- c(
  hypothesis(bkt.mod, 'mean.tmax_summer:latitude_s>0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'mean.tmax_autumn:latitude_s<0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'mean.tmax_winter:latitude_s>0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'mean.tmax_spring:latitude_s<0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'Imean.tmax_springE2:latitude_s<0')$hypothesis[[7]],
  
  hypothesis(bkt.mod, 'total.prcp_summer:latitude_s>0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'total.prcp_autumn:latitude_s<0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'total.prcp_winter:latitude_s>0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'total.prcp_spring:latitude_s>0')$hypothesis[[7]],
  hypothesis(bkt.mod, 'Itotal.prcp_springE2:latitude_s>0')$hypothesis[[7]]
)




# Hypothesis tests
bnt.hyp.post <- c(
  hypothesis(bnt.mod, 'mean.tmax_summer>0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'mean.tmax_autumn<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'mean.tmax_winter<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'mean.tmax_spring<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'Imean.tmax_springE2>0')$hypothesis[[7]],
  
  hypothesis(bnt.mod, 'total.prcp_summer>0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'total.prcp_autumn<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'total.prcp_winter<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'total.prcp_spring<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'Itotal.prcp_springE2<0')$hypothesis[[7]]
)

bnt.hyp.post2 <- c(
  hypothesis(bnt.mod, 'mean.tmax_summer:latitude_s<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'mean.tmax_autumn:latitude_s<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'mean.tmax_winter:latitude_s>0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'mean.tmax_spring:latitude_s<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'Imean.tmax_springE2:latitude_s<0')$hypothesis[[7]],
  
  hypothesis(bnt.mod, 'total.prcp_summer:latitude_s<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'total.prcp_autumn:latitude_s<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'total.prcp_winter:latitude_s<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'total.prcp_spring:latitude_s<0')$hypothesis[[7]],
  hypothesis(bnt.mod, 'Itotal.prcp_springE2:latitude_s>0')$hypothesis[[7]]
)


hyp.post <- tibble(
  params = rep(vars_f$params, 2),
  species = c(rep("brook_trout",10),rep("brown_trout",10)),
  hyp = c(bkt.hyp.post, bnt.hyp.post)
) 


hyp.post2 <- tibble(
  params = rep(vars2_f$params, 2),
  species = c(rep("brook_trout",10),rep("brown_trout",10)),
  hyp = c(bkt.hyp.post2, bnt.hyp.post2)
) 




p.coefs <- draws.b.bkt %>% 
  mutate(species = "brook_trout") %>%
  bind_rows(draws.b.bnt %>% mutate(species = "brown_trout")) %>%
  filter(.variable %in% vars) %>% 
  group_by(species, .variable) %>% 
  mean_qi(mean = .value, .width = c(.8)) %>%
  left_join(hyp.post, by = c(".variable"="params","species")) %>% 
  mutate(.variable = factor(
    .variable, 
    levels = levels(vars_f$params),
    labels = c(
      'Summer max temperature', 
      'Autumn max temperature', 
      'Winter max temperature',
      'Spring max temperature',
      'Spring max temperature^2', 
      'Summer precipitation',
      'Autumn precipitation',
      'Winter precipitation',
      'Spring precipitation',
      'Spring precipitation^2'
    ))) %>% 
  ggplot(aes(y = .variable, x = mean, xmin = .lower, xmax = .upper)) +
  facet_wrap(vars(species)) +
  geom_pointinterval(aes(fill = hyp), shape = 21, size = 5) + 
  geom_vline(xintercept = 0, color = "black", size = .5) + 
  scale_fill_gradient(low = "white", high = "red") + 
  scale_y_discrete(limits=rev) + 
  labs(y = "", x = "Effect size")
p.coefs

p.coefs.int <- draws.b.bkt %>% 
  mutate(species = "brook_trout") %>%
  bind_rows(draws.b.bnt %>% mutate(species = "brown_trout")) %>%
  filter(.variable %in% vars2) %>% 
  group_by(species, .variable) %>% 
  mean_qi(mean = .value, .width = c(.8)) %>%
  mutate(.variable = factor(.variable, levels = levels(vars2_f$params))) %>% 
  left_join(hyp.post2, by = c(".variable"="params","species")) %>% 
  mutate(.variable = factor(
    .variable, 
    levels = levels(vars2_f$params),
    labels = c(
      'Summer temperature:latitude_s', 
      'Autumn temperature:latitude_s', 
      'Winter temperature:latitude_s',
      'Spring temperature:latitude_s',
      'Spring temperature^2:latitude_s', 
      'Summer precipitation:latitude_s',
      'Autumn precipitation:latitude_s',
      'Winter precipitation:latitude_s',
      'Spring precipitation:latitude_s',
      'Spring precipitation^2:latitude_s'
    ))) %>% 
  ggplot(aes(y = .variable, x = mean, xmin = .lower, xmax = .upper)) +
  facet_wrap(vars(species)) +
  geom_pointinterval(aes(fill = hyp), shape = 21, size = 5) + 
  geom_vline(xintercept = 0, color = "black", size = .5) + 
  scale_fill_gradient(low = "white", high = "red") + 
  scale_y_discrete(limits=rev) + 
  labs(y = "", x = "Effect size")
p.coefs.int

p.coefs / p.coefs.int 

ggsave(here("output","figs","brms_coef_plot.png"),
       device=ragg::agg_png, res=300, height = 6, width = 8)
ggsave(here("output","figs","brms_coef_plot.pdf"),
       device=cairo_pdf, height = 6, width = 8)
