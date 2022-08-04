


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



# Summer Temp ------

nd.1 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = seq(-4,4, length.out = 200), 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.2 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = seq(-4,4, length.out = 200), 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

pred1 <- bkt.mod %>% epred_draws(newdata = nd.1) %>% mutate(species="brook_trout")
pred2 <- bnt.mod %>% epred_draws(newdata = nd.2) %>% mutate(species="brown_trout")
pred.p <- bind_rows(pred1, pred2)

p.temp.su <- pred.p %>% 
  ggplot(aes(x = mean.tmax_summer, y = .epred)) +
  stat_lineribbon(.width = c(0.5, 0.8), alpha = .5) +
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(species), scales = "free_y") + 
  labs(x = "Max summer temperature",
       y = expression(Density~(fish~mile^{-1})),
       fill = "CI")  + 
  coord_cartesian(clip = "off", ylim = c(0,2000))

# p.temp.su.log <- pred.p %>% 
#   ggplot(aes(x = mean.tmax_summer, y = log(.epred))) +
#   geom_point(
#     data = df_trends_bkt0 %>% bind_rows(df_trends_bnt0),
#     aes(x = mean.tmax_summer, y = log(cpe)),
#     alpha = 1, shape=16, color = "grey") +
#   stat_lineribbon(.width = c(0.5, 0.8), alpha = .5) +
#   scale_fill_brewer(palette = "Reds") +
#   facet_wrap(vars(species), scales = "free_y") + 
#   labs(x = "Max summer temperature",
#        y = expression(Density~(fish~mile^{-1})),
#        fill = "CI")  

# p.1 <- pred1 %>%
#   ggplot(aes(x = mean.tmax_summer, y = .epred)) +
#   stat_lineribbon(.width = c(0.5, 0.8),) +
#   geom_point(
#     data = df_trends_bkt0,
#     aes(x = mean.tmax_summer, y = cpe), 
#     alpha = 0.1) + 
#   scale_fill_brewer(palette = "Reds") +
#   labs(x = "Max summer temperature", y = "YOY per mile",
#        fill = "CI") + 
#   facet_zoom(ylim = c(0,2000))
# 
# p.2 <- pred2 %>%
#   ggplot(aes(x = mean.tmax_summer, y = .epred)) +
#   stat_lineribbon() +
#   geom_point(
#     data = df_trends_bkt0,
#     aes(x = mean.tmax_summer, y = cpe), 
#     alpha = 0.1) + 
#   scale_fill_brewer(palette = "Reds") +
#   labs(x = "Max summer temperature", y = "YOY per mile",
#        fill = "CI") + 
#   facet_zoom(ylim = c(0,2000))


# ggsave(here("output","figs","temp_main_su_log.png"), p.temp.su.log,
#        device=agg_png, res=300, height = 4, width = 10)


# Fall Temp ------

nd.3 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = seq(-4,4, length.out = 200), 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.4 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = seq(-4,4, length.out = 200), 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

pred3 <- bkt.mod %>% epred_draws(newdata = nd.3) %>% mutate(species="Brook Trout")
pred4 <- bnt.mod %>% epred_draws(newdata = nd.4) %>% mutate(species="Brown Trout")
pred.p <- bind_rows(pred3, pred4)

p.temp.au <-  pred.p %>% 
  ggplot(aes(x = mean.tmax_autumn, y = .epred)) +
  stat_lineribbon(.width = c(0.5, 0.8),) +
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(species), scales = "free_y") + 
  labs(x = "Max Autmn temperature",
       y = expression(Density~(fish~mile^{-1})),
       fill = "CI")  + 
  coord_cartesian(clip = "off", ylim = c(0,1100))


# ggsave(here("output","figs","temp_main_au.png"), p.bkt.temp.au, 
#        device=agg_png, res=300, height = 4, width = 10)
# 


# Winter Temp ------

nd.5 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = seq(-4,4, length.out = 200), 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.6 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = seq(-4,4, length.out = 200), 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

pred5 <- bkt.mod %>% epred_draws(newdata = nd.5) %>% mutate(species="Brook Trout")
pred6 <- bnt.mod %>% epred_draws(newdata = nd.6) %>% mutate(species="Brown Trout")
pred.p <- bind_rows(pred5, pred6)

p.temp.wi <- pred.p %>% 
  ggplot(aes(x = mean.tmax_winter, y = .epred)) +
  stat_lineribbon(.width = c(0.5, 0.8),) +
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(species), scales = "free_y") + 
  labs(x = "Max winter temperature",
       y = expression(Density~(fish~mile^{-1})),
       fill = "CI")  + 
  coord_cartesian(clip = "off", ylim = c(0,1000))


# ggsave(here("output","figs","temp_main_wi.png"), p.bkt.temp.wi, 
#        device=agg_png, res=300, height = 4, width = 10)


# Spring Temp ------

nd.7 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = seq(-4,4, length.out = 200),
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.8 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = seq(-4,4, length.out = 200),
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

pred7 <- bkt.mod %>% epred_draws(newdata = nd.7) %>% mutate(species="Brook Trout")
pred8 <- bnt.mod %>% epred_draws(newdata = nd.8) %>% mutate(species="Brown Trout")
pred.p <- bind_rows(pred7, pred8)

p.temp.sp <-  pred.p %>% 
  ggplot(aes(x = mean.tmax_spring, y = .epred)) +
  stat_lineribbon(.width = c(0.5, 0.8),) +
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(species), scales = "free_y") + 
  labs(x = "Max spring temperature",
       y = expression(Density~(fish~mile^{-1})),
       fill = "CI")  + 
  coord_cartesian(clip = "off", ylim = c(0,700))


# pred7 %>%
#   ggplot(aes(x = mean.tmax_spring, y = .epred)) +
#   stat_lineribbon(.width = c(0.5, 0.8),) +
#   geom_point(
#     data = df_trends_bkt0,
#     aes(x = mean.tmax_spring, y = cpe), 
#     alpha = 0.1) + 
#   scale_fill_brewer(palette = "Reds") +
#   facet_zoom(ylim = c(0,2000))
# 
# pred8 %>%
#   ggplot(aes(x = mean.tmax_spring, y = .epred)) +
#   stat_lineribbon() +
#   geom_point(
#     data = df_trends_bkt0,
#     aes(x = mean.tmax_spring, y = cpe), 
#     alpha = 0.1) + 
#   scale_fill_brewer(palette = "Reds") +
#   facet_zoom(ylim = c(0,2000))

# ggsave(here("output","figs","temp_main_sp.png"), p.bkt.temp.sp, 
#        device=agg_png, res=300, height = 4, width = 10)


# Summer rain ------

nd.9 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = seq(-4,5, length.out = 200), 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.10 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = seq(-4,5, length.out = 200), 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

pred9 <- bkt.mod %>% epred_draws(newdata = nd.9) %>% mutate(species="Brook Trout")
pred10 <- bnt.mod %>% epred_draws(newdata = nd.10) %>% mutate(species="Brown Trout")
pred.p <- bind_rows(pred9, pred10)

p.rain.su <- pred.p %>% 
  ggplot(aes(x = total.prcp_summer, y = .epred)) +
  stat_lineribbon(.width = c(0.5, 0.8),) +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(species), scales = "free_y") + 
  labs(x = "Total summer precipiation",
       y = expression(Density~(fish~mile^{-1})),
       fill = "CI")  + 
  coord_cartesian(clip = "off", ylim = c(0,800)) + 
  scale_x_continuous(breaks = seq(-4,5,1))

# ggsave(here("output","figs","rain_main_su.png"), p.bkt.rain.su, 
#        device=agg_png, res=300, height = 4, width = 10)




# Fall rain ------

nd.11 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = seq(-4,5, length.out = 200), 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.12 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = seq(-4,5, length.out = 200), 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

pred11 <- bkt.mod %>% epred_draws(newdata = nd.11) %>% mutate(species="Brook Trout")
pred12 <- bnt.mod %>% epred_draws(newdata = nd.12) %>% mutate(species="Brown Trout")
pred.p <- bind_rows(pred11, pred12)

p.rain.au <-   pred.p %>% 
  ggplot(aes(x = total.prcp_autumn, y = .epred)) +
  stat_lineribbon(.width = c(0.5, 0.8),) +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(species), scales = "free_y") + 
  labs(x = "Total Autumn precipiation",
       y = expression(Density~(fish~mile^{-1})),
       fill = "CI")  + 
  coord_cartesian(clip = "off", ylim = c(0,800)) + 
  scale_x_continuous(breaks = seq(-4,5,1))
# 
# ggsave(here("output","figs","rain_main_au.png"), p.bkt.rain.au, 
#        device=agg_png, res=300, height = 4, width = 10)




# Winter rain ------

nd.13 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = seq(-4,5, length.out = 200), 
  total.prcp_spring = 0 
)


nd.14 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = seq(-4,5, length.out = 200), 
  total.prcp_spring = 0 
)

pred13 <- bkt.mod %>% epred_draws(newdata = nd.13) %>% mutate(species="Brook Trout")
pred14<- bnt.mod %>% epred_draws(newdata = nd.14) %>% mutate(species="Brown Trout")
pred.p <- bind_rows(pred13, pred14)

p.rain.wi <- pred.p %>% 
  ggplot(aes(x = total.prcp_winter, y = .epred)) +
  stat_lineribbon(.width = c(0.5, 0.8),) +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(species), scales = "free_y") + 
  labs(x = "Total winter precipiation",
       y = expression(Densityy~(fish~mile^{-1})),
       fill = "CI")  + 
  coord_cartesian(clip = "off", ylim = c(0,1100)) + 
  scale_x_continuous(breaks = seq(-4,5,1))

# pred13 %>% 
#   ggplot(aes(x = total.prcp_winter, y = .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette = "Blues") +
#   geom_point(data = df_trends_bkt0, 
#              aes(x = total.prcp_winter, y = cpe)) + ylim(0,2000)
# pred14 %>% 
#   ggplot(aes(x = total.prcp_winter, y = .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette = "Blues") +
#   geom_point(data = df_trends_bnt0, 
#              aes(x = total.prcp_winter, y = cpe)) + ylim(0,2000)

# ggsave(here("output","figs","rain_main_wi.png"), p.bkt.rain.wi, 
#        device=agg_png, res=300, height = 4, width = 10)



# Spring rain ------

nd.15 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = seq(-4,5, length.out = 200) 
)

nd.16 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = seq(-4,5, length.out = 200) 
)

pred15 <- bkt.mod %>% epred_draws(newdata = nd.15) %>% mutate(species="Brook Trout")
pred16 <- bnt.mod %>% epred_draws(newdata = nd.16) %>% mutate(species="Brown Trout")
pred.p <- bind_rows(pred15, pred16)

p.rain.sp <- pred.p %>% 
  ggplot(aes(x = total.prcp_spring, y = .epred)) +
  stat_lineribbon(.width = c(0.5, 0.8),) +
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(species), scales = "free_y") + 
  labs(x = "Total spring precipiation",
       y = expression(Densityensity~(fish~mile^{-1})),
       fill = "CI")  + 
  coord_cartesian(clip = "off", ylim = c(0,700)) + 
  scale_x_continuous(breaks = seq(-4,5,1))


# pred15 %>%
#   ggplot(aes(x = total.prcp_spring, y = .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette = "Blues") +
#   geom_point(data = df_trends_bkt0,
#              aes(x = total.prcp_spring, y = cpe)) 
# pred16 %>%
#   ggplot(aes(x = total.prcp_spring, y = .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette = "Blues") +
#   geom_point(data = df_trends_bnt0,
#              aes(x = total.prcp_spring, y = cpe))


# ggsave(here("output","figs","rain_main_sp.png"), p.bkt.rain.sp,
#        device=agg_png, res=300, height = 4, width = 10)



# Panel =====================================================

a <- ((p.temp.su | p.rain.su) / 
        (p.temp.au | p.rain.au) / 
        (p.temp.wi | p.rain.wi) / 
        (p.temp.sp | p.rain.sp))

p.panel <- 
  a | p.coefs 


ggsave(here("output","figs","epred_main_effects_panel.png"), p.panel,
       device=ragg::agg_png, res=300, height = 8, width = 16)

