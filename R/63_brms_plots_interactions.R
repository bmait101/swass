
theme_clean <- function() {
  theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.title = element_text(size = rel(1), margin = margin(0,0,0,0,"cm")),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = .5),
      panel.grid = element_blank(),
      panel.spacing = unit(.5, "lines"),
      axis.ticks = element_line(size = 0.5, color = "black"),
      axis.ticks.length = unit(.2, 'cm'),
      strip.text = element_text(hjust = 0.5),
      strip.background = element_rect(color = NA, fill = "white"),
      legend.margin = margin(0,0,0,0,'cm'), 
      legend.position = "none"
    )
}

theme_set(theme_clean())


# # use for Class 1 models
# df_analysis_bkt0 <- df_analysis_bkt0 %>% 
#   filter(trout_class %in% c("CLASS I","CLASS II")) %>% droplevels()
# df_analysis_bnt0 <- df_analysis_bnt0 %>% 
#   filter(trout_class %in% c("CLASS I","CLASS II")) %>% droplevels()


# pred1 %>% 
#   ggplot(aes(x = mean.tmax_summer, y = .epred, 
#              group = latitude_f)) +
#   stat_lineribbon(.width = c(0.5, 0.8)) + 
#   scale_fill_brewer(palette = "Reds") +
#   facet_wrap(vars(latitude_f), scales = "free_y") +
#   labs(x = "Max summer temperature",
#        y = expression(Density~(fish~km^{-1})),
#        fill = "CI", title = "(a) Brook Trout - summer max temperature") + 
#   coord_cartesian(clip = "off", ylim = c(0,1300))


# Summer temp ----------------------------------------


nd.1 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bkt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
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
  reach_id = levels(df_analysis_bnt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = seq(-4,4, length.out = 200), 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)


pred1 <- bkt.mod %>% 
  epred_draws(newdata = nd.1) %>% 
  mutate(species="Brook Trout", 
         latitude_f = as.factor(latitude_s)) %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))
pred2 <- bnt.mod %>% 
  epred_draws(newdata = nd.2) %>%
  mutate(species="Brown Trout", 
         latitude_f = as.factor(latitude_s))  %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))

p.bkt.temp.su.x <- pred1 %>% 
  ggplot(aes(x = mean.tmax_summer, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Max summer temperature",
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI")

p.bnt.temp.su.x <- pred2 %>% 
  ggplot(aes(x = mean.tmax_summer, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Max summer temperature",
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 




# Fall temp ----------------------------------------

nd.1 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bkt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = seq(-4,4, length.out = 200), 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.2 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bnt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = seq(-4,4, length.out = 200), 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)


pred3 <- bkt.mod %>% 
  epred_draws(newdata = nd.1) %>% 
  mutate(species="Brook Trout", 
         latitude_f = as.factor(latitude_s)) %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))
pred4 <- bnt.mod %>% 
  epred_draws(newdata = nd.2) %>%
  mutate(species="Brown Trout", 
         latitude_f = as.factor(latitude_s))  %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))

p.bkt.temp.au.x <- pred3 %>% 
  ggplot(aes(x = mean.tmax_autumn, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Max Autumn temperature", 
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 

p.bnt.temp.au.x <- pred4 %>% 
  ggplot(aes(x = mean.tmax_autumn, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8),) + 
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Max Autumn temperature", 
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI")  



# p.temp.au.int <- 
#   p.bkt.temp.au.x / p.bnt.temp.au.x + 
#   plot_layout(guides = "collect") &
#   theme(legend.position='right', 
#         plot.margin = margin(.1,.1,.1,.1, unit = 'cm'), 
#         panel.background = element_rect(color = NA))

# 
# ggsave(here("output","figs","temp_main_au_interact.png"),
#        p.temp.au.int,
#        device=agg_png, res=300, height = 6.5, width = 11)



# Winter temp ----------------------------------------

nd.1 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bkt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = seq(-4,4, length.out = 200), 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.2 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bnt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = seq(-4,4, length.out = 200), 
  mean.tmax_spring = 0,
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)


pred5 <- bkt.mod %>% 
  epred_draws(newdata = nd.1) %>% 
  mutate(species="Brook Trout", 
         latitude_f = as.factor(latitude_s)) %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))

pred6 <- bnt.mod %>% 
  epred_draws(newdata = nd.2) %>%
  mutate(species="Brown Trout", 
         latitude_f = as.factor(latitude_s))  %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))

p.bkt.temp.wi.x <- pred5 %>% 
  ggplot(aes(x = mean.tmax_winter, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Max winter temperature",
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 

p.bnt.temp.wi.x <- pred6 %>% 
  ggplot(aes(x = mean.tmax_winter, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Max winter temperature", 
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 

# p.temp.wi.int <- 
#   p.bkt.temp.wi.x / p.bnt.temp.wi.x + 
#   plot_layout(guides = "collect") &
#   theme(legend.position='right', 
#         plot.margin = margin(.1,.1,.1,.1, unit = 'cm'), 
#         panel.background = element_rect(color = NA))


# ggsave(here("output","figs","temp_main_wi_interact.png"),
#        p.temp.wi.int,
#        device=agg_png, res=300, height = 6.5, width = 11)





# Spring temp ----------------------------------------

nd.1 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bkt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = seq(-4,4, length.out = 200),
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.2 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bnt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = seq(-4,4, length.out = 200),
  total.prcp_summer =0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)


pred7 <- bkt.mod %>% 
  epred_draws(newdata = nd.1) %>% 
  mutate(species="Brook Trout", 
         latitude_f = as.factor(latitude_s)) %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))
pred8 <- bnt.mod %>% 
  epred_draws(newdata = nd.2) %>%
  mutate(species="Brown Trout", 
         latitude_f = as.factor(latitude_s))  %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))

p.bkt.temp.sp.x <- pred7 %>% 
  ggplot(aes(x = mean.tmax_spring, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Max spring temperature", 
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 

p.bnt.temp.sp.x <- pred8 %>% 
  ggplot(aes(x = mean.tmax_spring, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Max spring temperature",
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 



# p.temp.sp.int <- 
#   p.bkt.temp.sp.x / p.bnt.temp.sp.x + 
#   plot_layout(guides = "collect") &
#   theme(legend.position='right', 
#         plot.margin = margin(.1,.1,.1,.1, unit = 'cm'), 
#         panel.background = element_rect(color = NA))



# ggsave(here("output","figs","temp_main_sp_interact.png"),
#        p.temp.sp.int,
#        device=agg_png, res=300, height = 6.5, width = 11)



# Summer rain ----------------------------------------


nd.1 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bkt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = seq(-4,5, length.out = 200), 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)

nd.2 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bnt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = seq(-4,5, length.out = 200), 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)


pred9 <- bkt.mod %>% 
  epred_draws(newdata = nd.1) %>% 
  mutate(species="Brook Trout", 
         latitude_f = as.factor(latitude_s)) %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))
pred10 <- bnt.mod %>% 
  epred_draws(newdata = nd.2) %>%
  mutate(species="Brown Trout", 
         latitude_f = as.factor(latitude_s))  %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))

p.bkt.rain.su.x <- pred9 %>% 
  ggplot(aes(x = total.prcp_summer, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Summer precipitation", 
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 

p.bnt.rain.su.x <- pred10 %>% 
  ggplot(aes(x = total.prcp_summer, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Summer precipitation",
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 



# p.rain.su.panel <- 
#   p.bkt.rain.su.x / p.bnt.rain.su.x + 
#   plot_layout(guides = "collect") &
#   theme(legend.position='right', 
#         plot.margin = margin(.1,.1,.1,.1, unit = 'cm'), 
#         panel.background = element_rect(color = NA))

# ggsave(here("output","figs","epred_rain_su_x.png"),
#        p.rain.panel,
#        device=agg_png, res=300, height = 6.5, width = 11)




# Fall rain ----------------------------------------

nd.1 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bkt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = seq(-4,5, length.out = 200), 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)


nd.2 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bnt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = seq(-4,5, length.out = 200), 
  total.prcp_winter = 0, 
  total.prcp_spring = 0 
)


pred11 <- bkt.mod %>% 
  epred_draws(newdata = nd.1) %>% 
  mutate(species="Brook Trout", 
         latitude_f = as.factor(latitude_s)) %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))
pred12 <- bnt.mod %>% 
  epred_draws(newdata = nd.2) %>%
  mutate(species="Brown Trout", 
         latitude_f = as.factor(latitude_s))  %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))

p.bkt.rain.au.x <- pred11 %>% 
  ggplot(aes(x = total.prcp_autumn, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Autumn precipitation", 
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 

p.bnt.rain.au.x <- pred12 %>% 
  ggplot(aes(x = total.prcp_autumn, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Autumn precipitation",
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 



# p.rain.au.panel <- 
#   p.bkt.rain.au.x / p.bnt.rain.au.x + 
#   plot_layout(guides = "collect") &
#   theme(legend.position='right', 
#         plot.margin = margin(.1,.1,.1,.1, unit = 'cm'), 
#         panel.background = element_rect(color = NA))

# ggsave(here("output","figs","epred_rain_au_x.png"),
#        p.rain.au.panel,
#        device=agg_png, res=300, height = 6.5, width = 11)



# Winter rain ----------------------------------------

nd.1 <- expand_grid(
  total_effort = 1,year_s = 0,
  reach_id = levels(df_analysis_bkt0$reach_id)[[1]],
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5),
  mean.tmax_summer = 0,
  mean.tmax_autumn = 0,
  mean.tmax_winter = 0,
  mean.tmax_spring = 0,
  total.prcp_summer =0,
  total.prcp_autumn = 0,
  total.prcp_winter = seq(-4,5, length.out = 200),
  total.prcp_spring = 0
)

nd.2 <- expand_grid(
  total_effort = 1,year_s = 0,
  reach_id = levels(df_analysis_bnt0$reach_id)[[1]],
  gradient = 0, stream_order = 0,
  latitude_s = c(-1,0.5,1.5),
  mean.tmax_summer = 0,
  mean.tmax_autumn = 0,
  mean.tmax_winter = 0,
  mean.tmax_spring = 0,
  total.prcp_summer =0,
  total.prcp_autumn = 0,
  total.prcp_winter = seq(-4,5, length.out = 200),
  total.prcp_spring = 0
)


pred13 <- bkt.mod %>%
  epred_draws(newdata = nd.1) %>%
  mutate(species="Brook Trout",
         latitude_f = as.factor(latitude_s)) %>%
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))
pred14 <- bnt.mod %>%
  epred_draws(newdata = nd.2) %>%
  mutate(species="Brown Trout",
         latitude_f = as.factor(latitude_s))  %>%
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))

p.bkt.rain.wi.x <- pred13 %>%
  ggplot(aes(x = total.prcp_winter, y = .epred * 1.609,
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Winter precipitation", 
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI") 

p.bnt.rain.wi.x <- pred14 %>%
  ggplot(aes(x = total.prcp_winter, y = .epred * 1.609,
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Winter precipitation",
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI")



# p.rain.wi.panel <- 
#   p.bkt.rain.wi.x / p.bnt.rain.wi.x + 
#   plot_layout(guides = "collect") &
#   theme(legend.position='right', 
#         plot.margin = margin(.1,.1,.1,.1, unit = 'cm'), 
#         panel.background = element_rect(color = NA))

# ggsave(here("output","figs","epred_rain_wi_x.png"),
#        p.rain.wi.panel,
#        device=agg_png, res=300, height = 6.5, width = 11)



# Spring rain ----------------------------------------

nd.1 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bkt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, mean.tmax_spring = 0,
  total.prcp_summer = 0, total.prcp_autumn = 0, total.prcp_winter = 0, 
  total.prcp_spring = seq(-4,5, length.out = 200) 
)

nd.2 <- expand_grid(
  total_effort = 1,year_s = 0, 
  reach_id = levels(df_analysis_bnt0$reach_id)[[1]], 
  gradient = 0, stream_order = 0,latitude_s = c(-1,0.5,1.5), 
  mean.tmax_summer = 0, mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, mean.tmax_spring = 0,
  total.prcp_summer = 0, total.prcp_autumn = 0, total.prcp_winter = 0, 
  total.prcp_spring = seq(-4,5, length.out = 200) 
)


pred15 <- bkt.mod %>% 
  epred_draws(newdata = nd.1) %>% 
  mutate(species="Brook Trout", 
         latitude_f = as.factor(latitude_s)) %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))
pred16 <- bnt.mod %>% 
  epred_draws(newdata = nd.2) %>%
  mutate(species="Brown Trout", 
         latitude_f = as.factor(latitude_s))  %>% 
  mutate(latitude_f = recode(
    latitude_f, "-1" = "South WI", "0.5" = "Mid WI", "1.5" = "North WI"))

p.bkt.rain.sp.x <- pred15 %>% 
  ggplot(aes(x = total.prcp_spring, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Spring precipitation", 
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI")

p.bnt.rain.sp.x <- pred16 %>% 
  ggplot(aes(x = total.prcp_spring, y = .epred * 1.609, 
             group = latitude_f)) +
  stat_lineribbon(.width = c(0.5, 0.8)) + 
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(vars(fct_rev(latitude_f)), scales = "free_y", nrow=3, ncol=1) +
  labs(x = "Spring precipitation", 
       y = expression(Recruitment~strength~(YOY~km^{-1})),
       fill = "CI")

# p.rain.sp.panel <- 
#   p.bkt.rain.sp.x / p.bnt.rain.sp.x + 
#   plot_layout(guides = "collect") &
#   theme(legend.position='right', 
#         plot.margin = margin(.1,.1,.1,.1, unit = 'cm'), 
#         panel.background = element_rect(color = NA))

# ggsave(here("output","figs","epred_rain_sp_x.png"),
#        p.rain.sp.panel,
#        device=agg_png, res=300, height = 6.5, width = 11)


# Plots ============================================


# Temp ---------------------------------

# p.bkt.temp.su.x
# p.bkt.temp.au.x
# p.bkt.temp.wi.x
# p.bkt.temp.sp.x
# 
# p.bnt.temp.su.x
# p.bnt.temp.au.x
# p.bnt.temp.wi.x
# p.bnt.temp.sp.x

p.temp.x <- 
  (p.bkt.temp.su.x |
     (p.bkt.temp.au.x + theme(axis.title.y = element_blank())) |
     (p.bkt.temp.wi.x + theme(axis.title.y = element_blank())) | 
     (p.bkt.temp.sp.x + theme(axis.title.y = element_blank()))
   )/
  (p.bnt.temp.su.x | 
     (p.bnt.temp.au.x + theme(axis.title.y = element_blank())) |
     (p.bnt.temp.wi.x + theme(axis.title.y = element_blank())) | 
     (p.bnt.temp.sp.x + theme(axis.title.y = element_blank()))
  )

# save plot
path <- here::here("output","figs1","fig4_temp_panel")
ggsave(
  glue::glue("{path}.pdf"), 
  plot = p.temp.x, 
  width = 10, 
  height = 12, 
  device = cairo_pdf
)
# manually add panel letters then covert 
pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  format = "png", 
  dpi = 600
)


# ggsave(here("output","figs","epred_x_temp.png"),p.temp.x, 
#        device=agg_png, res=300, height = 12, width = 10)
# ggsave(here("output","figs","epred_x_temp.pdf"),p.temp.x, 
#        device=cairo_pdf, height = 12, width = 10)


# a <- p.bkt.temp.su.x | (p.bnt.temp.su.x + theme(axis.title.y = element_blank()))
# 
# ggsave(here("output","figs","epred_x_temp_su.png"), a, 
#        device=agg_png, res=600, height = 6.5, width = 7)
# 
# b <- p.bkt.temp.sp.x | (p.bnt.temp.sp.x + theme(axis.title.y = element_blank()))
# 
# ggsave(here("output","figs","epred_x_temp_sp.png"), b, 
#        device=agg_png, res=600, height = 6.5, width = 7)


# Rain -----------------------


# p.bkt.rain.su.x
# p.bkt.rain.au.x
# p.bkt.rain.wi.x
# p.bkt.rain.sp.x
# 
# p.bnt.rain.su.x
# p.bnt.rain.au.x
# p.bnt.rain.wi.x
# p.bnt.rain.sp.x

p.rain.x <- 
  (p.bkt.rain.su.x |
     (p.bkt.rain.au.x + theme(axis.title.y = element_blank())) |
     (p.bkt.rain.wi.x + theme(axis.title.y = element_blank())) | 
     (p.bkt.rain.sp.x + theme(axis.title.y = element_blank()))
   ) /
  (p.bnt.rain.su.x | 
     (p.bnt.rain.au.x + theme(axis.title.y = element_blank())) |
     (p.bnt.rain.wi.x + theme(axis.title.y = element_blank())) | 
     (p.bnt.rain.sp.x + theme(axis.title.y = element_blank()))
  )


# save plot
path <- here::here("output","figs1","fig5_rain_panel")
ggsave(
  glue::glue("{path}.pdf"), 
  plot = p.rain.x, 
  width = 10, 
  height = 12, 
  device = cairo_pdf
)
# manually add fish images then covert 
pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  format = "png", 
  dpi = 600
)

# ggsave(here("output","figs","epred_x_rain.png"), p.rain.x,
#        device=agg_png, res=300, height = 12, width = 10)
# ggsave(here("output","figs","epred_x_rain.pdf"), p.rain.x,
#        device=cairo_pdf, height = 12, width = 10)


# a <- p.bkt.rain.su.x | (p.bnt.rain.su.x + theme(axis.title.y = element_blank()))
# 
# ggsave(here("output","figs","epred_x_rain_su.png"), a, 
#        device=agg_png, res=600, height = 6.5, width = 7)
# 
# b <- p.bkt.rain.wi.x | (p.bnt.rain.wi.x + theme(axis.title.y = element_blank()))
# 
# ggsave(here("output","figs","epred_x_rain_wi.png"), b, 
#        device=agg_png, res=600, height = 6.5, width = 7)
# 
# c <- p.bkt.rain.sp.x | (p.bnt.rain.sp.x + theme(axis.title.y = element_blank()))
# 
# ggsave(here("output","figs","epred_x_rain_sp.png"), c, 
#        device=agg_png, res=600, height = 6.5, width = 7)

