

# Set up =======================================================================

library(mgcv)

# use multiple threads for fitting
ctrl <- gam.control(nthreads = 4)


# Data =========================================================================

source(here::here("R", "50_prep_data.R"))

# df_trends_bkt0 <- df_trends_bkt0 %>% 
#   filter(trout_class %in% c("CLASS I","CLASS II")) %>%
#   droplevels()
# 
# # Adult brook trout
# df_trends_bkt1 <- df_trends_bkt1 %>% 
#   filter(trout_class %in% c("CLASS I","CLASS II")) %>% 
#   droplevels()
# 
# # YOY brown trout
# df_trends_bnt0 <- df_trends_bnt0 %>%
#   filter(trout_class %in% c("CLASS I","CLASS II")) %>% 
#   droplevels()
# 
# # Adult brown trout
# df_trends_bnt1 <- df_trends_bnt1 %>%
#   filter(trout_class %in% c("CLASS I","CLASS II"))  %>% 
#   droplevels()

# YOY BROOKIES =================================================================

# statewide trends (G): 30 seconds
system.time(
  bkt0.m1 <- gam(total_catch ~ 
                   s(year, k=15) + 
                   s(reach_id, bs = "re") + 
                   offset(log(total_effort)), 
                 data = df_trends_bkt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt0.m1, here("output", "models", "trends", "bkt0.m1.rds"))


# stream trends (S):  7.3  minutes
system.time(
  bkt0.m2 <- gam(total_catch ~
                   s(year, reach_id, bs = "fs", k = 5) + 
                   offset(log(total_effort)),
                 data = df_trends_bkt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt0.m2, here("output", "models", "trends", "bkt0.m2.rds"))




# statewide and stream trends (GS): 10 minutes
system.time(
  bkt0.m3 <- gam(total_catch ~
                   s(year, k=15) + 
                   s(year, reach_id, bs = "fs", k = 5) + 
                   offset(log(total_effort)),
                 data = df_trends_bkt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt0.m3, here("output", "models", "trends", "bkt0.m3.rds"))
# saveRDS(bkt0.m3, here("output", "models", "trends", "bkt0.m3_12.rds"))


# region-specific trends: 5 minutes
system.time(
  bkt0.m4 <- gam(total_catch ~ huc_names8 +
                   s(year, by = huc_names8) +
                   s(reach_id, bs = "re") +
                   offset(log(total_effort)),
                 data = df_trends_bkt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt0.m4, here("output", "models", "trends", "bkt0.m4.rds"))


# region-specific trends, plus stream trends: 99 minutes
system.time(
  bkt0.m5 <- gam(total_catch ~ huc_names8 +
                   s(year, by = huc_names8) +
                   s(year, reach_id, bs = "fs", k = 5) +
                   offset(log(total_effort)),
                 data = df_trends_bkt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt0.m5, here("output", "models", "trends", "bkt0.m5.rds"))



# best fit with y2y: 9 min
system.time(
  bkt0.m3.y2y <- gam(total_catch ~
                   s(year, k=15) + 
                   s(year, reach_id, bs = "fs", k = 5) + 
                   s(year_f, bs = "re") + 
                   offset(log(total_effort)),
                 data = df_trends_bkt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt0.m3.y2y, here("output", "models", "trends", "bkt0.m3.y2y.rds"))
# saveRDS(bkt0.m3.y2y, here("output", "models", "trends", "bkt0.m3.y2y_12.rds"))


# Covariate models ---------------

# covariates with stream statewide and stream trends: 17 min
system.time(
  bkt0.cov <- gam(total_catch ~
                    s(year_s, k = 15) +
                    s(year_s, reach_id, bs = "fs", k = 5) +
                    s(total.prcp_summer, k=5) +
                    s(total.prcp_autumn, k=5) +
                    s(total.prcp_winter, k=5) +
                    s(total.prcp_spring, k=5) +
                    s(mean.tmax_summer, k=5) +
                    s(mean.tmax_autumn, k=5) +
                    s(mean.tmax_winter, k=5) +
                    s(mean.tmax_spring, k=5) +
                    s(latitude_s, k=3) +
                    s(gradient, k=3) +
                    s(stream_order, k=3) +
                    offset(log(total_effort)),
                  family = nb(),
                  method = "REML",
                  control = ctrl,
                  data = df_trends_bkt0)
)

saveRDS(bkt0.cov, here("output", "models", "trends", "bkt0.cov.rds"))

# covariates with y2y: 64 min
system.time(
  bkt0.covy <- gam(total_catch ~
                    s(year_s, k = 15) +
                    s(year_s, reach_id, bs = "fs", k = 5) +
                    s(total.prcp_summer, k=5) +
                    s(total.prcp_autumn, k=5) +
                    s(total.prcp_winter, k=5) +
                    s(total.prcp_spring, k=5) +
                    s(mean.tmax_summer, k=5) +
                    s(mean.tmax_autumn, k=5) +
                    s(mean.tmax_winter, k=5) +
                    s(mean.tmax_spring, k=5) +
                    s(latitude_s, k=3) +
                    s(gradient, k=3) +
                    s(stream_order, k=3) +
                    s(year_f, bs = "re") +
                    offset(log(total_effort)),
                  family = nb(),
                  method = "REML",
                  control = ctrl,
                  data = df_trends_bkt0)
)

saveRDS(bkt0.covy, here("output", "models", "trends", "bkt0.covy.rds"))


# add interactions:  min
system.time(
  bkt0.cov2 <- gam(total_catch ~
                  s(year_s, k = 15) +
                  s(year_s, reach_id, bs = "fs", k = 5) +
                  # s(year_f, bs = "re") +
                  s(total.prcp_summer, k=5) +
                  s(total.prcp_autumn, k=5) +
                  s(total.prcp_winter, k=5) +
                  s(total.prcp_spring, k=5) +
                  s(mean.tmax_summer, k=5) +
                  s(mean.tmax_autumn, k=5) +
                  s(mean.tmax_winter, k=5) +
                  s(mean.tmax_spring, k=5) +
                  s(latitude_s, k=3) +
                  s(gradient, k=3) +
                  s(stream_order, k=3) +
                  ti(latitude_s, mean.tmax_summer) +
                  ti(latitude_s, mean.tmax_autumn) +
                  ti(latitude_s, mean.tmax_winter) +
                  ti(latitude_s, mean.tmax_spring) +
                  offset(log(total_effort)),
                family = nb(),
                method = "REML",
                control = ctrl,
                data = df_trends_bkt0)
)

saveRDS(bkt0.cov2, here("output", "models", "trends", "bkt0.cov2.rds"))


# add y2y: 36
system.time(
  bkt0.cov3 <- gam(total_catch ~
                     s(year_s, k = 15) +
                     s(year_s, reach_id, bs = "fs", k = 5) +
                     s(year_f, bs = "re") +
                     s(total.prcp_summer, k=5) +
                     s(total.prcp_autumn, k=5) +
                     s(total.prcp_winter, k=5) +
                     s(total.prcp_spring, k=5) +
                     s(mean.tmax_summer, k=5) +
                     s(mean.tmax_autumn, k=5) +
                     s(mean.tmax_winter, k=5) +
                     s(mean.tmax_spring, k=5) +
                     s(latitude_s, k=3) +
                     s(gradient, k=3) +
                     s(stream_order, k=3) +
                     ti(latitude_s, mean.tmax_summer) +
                     ti(latitude_s, mean.tmax_autumn) +
                     ti(latitude_s, mean.tmax_winter) +
                     ti(latitude_s, mean.tmax_spring) +
                     offset(log(total_effort)),
                   family = nb(),
                   method = "REML",
                   control = ctrl,
                   data = df_trends_bkt0)
)

saveRDS(bkt0.cov3, here("output", "models", "trends", "bkt0.cov3.rds"))



# ADULT BROOKIES =========================================================


# statewide trends: 2 min
system.time(
  bkt1.m1 <- gam(total_catch ~ 
                   s(year, k=15) + 
                   s(reach_id, bs = "re") + 
                   offset(log(total_effort)), 
                 data = df_trends_bkt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt1.m1, here("output", "models", "trends", "bkt1.m1.rds"))

# stream trends: 59 minutes
system.time(
  bkt1.m2 <- gam(total_catch ~
                   s(year, reach_id, bs = "fs", k = 5) + 
                   offset(log(total_effort)),
                 data = df_trends_bkt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt1.m2, here("output", "models", "trends", "bkt1.m2.rds"))

# statewide and stream trends: 41 minutes
system.time(
  bkt1.m3 <- gam(total_catch ~
                   s(year, k=15) + 
                   s(year, reach_id, bs = "fs", k = 5) + 
                   offset(log(total_effort)),
                 data = df_trends_bkt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt1.m3, here("output", "models", "trends", "bkt1.m3.rds"))
# saveRDS(bkt1.m3, here("output", "models", "trends", "bkt1.m3_12.rds"))

# region-specific trends: 17 minutes
system.time(
  bkt1.m4 <- gam(total_catch ~ huc_names8 +
                   s(year, by = huc_names8) +
                   s(reach_id, bs = "re") +
                   offset(log(total_effort)),
                 data = df_trends_bkt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt1.m4, here("output", "models", "trends", "bkt1.m4.rds"))

# region-specific trends, plus stream trends: 96 minutes
system.time(
  bkt1.m5 <- gam(total_catch ~ huc_names8 +
                   s(year, by = huc_names8) +
                   s(year, reach_id, bs = "fs", k = 5) +
                   offset(log(total_effort)),
                 data = df_trends_bkt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)

saveRDS(bkt1.m5, here("output", "models", "trends", "bkt1.m5.rds"))


# statewide and stream trends with y2y: 48 minutes
system.time(
  bkt1.m3.y2y <- gam(total_catch ~
                   s(year, k=15) + 
                   s(year, reach_id, bs = "fs", k = 5) + 
                   s(year_f, bs = "re") +  
                   offset(log(total_effort)),
                 data = df_trends_bkt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bkt1.m3.y2y, here("output", "models", "trends", "bkt1.m3.y2y.rds"))
# saveRDS(bkt1.m3.y2y, here("output", "models", "trends", "bkt1.m3.y2y_12.rds"))


# YOY BROWN TROUT ========================================================


# statewide trends: 52 seconds
system.time(
  bnt0.m1 <- gam(total_catch ~ 
                   s(year, k=15) + 
                   s(reach_id, bs = "re") + 
                   offset(log(total_effort)), 
                 data = df_trends_bnt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt0.m1, here("output", "models", "trends", "bnt0.m1.rds"))


# stream trends: 11 minutes
system.time(
  bnt0.m2 <- gam(total_catch ~
                   s(year, reach_id, bs = "fs", k = 5) + 
                   offset(log(total_effort)),
                 data = df_trends_bnt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt0.m2, here("output", "models", "trends", "bnt0.m2.rds"))


# statewide and stream trends: 20 minutes
system.time(
  bnt0.m3 <- gam(total_catch ~
                   s(year, k=15) + 
                   s(year, reach_id, bs = "fs", k = 5) + 
                   offset(log(total_effort)),
                 data = df_trends_bnt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt0.m3, here("output", "models", "trends", "bnt0.m3.rds"))
# saveRDS(bnt0.m3, here("output", "models", "trends", "bnt0.m3_12.rds"))


# region-specific trends: 8 minutes
system.time(
  bnt0.m4 <- gam(total_catch ~ huc_names8 +
                   s(year, by = huc_names8) +
                   s(reach_id, bs = "re") +
                   offset(log(total_effort)),
                 data = df_trends_bnt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt0.m4, here("output", "models", "trends", "bnt0.m4.rds"))


# region-specific trends, plus stream trends: 44 minutes
system.time(
  bnt0.m5 <- gam(total_catch ~ huc_names8 +
                   s(year, by = huc_names8) +
                   s(year, reach_id, bs = "fs", k = 5) +
                   offset(log(total_effort)),
                 data = df_trends_bnt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)

saveRDS(bnt0.m5, here("output", "models", "trends", "bnt0.m5.rds"))


# statewide and stream trends with y2y: 20 minutes
system.time(
  bnt0.m3.y2y <- gam(total_catch ~
                   s(year, k=15) + 
                   s(year, reach_id, bs = "fs", k = 5) + 
                   s(year_f, bs = "re") +  
                   offset(log(total_effort)),
                 data = df_trends_bnt0,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt0.m3.y2y, here("output", "models", "trends", "bnt0.m3.y2y.rds"))
# saveRDS(bnt0.m3.y2y, here("output", "models", "trends", "bnt0.m3.y2y_12.rds"))


# Covariate models -----------------

# # covariates with stream statewide and stream trends: 30 min
# system.time(
#   bnt0.cov <- gam(total_catch ~ 
#                  s(year_s, k = 15) + 
#                  s(year_s, reach_id, bs = "fs", k = 5) +
#                  s(total.prcp_summer, k=5) + 
#                  s(total.prcp_autumn, k=5) + 
#                  s(total.prcp_winter, k=5) + 
#                  s(total.prcp_spring, k=5) +
#                  s(mean.tmax_summer, k=5) + 
#                  s(mean.tmax_autumn, k=5) + 
#                  s(mean.tmax_winter, k=5) + 
#                  s(mean.tmax_spring, k=5) + 
#                  s(latitude_s, k=3) + 
#                  s(gradient, k=3) + 
#                  s(stream_order, k=3) + 
#                  offset(log(total_effort)), 
#                family = nb(),
#                method = "REML",
#                control = ctrl,
#                data = df_trends_bnt0)
# )
# 
# 
# saveRDS(bnt0.cov, here("output", "models", "trends", "bnt0.cov.rds"))
# 
# 
# covariates y2y: 30 min
# system.time(
#   bnt0.covy <- gam(total_catch ~
#                     s(year_s, k = 15) +
#                     s(year_s, reach_id, bs = "fs", k = 5) +
#                     s(year_f, bs = "re")  +
#                     s(total.prcp_summer, k=5) +
#                     s(total.prcp_autumn, k=5) +
#                     s(total.prcp_winter, k=5) +
#                     s(total.prcp_spring, k=5) +
#                     s(mean.tmax_summer, k=5) +
#                     s(mean.tmax_autumn, k=5) +
#                     s(mean.tmax_winter, k=5) +
#                     s(mean.tmax_spring, k=5) +
#                     s(latitude_s, k=3) +
#                     s(gradient, k=3) +
#                     s(stream_order, k=3) +
#                     offset(log(total_effort)),
#                   family = nb(),
#                   method = "REML",
#                   control = ctrl,
#                   data = df_trends_bnt0)
# )
# 
# saveRDS(bnt0.covy, here("output", "models", "trends", "bnt0.covy.rds"))



# add interactions: 1.8 hrs

system.time(
  bnt0.cov2 <- gam(total_catch ~
                  s(year_s, k = 15) +
                  s(year_s, reach_id, bs = "fs", k = 5) +
                  s(total.prcp_summer, k=5) +
                  s(total.prcp_autumn, k=5) +
                  s(total.prcp_winter, k=5) +
                  s(total.prcp_spring, k=5) +
                  s(mean.tmax_summer, k=5) +
                  s(mean.tmax_autumn, k=5) +
                  s(mean.tmax_winter, k=5) +
                  s(mean.tmax_spring, k=5) +
                  s(latitude_s, k=3) +
                  s(gradient, k=3) +
                  s(stream_order, k=3) +
                  ti(latitude_s, mean.tmax_summer) +
                  ti(latitude_s, mean.tmax_autumn) +
                  ti(latitude_s, mean.tmax_winter) +
                  ti(latitude_s, mean.tmax_spring) +
                  offset(log(total_effort)),
                family = nb(),
                method = "REML",
                control = ctrl,
                data = df_trends_bnt0)
)


saveRDS(bnt0.cov2, here("output", "models", "trends", "bnt0.cov2.rds"))


# add y2y: 2.7 hours
system.time(
  bnt0.cov3 <- gam(total_catch ~
                    s(year_s, k = 15) +
                    s(year_s, reach_id, bs = "fs", k = 5) +
                    s(year_f, bs = "re")  +
                    s(total.prcp_summer, k=5) +
                    s(total.prcp_autumn, k=5) +
                    s(total.prcp_winter, k=5) +
                    s(total.prcp_spring, k=5) +
                    s(mean.tmax_summer, k=5) +
                    s(mean.tmax_autumn, k=5) +
                    s(mean.tmax_winter, k=5) +
                    s(mean.tmax_spring, k=5) +
                    s(latitude_s, k=3) +
                    s(gradient, k=3) +
                    s(stream_order, k=3) +
                    ti(latitude_s, mean.tmax_summer) +
                    ti(latitude_s, mean.tmax_autumn) +
                    ti(latitude_s, mean.tmax_winter) +
                    ti(latitude_s, mean.tmax_spring) +
                    offset(log(total_effort)),
                  family = nb(),
                  method = "REML",
                  control = ctrl,
                  data = df_trends_bnt0)
)


saveRDS(bnt0.cov3, here("output", "models", "trends", "bnt0.cov3.rds"))
# 
# 
# # remove non-sig interactions: 2.7 hours
# system.time(
#   bnt0.cov4<- gam(total_catch ~
#                      s(year_s, k = 15) +
#                      s(year_s, reach_id, bs = "fs", k = 5) +
#                      s(year_f, bs = "re")  +
#                      s(total.prcp_summer, k=5) + 
#                      s(total.prcp_autumn, k=5) + 
#                      s(total.prcp_winter, k=5) + 
#                      s(total.prcp_spring, k=5) +
#                      s(mean.tmax_summer, k=5) + 
#                      s(mean.tmax_autumn, k=5) + 
#                      s(mean.tmax_winter, k=5) + 
#                      s(mean.tmax_spring, k=5) + 
#                      s(latitude_s, k=3) +
#                      s(gradient, k=3) +
#                      s(stream_order, k=3) +
#                      ti(latitude_s, mean.tmax_summer) +
#                      # ti(latitude_s, mean.tmax_autumn) +
#                      # ti(latitude_s, mean.tmax_winter) +
#                      ti(latitude_s, mean.tmax_spring) +
#                      offset(log(total_effort)),
#                    family = nb(),
#                    method = "REML",
#                    control = ctrl,
#                    data = df_trends_bnt0)
# )
# 
# 
# saveRDS(bnt0.cov4, here("output", "models", "trends", "bnt0.cov4.rds"))
# 
# system.time(
#   bnt0.cov4<- gam(total_catch ~
#                     s(year_s, k = 15) +
#                     s(year_s, reach_id, bs = "fs", k = 5) +
#                     s(year_f, bs = "re")  +
#                     s(total.prcp_summer, k=5) + 
#                     s(total.prcp_autumn, k=5) + 
#                     s(total.prcp_winter, k=5) + 
#                     s(total.prcp_spring, k=5) +
#                     s(mean.tmax_summer, k=5) + 
#                     s(mean.tmax_autumn, k=5) + 
#                     s(mean.tmax_winter, k=5) + 
#                     s(mean.tmax_spring, k=5) + 
#                     s(latitude_s, k=3) +
#                     s(gradient, k=3) +
#                     s(stream_order, k=3) +
#                     ti(latitude_s, mean.tmax_summer) +
#                     # ti(latitude_s, mean.tmax_autumn) +
#                     # ti(latitude_s, mean.tmax_winter) +
#                     ti(latitude_s, mean.tmax_spring) +
#                     offset(log(total_effort)),
#                   family = nb(),
#                   method = "REML",
#                   control = ctrl,
#                   data = df_trends_bnt0)
# )
# 
# 
# saveRDS(bnt0.cov4, here("output", "models", "trends", "bnt0.cov4.rds"))
# 
# 
# # aadd order interactions too
# system.time(
#   bnt0.cov5 <- gam(total_catch ~ 
#                      s(year_s, k = 15) + 
#                      s(year_s, reach_id, bs = "fs", k = 5) +
#                      s(year_f, bs = "re") + 
#                      s(total.prcp_summer, k=5) + 
#                      s(total.prcp_autumn, k=5) + 
#                      s(total.prcp_winter, k=5) + 
#                      s(total.prcp_spring, k=5) +
#                      s(mean.tmax_summer, k=5) + 
#                      s(mean.tmax_autumn, k=5) + 
#                      s(mean.tmax_winter, k=5) + 
#                      s(mean.tmax_spring, k=5) + 
#                      s(latitude_s, k=3) + 
#                      s(gradient, k=3) + 
#                      s(stream_order, k=3) + 
#                      ti(latitude_s, mean.tmax_summer) +
#                      ti(latitude_s, mean.tmax_autumn) +
#                      ti(latitude_s, mean.tmax_winter) +
#                      ti(latitude_s, mean.tmax_spring) +
#                      ti(latitude_s, total.prcp_summer) +
#                      ti(latitude_s, total.prcp_autumn) +
#                      ti(latitude_s, total.prcp_winter) +
#                      ti(latitude_s, total.prcp_spring) +
#                      offset(log(total_effort)), 
#                    family = nb(),
#                    method = "REML",
#                    control = ctrl,
#                    data = df_trends_bkt0)
# )
# 
# saveRDS(bnt0.cov5, here("output", "models", "trends", "bnt0.cov5.rds"))

# ADULT BROWN TROUT =============================================

# statewide trends: 105 seconds
system.time(
  bnt1.m1 <- gam(total_catch ~ 
                   s(year, k=15) + 
                   s(reach_id, bs = "re") + 
                   offset(log(total_effort)), 
                 data = df_trends_bnt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt1.m1, here("output", "models", "trends", "bnt1.m1.rds"))


# stream trends: 30 minutes
system.time(
  bnt1.m2 <- gam(total_catch ~
                   s(year, reach_id, bs = "fs", k = 5) + 
                   offset(log(total_effort)),
                 data = df_trends_bnt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt1.m2, here("output", "models", "trends", "bnt1.m2.rds"))


# statewide and stream trends: 35 minutes
system.time(
  bnt1.m3 <- gam(total_catch ~
                   s(year, k=15) + 
                   s(year, reach_id, bs = "fs", k = 5) + 
                   offset(log(total_effort)),
                 data = df_trends_bnt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt1.m3, here("output", "models", "trends", "bnt1.m3.rds"))
# saveRDS(bnt1.m3, here("output", "models", "trends", "bnt1.m3_12.rds"))


# region-specific trends: 15 minutes
system.time(
  bnt1.m4 <- gam(total_catch ~ huc_names8 +
                   s(year, by = huc_names8) +
                   s(reach_id, bs = "re") +
                   offset(log(total_effort)),
                 data = df_trends_bnt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt1.m4, here("output", "models", "trends", "bnt1.m4.rds"))

# region-specific trends, plus stream trends: 67 min
system.time(
  bnt1.m5 <- gam(total_catch ~ huc_names8 +
                   s(year, by = huc_names8) +
                   s(year, reach_id, bs = "fs", k = 5) +
                   offset(log(total_effort)),
                 data = df_trends_bnt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt1.m5, here("output", "models", "trends", "bnt1.m5.rds"))


# statewide and stream trends: with y2y: 5 minutes
system.time(
  bnt1.m3.y2y <- gam(total_catch ~
                   s(year, k=15) + 
                   s(year, reach_id, bs = "fs", k = 5) + 
                   s(year_f, bs = "re") + 
                   offset(log(total_effort)),
                 data = df_trends_bnt1,
                 method = "REML",
                 family = nb(),
                 control = ctrl)
)
saveRDS(bnt1.m3.y2y, here("output", "models", "trends", "bnt1.m3.y2y.rds"))
# saveRDS(bnt1.m3.y2y, here("output", "models", "trends", "bnt1.m3.y2y_cl1.rds"))
