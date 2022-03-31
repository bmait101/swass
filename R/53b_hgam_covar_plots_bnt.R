

# 2. YOY BROWN TROUT ===========================================================


# Summarize models -------------------------------------------------

aic.compare(bnt0.cov, bnt0.cov2, bnt0.cov3, bnt0.cov5)

summary(bnt0.cov2)
sms1 <- smooths(bnt0.cov2)
draw(bnt0.cov2, select = sms1[1], residuals=FALSE)
draw(bnt0.cov2, select = sms1[3:6], residuals=FALSE)
draw(bnt0.cov2, select = sms1[7:10], residuals=FALSE)
draw(bnt0.cov2, select = sms1[14:17], residuals=FALSE)

summary(bnt0.cov5)
sms2 <- smooths(bnt0.cov5)
draw(bnt0.cov5, select = sms2[1], residuals=FALSE)
draw(bnt0.cov5, select = sms2[4:7], residuals=TRUE)
draw(bnt0.cov5, select = sms2[8:11], residuals=TRUE)
draw(bnt0.cov5, select = sms2[12:14], residuals=TRUE)
draw(bnt0.cov5, select = sms2[15:18], residuals=FALSE)



bnt.pred.mod <- bnt0.cov3

# Summer rainfall --------------------------------------------------------------

# Derivatives
deriv.bnt.prcp.su <- derivatives(
  bnt.pred.mod, type = "central", 
  term = "s(total.prcp_summer)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

ggplot(deriv.bnt.prcp.su, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# Predictions
nd.bnt0.prcp.su <- with(df_trends_bnt0, tibble(
  total_effort = 1, year_s = 0, year_f = 2010,
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  total.prcp_summer = seq(-4,5, length.out = 200), 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0, 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0
))

nd.bnt0.prcp.su <- bind_cols(
  nd.bnt0.prcp.su,
  as_tibble(predict(bnt.pred.mod, newdata = nd.bnt0.prcp.su,
                    terms = "s(total.prcp_summer)",se.fit = TRUE)))

crit.t <- qt(0.975, df = df.residual(bnt.pred.mod))
nd.bnt0.prcp.su <- nd.bnt0.prcp.su %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

p.bnt0.prcp.su <- nd.bnt0.prcp.su %>% 
  ggplot(aes(x = total.prcp_summer, y = exp(fit))) +
  geom_ribbon(
    aes(x = total.prcp_summer,ymin = exp(lower), ymax = exp(upper)), 
    alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line() 
p.bnt0.prcp.su

# Link scale
nd.bnt0.prcp.su %>% 
  ggplot(aes(x = total.prcp_summer, y = fit)) +
  geom_ribbon(aes(x = total.prcp_summer,
                  ymin = lower, ymax = upper), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  +
  geom_point(data = df_trends_bnt0, aes(x = total.prcp_summer, y = log(cpe)))

# Autumn rainfall --------------------------------------------------------------

# Derivatives
deriv.bnt.prcp.au <- derivatives(
  bnt.pred.mod, type = "central", 
  term = "s(total.prcp_autumn)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

ggplot(deriv.bnt.prcp.au, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# Predictions
nd.bnt0.prcp.au <- with(df_trends_bnt0, tibble(
  total_effort = 1, year_s = 0, year_f = 2010,
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = seq(-4,5, length.out = 200), 
  total.prcp_winter = 0, 
  total.prcp_spring = 0, 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0
))

nd.bnt0.prcp.au <- bind_cols(
  nd.bnt0.prcp.au,
  as_tibble(predict(bnt.pred.mod, newdata = nd.bnt0.prcp.au,
                    terms = "s(total.prcp_autumn)",se.fit = TRUE)))

crit.t <- qt(0.975, df = df.residual(bnt.pred.mod))
nd.bnt0.prcp.au <- nd.bnt0.prcp.au %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

p.bnt0.prcp.au <- nd.bnt0.prcp.au %>% 
  ggplot(aes(x = total.prcp_autumn, y = exp(fit))) +
  geom_ribbon(
    aes(x = total.prcp_autumn,ymin = exp(lower), ymax = exp(upper)), 
    alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line() 
p.bnt0.prcp.au

# Link scale
nd.bnt0.prcp.au %>% 
  ggplot(aes(x = total.prcp_autumn, y = fit)) +
  geom_ribbon(aes(x = total.prcp_autumn,
                  ymin = lower, ymax = upper), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  +
  geom_point(data = df_trends_bnt0, aes(x = total.prcp_autumn, y = log(cpe)))


# Winter rainfall --------------------------------------------------------------

# Derivatives
deriv.bnt.prcp.wi <- derivatives(
  bnt.pred.mod, type = "central", 
  term = "s(total.prcp_winter)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

ggplot(deriv.bnt.prcp.wi, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# Predictions
nd.bnt0.prcp.wi <- with(df_trends_bnt0, tibble(
  total_effort = 1, year_s = 0, year_f = 2010,
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = seq(-4,5, length.out = 200), 
  total.prcp_spring = 0, 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0
))

nd.bnt0.prcp.wi <- bind_cols(
  nd.bnt0.prcp.wi,
  as_tibble(predict(bnt.pred.mod, newdata = nd.bnt0.prcp.wi,
                    terms = "s(total.prcp_winter)",se.fit = TRUE)))

crit.t <- qt(0.975, df = df.residual(bnt.pred.mod))
nd.bnt0.prcp.wi <- nd.bnt0.prcp.wi %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

p.bnt0.prcp.wi <- nd.bnt0.prcp.wi %>% 
  ggplot(aes(x = total.prcp_winter, y = exp(fit))) +
  geom_ribbon(
    aes(x = total.prcp_winter,ymin = exp(lower), ymax = exp(upper)), 
    alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line() 
p.bnt0.prcp.wi

# Link scale
nd.bnt0.prcp.wi %>% 
  ggplot(aes(x = total.prcp_winter, y = fit)) +
  geom_ribbon(aes(x = total.prcp_winter,
                  ymin = lower, ymax = upper), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  +
  geom_point(data = df_trends_bnt0, aes(x = total.prcp_winter, y = log(cpe)))


# Spring rainfall --------------------------------------------------------------

# Derivatives
deriv.bnt.prcp.sp <- derivatives(
  bnt.pred.mod, term = "s(total.prcp_spring)", 
  type = "central", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

ggplot(deriv.bnt.prcp.sp, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# Predictions
nd.bnt0.prcp.sp <- with(df_trends_bnt0, tibble(
  total_effort = 1, year_s = 0, year_f = 2010,
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = seq(-4,5, length.out = 200), 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0
))

nd.bnt0.prcp.sp <- bind_cols(
  nd.bnt0.prcp.sp,
  as_tibble(predict(bnt.pred.mod, newdata = nd.bnt0.prcp.sp,
                    terms = "s(total.prcp_spring)",se.fit = TRUE)))

crit.t <- qt(0.975, df = df.residual(bnt.pred.mod))
nd.bnt0.prcp.sp <- nd.bnt0.prcp.sp %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

p.bnt0.prcp.sp <- nd.bnt0.prcp.sp %>% 
  ggplot(aes(x = total.prcp_spring, y = exp(fit))) +
  geom_ribbon(
    aes(x = total.prcp_spring, ymin = exp(lower), ymax = exp(upper)), 
    alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line() 
p.bnt0.prcp.sp

# Link scale
nd.bnt0.prcp.sp %>% 
  ggplot(aes(x = total.prcp_spring, y = fit)) +
  geom_ribbon(aes(x = total.prcp_spring,
                  ymin = lower, ymax = upper), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  +
  geom_point(data = df_trends_bnt0, aes(x = total.prcp_spring, y = log(cpe)))


# Summer temp --------------------------------------------------------------

# Derivatives
deriv.bnt.temp.su <- derivatives(
  bnt.pred.mod, type = "central", 
  term = "s(mean.tmax_summer)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

ggplot(deriv.bnt.temp.su, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# Predictions
nd.bnt0.temp.su <- with(df_trends_bnt0, tibble(
  total_effort = 1, year_s = 0, year_f = 2010,
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0, 
  mean.tmax_summer = seq(-4,4, length.out = 200), 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0
))

nd.bnt0.temp.su <- bind_cols(
  nd.bnt0.temp.su,
  as_tibble(predict(bnt.pred.mod, newdata = nd.bnt0.temp.su,
                    terms = "s(mean.tmax_summer)",se.fit = TRUE)))

crit.t <- qt(0.975, df = df.residual(bnt.pred.mod))
nd.bnt0.temp.su <- nd.bnt0.temp.su %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

p.bnt0.temp.su <- nd.bnt0.temp.su %>% 
  ggplot(aes(x = mean.tmax_summer, y = exp(fit))) +
  geom_ribbon(
    aes(x = mean.tmax_summer,ymin = exp(lower), ymax = exp(upper)), 
    alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line() 
p.bnt0.temp.su

# Link scale
nd.bnt0.temp.su %>% 
  ggplot(aes(x = mean.tmax_summer, y = fit)) +
  geom_ribbon(aes(x = mean.tmax_summer,
                  ymin = lower, ymax = upper), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  +
  geom_point(data = df_trends_bnt0, aes(x = mean.tmax_summer, y = log(cpe)))


# Autumn temp --------------------------------------------------------------

# Derivatives
deriv.bnt.temp.au <- derivatives(
  bnt.pred.mod, type = "central", 
  term = "s(mean.tmax_autumn)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

ggplot(deriv.bnt.temp.au, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# Predictions
nd.bnt0.temp.au <- with(df_trends_bnt0, tibble(
  total_effort = 1, year_s = 0, year_f = 2010,
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0, 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = seq(-4,4, length.out = 200), 
  mean.tmax_winter = 0, 
  mean.tmax_spring = 0
))

nd.bnt0.temp.au <- bind_cols(
  nd.bnt0.temp.au,
  as_tibble(predict(bnt.pred.mod, newdata = nd.bnt0.temp.au,
                    terms = "s(mean.tmax_autumn)",se.fit = TRUE)))

crit.t <- qt(0.975, df = df.residual(bnt.pred.mod))
nd.bnt0.temp.au <- nd.bnt0.temp.au %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

p.bnt0.temp.au <- nd.bnt0.temp.au %>% 
  ggplot(aes(x = mean.tmax_autumn, y = exp(fit))) +
  geom_ribbon(
    aes(x = mean.tmax_autumn,ymin = exp(lower), ymax = exp(upper)), 
    alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line() 
p.bnt0.temp.au

# Link scale
nd.bnt0.temp.au %>% 
  ggplot(aes(x = mean.tmax_autumn, y = fit)) +
  geom_ribbon(aes(x = mean.tmax_autumn,
                  ymin = lower, ymax = upper), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  +
  geom_point(data = df_trends_bnt0, aes(x = mean.tmax_autumn, y = log(cpe)))

# Winter temp --------------------------------------------------------------

# Derivatives
deriv.bnt.temp.wi <- derivatives(
  bnt.pred.mod, type = "central", 
  term = "s(mean.tmax_winter)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

ggplot(deriv.bnt.temp.wi, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# Predictions
nd.bnt0.temp.wi <- with(df_trends_bnt0, tibble(
  total_effort = 1, year_s = 0, year_f = 2010,
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0, 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = seq(-4,4, length.out = 200), 
  mean.tmax_spring = 0
))

nd.bnt0.temp.wi <- bind_cols(
  nd.bnt0.temp.wi,
  as_tibble(predict(bnt.pred.mod, newdata = nd.bnt0.temp.wi,
                    terms = "s(mean.tmax_winter)",se.fit = TRUE)))

crit.t <- qt(0.975, df = df.residual(bnt.pred.mod))
nd.bnt0.temp.wi <- nd.bnt0.temp.wi %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

p.bnt0.temp.wi <- nd.bnt0.temp.wi %>% 
  ggplot(aes(x = mean.tmax_winter, y = exp(fit))) +
  geom_ribbon(
    aes(x = mean.tmax_winter,ymin = exp(lower), ymax = exp(upper)), 
    alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line() 
p.bnt0.temp.wi

# Link scale
nd.bnt0.temp.wi %>% 
  ggplot(aes(x = mean.tmax_winter, y = fit)) +
  geom_ribbon(aes(x = mean.tmax_winter,
                  ymin = lower, ymax = upper), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  +
  geom_point(data = df_trends_bnt0, aes(x = mean.tmax_winter, y = log(cpe)))


# Spring temp --------------------------------------------------------------

# Derivatives
deriv.bnt.temp.sp <- derivatives(
  bnt.pred.mod, term = "s(mean.tmax_spring)", 
  type = "central", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

ggplot(deriv.bnt.temp.sp, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# Predictions
nd.bnt0.temp.sp <- with(df_trends_bnt0, tibble(
  total_effort = 1, year_s = 0, year_f = 2010,
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  latitude_s = 0, gradient = 0, stream_order = 0,
  total.prcp_summer = 0, 
  total.prcp_autumn = 0, 
  total.prcp_winter = 0, 
  total.prcp_spring = 0, 
  mean.tmax_summer = 0, 
  mean.tmax_autumn = 0, 
  mean.tmax_winter = 0, 
  mean.tmax_spring = seq(-4,4, length.out = 200)
))

nd.bnt0.temp.sp <- bind_cols(
  nd.bnt0.temp.sp,
  as_tibble(predict(bnt.pred.mod, newdata = nd.bnt0.temp.sp,
                    terms = "s(mean.tmax_spring)",se.fit = TRUE)))

crit.t <- qt(0.975, df = df.residual(bnt.pred.mod))
nd.bnt0.temp.sp <- nd.bnt0.temp.sp %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

p.bnt0.temp.sp <- nd.bnt0.temp.sp %>% 
  ggplot(aes(x = mean.tmax_spring, y = exp(fit))) +
  geom_ribbon(
    aes(x = mean.tmax_spring, ymin = exp(lower), ymax = exp(upper)), 
    alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line() 
p.bnt0.temp.sp

# Link scale
nd.bnt0.temp.sp %>% 
  ggplot(aes(x = mean.tmax_spring, y = fit)) +
  geom_ribbon(aes(x = mean.tmax_spring,
                  ymin = lower, ymax = upper), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  +
  geom_point(data = df_trends_bnt0, aes(x = mean.tmax_spring, y = log(cpe)))

