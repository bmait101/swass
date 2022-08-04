# Analysis of trout population data from WDNR
# Bryan M Maitland
# 18 Jan 2022


# Set up =======================================================================

library(tidyverse)
library(gratia)
# library(ragg)
library(patchwork)

aic.compare <- function(...) 
  AIC(...) %>%
  tibble::rownames_to_column(var= "Model") %>%
  mutate(deltaAIC = AIC - min(AIC))%>%
  mutate(across(c(df, AIC, deltaAIC), round)) %>% 
  ungroup() %>% 
  arrange(deltaAIC)

# Data =========================================================================

source(here::here("R", "50_prep_data.R"))


# Load model objects ===========================================================

bkt0.m1 <- readRDS(here("output", "models", "trends", "bkt0.m1.rds"))
bkt0.m2 <- readRDS(here("output", "models", "trends", "bkt0.m2.rds"))
bkt0.m3 <- readRDS(here("output", "models", "trends", "bkt0.m3_12.rds"))
bkt0.m4 <- readRDS(here("output", "models", "trends", "bkt0.m4.rds"))
bkt0.m5 <- readRDS(here("output", "models", "trends", "bkt0.m5.rds"))
bkt0.m3.y2y <- readRDS(here("output", "models", "trends", "bkt0.m3.y2y_12.rds"))


bkt1.m1 <- readRDS(here("output", "models", "trends", "bkt1.m1.rds"))
bkt1.m2 <- readRDS(here("output", "models", "trends", "bkt1.m2.rds"))
bkt1.m3 <- readRDS(here("output", "models", "trends", "bkt1.m3_12.rds"))
bkt1.m4 <- readRDS(here("output", "models", "trends", "bkt1.m4.rds"))
bkt1.m5 <- readRDS(here("output", "models", "trends", "bkt1.m5.rds"))
bkt1.m3.y2y <- readRDS(here("output", "models", "trends", "bkt1.m3.y2y_12.rds"))

bnt0.m1 <- readRDS(here("output", "models", "trends", "bnt0.m1.rds"))
bnt0.m2 <- readRDS(here("output", "models", "trends", "bnt0.m2.rds"))
bnt0.m3 <- readRDS(here("output", "models", "trends", "bnt0.m3_12.rds"))
bnt0.m4 <- readRDS(here("output", "models", "trends", "bnt0.m4.rds"))
bnt0.m5 <- readRDS(here("output", "models", "trends", "bnt0.m5.rds"))
bnt0.m3.y2y <- readRDS(here("output", "models", "trends", "bnt0.m3.y2y.rds"))


bnt1.m1 <- readRDS(here("output", "models", "trends", "bnt1.m1.rds"))
bnt1.m2 <- readRDS(here("output", "models", "trends", "bnt1.m2.rds"))
bnt1.m3 <- readRDS(here("output", "models", "trends", "bnt1.m3.rds"))
bnt1.m4 <- readRDS(here("output", "models", "trends", "bnt1.m4.rds"))
bnt1.m5 <- readRDS(here("output", "models", "trends", "bnt1.m5.rds"))
bnt1.m3.y2y <- readRDS(here("output", "models", "trends", "bnt1.m3.y2y.rds"))
bnt1.m3 <- readRDS(here("output", "models", "trends", "bnt1.m3_cl1.rds"))


# Quick AIC model comparison for all groups =======================================

# YOY brookies
aic.compare(bkt0.m1, bkt0.m2, bkt0.m3, bkt0.m4, bkt0.m5)

# Adult brookies
aic.compare(bkt1.m1, bkt1.m2, bkt1.m3, bkt1.m4, bkt1.m5)

# YOY browns
aic.compare(bnt0.m1, bnt0.m2, bnt0.m3, bnt0.m4, bnt0.m5)

# Adult brown
aic.compare(bnt1.m1, bnt1.m2, bnt1.m3, bnt1.m4, bnt1.m5)


# 1. YOY BROOK TROUT ===========================================================
# Trend models --------------------------------------

# Compare models
aic.compare(bkt0.m1, bkt0.m2, bkt0.m3, bkt0.m4, bkt0.m5)

# Summarize best fit
summary(bkt0.m3)
appraise(bkt0.m3)
draw(bkt0.m3, select = "s(year)", residuals=FALSE)
draw(bkt0.m3, select = "s(year,reach_id)", residuals=FALSE)


# Summarize y2y model
summary(bkt0.m3.y2y)
appraise(bkt0.m3.y2y)
draw(bkt0.m3.y2y, select = "s(year)", residuals=FALSE)
draw(bkt0.m3.y2y, select = "s(year,reach_id)", residuals=FALSE)

# Plot y2y effects
y2y <- smooth_estimates(bkt0.m3.y2y, "s(year_f)")
ggplot(y2y, aes(x = year_f, y = est)) +
  geom_pointrange(aes(ymin = est - se,ymax = est + se)) +
  labs(x = NULL)

# Derivatives ----------------------

# calculate deriviatives
deriv.bkt0 <- 
  derivatives(
    bkt0.m3, type = "central", term = "s(year)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

deriv.bkt0.y2y <- 
  derivatives(
    bkt0.m3.y2y, type = "central", term = "s(year)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

# plot without y2y
ggplot(deriv.bkt0, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# plot with y2y
ggplot(deriv.bkt0.y2y, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)


# Predict ----------------------------------------------------------------------

## State-wide trends 

# New data
pred.bkt0 <- expand_grid(
  year = seq(min(df_trends_bkt0$year), max(df_trends_bkt0$year), 
             length.out = 300), 
  total_effort = 1, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]]
  )

# Predict
pred.bkt0 <- bind_cols(
  pred.bkt0,
  as_tibble(
    predict(
      bkt0.m3, 
      newdata = pred.bkt0,
      terms = "s(year)",
      se.fit = TRUE))
  )

# Add CIs
crit.t <- qt(0.975, df = df.residual(bkt0.m3))
pred.bkt0 <- pred.bkt0 %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

# Plot
p.bkt0.trend <- pred.bkt0 %>%
  ggplot(aes(x = year, y = exp(fit), group = reach_id)) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year),
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))


## State-wide with y2y

# New data
pred.bkt0.y2y <- with(df_trends_bkt0, tibble(
  year = seq(min(year), max(year), length.out = 200), 
  total_effort = 1, 
  reach_id = levels(df_trends_bkt0$reach_id)[[1]], 
  year_f = 2010
))

# Predict
pred.bkt0.y2y <- bind_cols(
  pred.bkt0.y2y,
  as_tibble(
    predict(bkt0.m3.y2y, 
            newdata = pred.bkt0.y2y,terms = "s(year)",se.fit = TRUE))
)

# Add CIs
crit.t <- qt(0.975, df = df.residual(bkt0.m3))
pred.bkt0.y2y <- pred.bkt0.y2y %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))



## Reach-level trends

# New data
pred.bkt0.re <- expand_grid(
  year = seq(min(df_trends_bkt0$year), max(df_trends_bkt0$year), 
             length.out = 50), 
  total_effort = 1, 
  reach_id = levels(df_trends_bkt0$reach_id)
)

# Predict
pred.bkt0.re <- bind_cols(
  pred.bkt0.re,
  as_tibble(
    predict(
      bkt0.m3, 
      newdata = pred.bkt0.re,
      terms = "s(year,reach_id)",
      se.fit = TRUE))
  )

# CIs
crit.t <- qt(0.975, df = df.residual(bkt0.m3))
pred.bkt0.re <- pred.bkt0.re %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

# Hucs to plot
hucs <- c("Lower Chippewa","Upper Chippewa")
hucs <- c("Lower St. Croix","Upper St. Croix")
hucs <- c("Castle Rock","Lake Dubay","Upper Wisconsin")
hucs <- c("Upper Fox","Milwaukee","Manitowoc-Sheboygan")
hucs <- c("Brule","Peshtigo","Oconto")
hucs <- c("Sugar","Pecatonica")
hucs <- c("Middle Rock","Upper Rock")
hucs <- c("Lower Wisconsin","Kickapoo","Coon-Yellow","Baraboo")
hucs <- c("Trempealeau","Black","Buffalo-Whitewater","Rush-Vermillion")
hucs <- c("Eau Claire","Red Cedar")
hucs <- c("Namekagon","Bad-Montreal","Beartrap-Nemadji")

# Plot
pred.bkt0.re %>% 
  left_join(df_trends_bkt0 %>% distinct(huc_names8, reach_id), 
            by = "reach_id") %>% 
  filter(huc_names8 %in% hucs) %>% 
  ggplot(aes(x = year, y = exp(fit), group = reach_id, 
             color = huc_names8, fill = huc_names8)) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year), 
              alpha = 0.2, inherit.aes = FALSE, fill = "grey60") +
  geom_line()  + 
  facet_wrap(~reach_id, scales = 'free_y') +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))  + 
  # coord_cartesian(ylim = c(0, 1000)) +
  geom_point(data = df_trends_bkt0 %>% filter(huc_names8 %in% hucs), 
             aes(year, cpe, color = huc_names8))



# 2. ADULT BROOK TROUT =========================================================
# Trend models --------------------------

# Compare models 
aic.compare(bkt1.m1, bkt1.m2, bkt1.m3, bkt1.m4, bkt1.m5)

# Summarize best fit
summary(bkt1.m3)
appraise(bkt1.m3)
draw(bkt1.m3, select = "s(year)", residuals=FALSE)

aic.compare(bkt1.m1, bkt1.m2, bkt1.m3, bkt1.m3.y2y)


# Summarize y2y model
summary(bkt1.m3.y2y)
appraise(bkt1.m3.y2y)
draw(bkt1.m3.y2y, select = "s(year)", residuals=FALSE)

# Plot y2y effects
y2y <- smooth_estimates(bkt1.m3.y2y, "s(year_f)")
ggplot(y2y, aes(x = year_f, y = est)) +
  geom_pointrange(aes(ymin = est - se,ymax = est + se)) +
  labs(x = NULL)


# Derivatives ----------------------

# calculate deriviatives
deriv.bkt1 <- 
  derivatives(
    bkt1.m3, type = "central", term = "s(year)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

deriv.bkt1.y2y <- 
  derivatives(
    bkt1.m3.y2y, type = "central", term = "s(year)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

# plot without y2y
ggplot(deriv.bkt1, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# plot with y2y
ggplot(deriv.bkt1.y2y, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)



# Predict -------------------------------------------------------------

## State-wide trends 

# New data
pred.bkt1 <- expand_grid(
  year = seq(min(df_trends_bkt1$year), max(df_trends_bkt1$year), 
             length.out = 300), 
  total_effort = 1, 
  reach_id = levels(df_trends_bkt1$reach_id)[[1]]
)

# Predict
pred.bkt1 <- bind_cols(
  pred.bkt1,
  as_tibble(
    predict(
      bkt1.m3, 
      newdata = pred.bkt1,
      terms = "s(year)",
      se.fit = TRUE))
)

# Add CIs
crit.t <- qt(0.975, df = df.residual(bkt1.m3))
pred.bkt1 <- pred.bkt1 %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

# Plot
p.bkt1.trend <- pred.bkt1 %>% 
  ggplot(aes(x = year, y = exp(fit), group = reach_id)) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  + 
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))



## State-wide with y2y

# New data
pred.bkt1.y2y <- with(df_trends_bkt1, tibble(
  year = seq(min(year), max(year), length.out = 200), 
  total_effort = 1, 
  reach_id = levels(df_trends_bkt1$reach_id)[[1]], 
  year_f = 2010
))

# Predict
pred.bkt1.y2y <- bind_cols(
  pred.bkt1.y2y,
  as_tibble(
    predict(
      bkt1.m3.y2y, 
      newdata = pred.bkt1.y2y,
      terms = "s(year)",
      se.fit = TRUE))
)

# Add CIs
crit.t <- qt(0.975, df = df.residual(bkt0.m3))
pred.bkt1.y2y <- pred.bkt1.y2y %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))



## Reach-level trends

# New data
pred.bkt1.re <- expand_grid(
  year = seq(min(df_trends_bkt1$year), max(df_trends_bkt1$year), 
             length.out = 50), 
  total_effort = 1, 
  reach_id = levels(df_trends_bkt1$reach_id)
)

# Predict
pred.bkt1.re <- bind_cols(
  pred.bkt1.re,
  as_tibble(
    predict(
      bkt1.m3, 
      newdata = pred.bkt1.re,
      terms = "s(year,reach_id)",
      se.fit = TRUE))
)

# CIs
crit.t <- qt(0.975, df = df.residual(bkt1.m3))
pred.bkt1.re <- pred.bkt1.re %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

# Hucs to plot
hucs <- c("Lower Chippewa","Upper Chippewa")
hucs <- c("Lower St. Croix","Upper St. Croix")
hucs <- c("Castle Rock","Lake Dubay","Upper Wisconsin")
hucs <- c("Upper Fox","Milwaukee","Manitowoc-Sheboygan")
hucs <- c("Brule","Peshtigo","Oconto")
hucs <- c("Sugar","Pecatonica")
hucs <- c("Middle Rock","Upper Rock")
hucs <- c("Lower Wisconsin","Kickapoo","Coon-Yellow","Baraboo")
hucs <- c("Trempealeau","Black","Buffalo-Whitewater","Rush-Vermillion")
hucs <- c("Eau Claire","Red Cedar")
hucs <- c("Namekagon","Bad-Montreal","Beartrap-Nemadji")

# Plot
pred.bkt1.re %>% 
  left_join(df_trends_bkt1 %>% distinct(huc_names8, reach_id), 
            by = "reach_id") %>% 
  filter(huc_names8 %in% hucs) %>% 
  ggplot(aes(x = year, y = exp(fit), group = reach_id, 
             color = huc_names8, fill = huc_names8)) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year), 
              alpha = 0.2, inherit.aes = FALSE, fill = "grey60") +
  geom_line()  + 
  facet_wrap(~reach_id, scales = 'free_y') +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))  + 
  # coord_cartesian(ylim = c(0, 1000)) +
  geom_point(data = df_trends_bkt1 %>% filter(huc_names8 %in% hucs), 
             aes(year, cpe, color = huc_names8))


## HUC8-level trends

# New data
pred.bkt1.re <- expand_grid(
  year = seq(min(df_trends_bkt1$year), max(df_trends_bkt1$year), 
             length.out = 50), 
  total_effort = 1, 
  reach_id = levels(df_trends_bkt1$reach_id)[[1]],
  huc_names8 = "Kickapoo"
)

# Predict
sms <- smooths(bkt1.m5)
pred.bkt1.re <- bind_cols(
  pred.bkt1.re,
  as_tibble(
    predict(
      bkt1.m5, 
      newdata = pred.bkt1.re,
      terms = sms[10],
      se.fit = TRUE))
)

# CIs
crit.t <- qt(0.975, df = df.residual(bkt1.m3))
pred.bkt1.re <- pred.bkt1.re %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

# Plot
pred.bkt1.re %>% 
  ggplot(aes(x = year, y = exp(fit), group = reach_id, 
             color = huc_names8, fill = huc_names8)) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year), 
              alpha = 0.2, inherit.aes = FALSE, fill = "grey60") +
  geom_line()  + 
  coord_cartesian(ylim = c(0, 2000)) +
  geom_point(data = df_trends_bkt1 %>% filter(huc_names8 %in% c("Kickapoo")), 
             aes(year, cpe, color = huc_names8))


# 3. YOY BROWN TROUT ===========================================================
# Trend models --------------------------------------

aic.compare(bnt0.m1, bnt0.m2, bnt0.m3, bnt0.m4, bnt0.m5)

summary(bnt0.m3)
draw(bnt0.m3, select = "s(year)", residuals=FALSE)
appraise(bnt0.m3)

aic.compare(bnt0.m1, bnt0.m2, bnt0.m3, bnt0.m4, bnt0.m5, bnt0.m3.y2y)

summary(bnt0.m3.y2y)
appraise(bnt0.m3.y2y)
draw(bnt0.m3.y2y, select = "s(year)", residuals=FALSE)


# Plot y2y effects
y2y <- smooth_estimates(bnt0.m3.y2y, "s(year_f)")
ggplot(y2y, aes(x = year_f, y = est)) +
  geom_pointrange(aes(ymin = est - se, ymax = est + se)) +
  labs(x = NULL)


# Derivatives -------------------

# calculate deriviatives
deriv.bnt0 <- 
  derivatives(
    bnt0.m3, type = "central", term = "s(year)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

deriv.bnt0.y2y <- 
  derivatives(
    bnt0.m3.y2y, type = "central", term = "s(year)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

# plot without y2y
ggplot(deriv.bnt0, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# plot with y2y
ggplot(deriv.bnt0.y2y, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)

# Predict -------------------------------------------------------------


## State-wide trends 

# New data
pred.bnt0 <- expand_grid(
  year = seq(min(df_trends_bnt0$year), max(df_trends_bnt0$year), 
             length.out = 300), 
  total_effort = 1, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]]
)

# Predict
pred.bnt0 <- bind_cols(
  pred.bnt0,
  as_tibble(
    predict(
      bnt0.m3, 
      newdata = pred.bnt0,
      terms = "s(year)",
      se.fit = TRUE))
)

# Add CIs
crit.t <- qt(0.975, df = df.residual(bnt0.m3))
pred.bnt0 <- pred.bnt0 %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

# Plot
p.bkt0.trend <- pred.bnt0 %>% 
  ggplot(aes(x = year, y = exp(fit), group = reach_id)) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  + 
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))



## State-wide with y2y

# New data
pred.bnt0.y2y <- with(df_trends_bnt0, tibble(
  year = seq(min(year), max(year), length.out = 200), 
  total_effort = 1, 
  reach_id = levels(df_trends_bnt0$reach_id)[[1]], 
  year_f = 2010
))

# Predict
pred.bnt0.y2y <- bind_cols(
  pred.bnt0.y2y,
  as_tibble(
    predict(bnt0.m3.y2y, newdata = pred.bnt0.y2y,terms = "s(year)",se.fit = TRUE))
)

# Add CIs
crit.t <- qt(0.975, df = df.residual(bnt0.m3))
pred.bnt0.y2y <- pred.bnt0.y2y %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))



## Reach-level trends

# New data
pred.bnt0.re <- expand_grid(
  year = seq(min(df_trends_bnt0$year), max(df_trends_bnt0$year), 
             length.out = 50), 
  total_effort = 1, 
  reach_id = levels(df_trends_bnt0$reach_id)
)

# Predict
pred.bnt0.re <- bind_cols(
  pred.bnt0.re,
  as_tibble(
    predict(
      bnt0.m3, 
      newdata = pred.bnt0.re,
      terms = "s(year,reach_id)",
      se.fit = TRUE))
)

# CIs
crit.t <- qt(0.975, df = df.residual(bnt0.m3))
pred.bnt0.re <- pred.bnt0.re %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

# Hucs to plot
hucs <- c("Lower Chippewa","Upper Chippewa")
hucs <- c("Lower St. Croix","Upper St. Croix")
hucs <- c("Castle Rock","Lake Dubay","Upper Wisconsin")
hucs <- c("Upper Fox","Milwaukee","Manitowoc-Sheboygan")
hucs <- c("Brule","Peshtigo","Oconto")
hucs <- c("Sugar","Pecatonica")
hucs <- c("Middle Rock","Upper Rock")
hucs <- c("Lower Wisconsin","Kickapoo","Coon-Yellow","Baraboo")
hucs <- c("Trempealeau","Black","Buffalo-Whitewater","Rush-Vermillion")
hucs <- c("Eau Claire","Red Cedar")
hucs <- c("Namekagon","Bad-Montreal","Beartrap-Nemadji")

# Plot
pred.bnt0.re %>% 
  left_join(df_trends_bnt0 %>% distinct(huc_names8, reach_id), 
            by = "reach_id") %>% 
  filter(huc_names8 %in% hucs) %>% 
  ggplot(aes(x = year, y = exp(fit), group = reach_id, 
             color = huc_names8, fill = huc_names8)) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year), 
              alpha = 0.2, inherit.aes = FALSE, fill = "grey60") +
  geom_line()  + 
  facet_wrap(~reach_id, scales = 'free_y') +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))  + 
  # coord_cartesian(ylim = c(0, 1000)) +
  geom_point(data = df_trends_bnt0 %>% filter(huc_names8 %in% hucs), 
             aes(year, cpe, color = huc_names8))



# 4. ADULT BROWN TROUT =========================================================
# Trend models --------------------------------------

# Compare models
aic.compare(bnt1.m1, bnt1.m2, bnt1.m3, bnt1.m4, bnt1.m5)

# Summarize best fit
summary(bnt1.m3)
appraise(bnt1.m3)
draw(bnt1.m3, select = "s(year)", residuals=FALSE)


aic.compare(bnt1.m1, bnt1.m2, bnt1.m3, bnt1.m4, bnt1.m5, bnt1.m3.y2y)

# Summarize y2y model
summary(bnt1.m3.y2y)
appraise(bnt1.m3.y2y)
draw(bnt1.m3.y2y, select = "s(year)", residuals=FALSE)

# Plot y2y effects
y2y <- smooth_estimates(bnt1.m3.y2y, "s(year_f)")
ggplot(y2y, aes(x = year_f, y = est)) +
  geom_pointrange(aes(ymin = est - se,ymax = est + se)) +
  labs(x = NULL)


# Model 5
summary(bnt1.m5)
sms <- smooths(bnt1.m5)
draw(bnt1.m5, select = sms[1:15], residuals=FALSE)
draw(bnt1.m5, select = sms[16:32], residuals=FALSE)



# Derivatives ----------------------

# calculate deriviatives
deriv.bnt1 <- 
  derivatives(
    bnt1.m3, type = "central", term = "s(year)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

deriv.bnt1.y2y <- 
  derivatives(
    bnt1.m3.y2y, type = "central", term = "s(year)", interval = "simultaneous") %>% 
  mutate(sig = case_when(0 >= lower & 0 <= upper ~ NA_real_, TRUE ~ derivative)) 

# plot without y2y
ggplot(deriv.bnt1, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))  

# plot with y2y
ggplot(deriv.bnt1.y2y, aes(x = data, y = derivative)) + 
  geom_line(size = 1, linetype="dashed") + 
  geom_line(aes(y = sig), color = "red", size = 2) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) + 
  geom_hline(yintercept = 0)+ 
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))  

# Predict --------------------

## State-wide trends 

# New data
pred.bnt1 <- expand_grid(
  year = seq(min(df_trends_bnt1$year), max(df_trends_bnt1$year), 
             length.out = 300), 
  total_effort = 1, 
  reach_id = levels(df_trends_bnt1$reach_id)[[1]]
)

# Predict
pred.bnt1 <- bind_cols(
  pred.bnt1,
  as_tibble(
    predict(
      bnt1.m3, 
      newdata = pred.bnt1,
      terms = "s(year)",
      se.fit = TRUE))
)

# Add CIs
crit.t <- qt(0.975, df = df.residual(bnt1.m3))
pred.bnt1 <- pred.bnt1 %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

# Plot
p.bkt0.trend <- pred.bnt1 %>% 
  ggplot(aes(x = year, y = exp(fit), group = reach_id)) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year), 
              alpha = 0.2, inherit.aes = FALSE, fill = "black") +
  geom_line()  + 
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))



## State-wide with y2y

# New data
pred.bnt1.y2y <- with(df_trends_bnt1, tibble(
  year = seq(min(year), max(year), length.out = 200), 
  total_effort = 1, 
  reach_id = levels(df_trends_bnt1$reach_id)[[1]], 
  year_f = 2010
))

# Predict
pred.bnt1.y2y <- bind_cols(
  pred.bnt1.y2y,
  as_tibble(
    predict(bnt1.m3.y2y, newdata = pred.bnt1.y2y,terms = "s(year)",se.fit = TRUE))
)

# Add CIs
crit.t <- qt(0.975, df = df.residual(bnt1.m3))
pred.bnt1.y2y <- pred.bnt1.y2y %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))



## Reach-level trends

# New data
pred.bnt1.re <- expand_grid(
  year = seq(min(df_trends_bnt1$year), max(df_trends_bnt1$year), 
             length.out = 50), 
  total_effort = 1, 
  reach_id = levels(df_trends_bnt1$reach_id)
)

# Predict
pred.bnt1.re <- bind_cols(
  pred.bnt1.re,
  as_tibble(
    predict(
      bnt1.m3, 
      newdata = pred.bnt1.re,
      terms = "s(year,reach_id)",
      se.fit = TRUE))
)

# CIs
crit.t <- qt(0.975, df = df.residual(bnt1.m3))
pred.bnt1.re <- pred.bnt1.re %>% 
  mutate(upper = fit + (crit.t * se.fit), lower = fit - (crit.t * se.fit))

# Hucs to plot
hucs <- c("Lower Chippewa","Upper Chippewa")
hucs <- c("Lower St. Croix","Upper St. Croix")
hucs <- c("Castle Rock","Lake Dubay","Upper Wisconsin")
hucs <- c("Upper Fox","Milwaukee","Manitowoc-Sheboygan")
hucs <- c("Brule","Peshtigo","Oconto")
hucs <- c("Sugar","Pecatonica")
hucs <- c("Middle Rock","Upper Rock")
hucs <- c("Lower Wisconsin","Kickapoo","Coon-Yellow","Baraboo")
hucs <- c("Trempealeau","Black","Buffalo-Whitewater","Rush-Vermillion")
hucs <- c("Eau Claire","Red Cedar")
hucs <- c("Namekagon","Bad-Montreal","Beartrap-Nemadji")

# Plot
pred.bnt1.re %>% 
  left_join(df_trends_bnt1 %>% distinct(huc_names8, reach_id), 
            by = "reach_id") %>% 
  filter(huc_names8 %in% hucs) %>% 
  ggplot(aes(x = year, y = exp(fit), group = reach_id, 
             color = huc_names8, fill = huc_names8)) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year), 
              alpha = 0.2, inherit.aes = FALSE, fill = "grey60") +
  geom_line()  + 
  facet_wrap(~reach_id, scales = 'free_y') +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))  + 
  # coord_cartesian(ylim = c(0, 1000)) +
  geom_point(data = df_trends_bnt1 %>% filter(huc_names8 %in% hucs), 
             aes(year, cpe, color = huc_names8))


