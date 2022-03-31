

# Prep ============================================
library(tidyverse)
library(here)
library(brms)
library(brmstools)
library(performance)
library(tidybayes)
library(ggdist)
library(patchwork)
library(ragg)

# Set conditions for plotting
conditions <- data.frame(total_effort = 1)  # set effort to counts per mile
int_conditions <- list(latitude_s=c(-1,0.5,1.5))  # set latitudes
# -1 = ~43.523
# 0.5 = ~45.09
# 1.5 = ~46.17

# Load model objects ====================================================

source(here::here("R", "50_prep_data.R"))
df_surveys <- readRDS(here("data", "df_trout_surveys_cln.rds"))

bkt.brm1 <- readRDS(here("output", "models", "brms", "bkt.brm1.rds"))
bkt.brm2 <- readRDS(here("output", "models", "brms", "bkt.brm2.rds"))
bkt.brm3 <- readRDS(here("output", "models", "brms", "bkt.brm3.rds"))
bkt.brm3.c1 <- readRDS(here("output", "models", "brms", "bkt.brm3_12.rds"))
bkt.brm4 <- readRDS(here("output", "models", "brms", "bkt.brm4.rds"))

bnt.brm1 <- readRDS(here("output", "models", "brms", "bnt.brm1.rds"))
bnt.brm2 <- readRDS(here("output", "models", "brms", "bnt.brm2.rds"))
bnt.brm3 <- readRDS(here("output", "models", "brms", "bnt.brm3.rds"))
bnt.brm3.c1 <- readRDS(here("output", "models", "brms", "bnt.brm3_12.rds"))
bnt.brm4 <- readRDS(here("output", "models", "brms", "bnt.brm4.rds"))


bkt.mod <- bkt.brm4
bnt.mod <- bnt.brm4

vars <- get_variables(bkt.mod)[c(2:11)]
vars2 <- get_variables(bnt.mod)[c(15:24)]

# Brook trout =============================================================

# bkt.brm0 <- add_criterion(bkt.brm0, "waic")
bkt.brm1 <- add_criterion(bkt.brm1, c("waic","loo"))
bkt.brm2 <- add_criterion(bkt.brm2, c("waic","loo"))
bkt.brm3 <- add_criterion(bkt.brm3, c("waic","loo"))
bkt.brm4 <- add_criterion(bkt.brm4, c("waic","loo"))
bkt.brm5 <- add_criterion(bkt.brm5, c("waic","loo"))
bkt.brm6 <- add_criterion(bkt.brm6, c("waic","loo"))

loo_compare(bkt.brm1, bkt.brm2, bkt.brm3, bkt.brm2a, bkt.brm4,
            criterion = "waic")

loo_compare(bkt.brm1, bkt.brm2, bkt.brm3, bkt.brm4, bkt.brm5, bkt.brm6,
            criterion = "loo")


# Summarize best fit
bkt.mod
r2_bayes(bkt.mod, ci = 0.95)
pp_check(bkt.mod, ndraws=100,type='dens_overlay') + xlim(0,500)
pp_check(bkt.mod, ndraws=100,type='ecdf_overlay') + xlim(0,1000)
plot(loo(bkt.mod))

# Export table
a <- summary(bkt.mod)
summary_mod1 <- rbind(
  data.frame(a$fixed), 
  data.frame(a$spec_pars), 
  data.frame(a$splines))
summary_mod1 <- select(summary_mod1, !ends_with("ESS"))

colnames(summary_mod1) <- c(
  "mean","SE", "lower bound", "upper bound", "Rhat")

summary_mod1a <- rbind(
  data.frame(a$random))
summary_mod1a <- select(summary_mod1a, !ends_with("ESS"))
colnames(summary_mod1a) <- c(
  "mean","SE", "lower bound", "upper bound", "Rhat")

summary_mod1 <- rbind(
  summary_mod1, summary_mod1a
)


summary_mod1 <- summary_mod1 %>% 
  rownames_to_column(var = "parameter")  %>%
  mutate(across(where(is.numeric), round, 2))

write.csv(summary_mod1, here("output","tables","bkt_model_summary.csv"),
          row.names = FALSE)



# Coef plot
coefplot(bkt.mod, level=0.95, grouping = "reach_id",
         r_col='blue', r_intervals=FALSE, r_alpha=0.75) +
  geom_vline(aes(xintercept=0), alpha=0.5, color='red')


# Plot effects
conditional_effects(bkt.mod, conditions = conditions)
conditional_smooths(bkt.mod)
conditional_effects(bkt.mod, conditions = conditions, 
                    effects = "mean.tmax_summer")
conditional_effects(bkt.mod, conditions = conditions, 
                    effects = "mean.tmax_summer")
conditional_effects(bkt.mod, conditions = conditions, 
                    effects = "mean.tmax_autumn")
conditional_effects(bkt.mod, conditions = conditions, 
                    effects = "mean.tmax_winter")
conditional_effects(bkt.mod, conditions = conditions, 
                    effects = "mean.tmax_spring")
conditional_effects(bkt.mod, conditions = conditions, 
                    effects = "total.prcp_summer")
conditional_effects(bkt.mod, conditions = conditions, 
                    effects = "total.prcp_autumn")
conditional_effects(bkt.mod, conditions = conditions, 
                    effects = "total.prcp_winter")
conditional_effects(bkt.mod, conditions = conditions, 
                    effects = "total.prcp_spring")

conditional_effects(bkt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "mean.tmax_summer:latitude_s")
conditional_effects(bkt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "mean.tmax_autumn:latitude_s")
conditional_effects(bkt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "mean.tmax_winter:latitude_s")
conditional_effects(bkt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "mean.tmax_spring:latitude_s")

conditional_effects(bkt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "total.prcp_summer:latitude_s")
conditional_effects(bkt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "total.prcp_autumn:latitude_s")
conditional_effects(bkt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "total.prcp_winter:latitude_s")
conditional_effects(bkt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "total.prcp_spring:latitude_s")


# Brown trout =============================================================

# Compare models
bnt.brm1 <- add_criterion(bnt.brm1, c("waic","loo"))
bnt.brm2 <- add_criterion(bnt.brm2, c("waic","loo"))
bnt.brm3 <- add_criterion(bnt.brm3, c("waic","loo"))
bnt.brm3a <- add_criterion(bnt.brm3a, c("waic","loo"))
bnt.brm4 <- add_criterion(bnt.brm4, c("waic","loo"))

loo_compare(bnt.brm1, bnt.brm2, bnt.brm3,
            criterion = "waic")

loo_compare(bnt.brm1, bnt.brm2, bnt.brm3, 
            criterion = "loo")

# Summarize best fit
bnt.mod
r2_bayes(bnt.mod, ci = 0.95)
pp_check(bnt.mod, ndraws=100,type='dens_overlay') + xlim(0,500)
pp_check(bnt.mod, ndraws=100,type='ecdf_overlay') + xlim(0,1000)
# pp_check(bnt.mod, type='intervals')
# pp_check(bnt.mod, type='error_scatter_avg')
plot(loo(bnt.mod))

# Export table
b <- summary(bnt.mod)
summary_mod2 <- rbind(
  data.frame(b$fixed), 
  data.frame(b$spec_pars), 
  data.frame(b$splines))
summary_mod2 <- select(summary_mod2, !ends_with("ESS"))

colnames(summary_mod2) <- c(
  "mean","SE", "lower bound", "upper bound", "Rhat")

summary_mod2a <- rbind(
  data.frame(b$random))
summary_mod2a <- select(summary_mod2a, !ends_with("ESS"))
colnames(summary_mod2a) <- c(
  "mean","SE", "lower bound", "upper bound", "Rhat")

summary_mod2 <- rbind(
  summary_mod2, summary_mod2a
)


summary_mod2 <- summary_mod2 %>% 
  rownames_to_column(var = "parameter")  %>%
  mutate(across(where(is.numeric), round, 2))

write.csv(summary_mod2, here("output","tables","bnt_model_summary.csv"),
          row.names = FALSE)

# Plot effects

conditional_smooths(bnt.mod)
conditional_effects(bnt.mod, conditions = conditions, effects = "year_s")
conditional_effects(bnt.mod, conditions = conditions, 
                    effects = "mean.tmax_summer")
conditional_effects(bnt.mod, conditions = conditions, 
                    effects = "mean.tmax_autumn")
conditional_effects(bnt.mod, conditions = conditions, 
                    effects = "mean.tmax_winter")
conditional_effects(bnt.mod, conditions = conditions, 
                    effects = "mean.tmax_spring")
conditional_effects(bnt.mod, conditions = conditions, 
                    effects = "total.prcp_summer")
conditional_effects(bnt.mod, conditions = conditions, 
                    effects = "total.prcp_autumn")
conditional_effects(bnt.mod, conditions = conditions, 
                    effects = "total.prcp_winter")
conditional_effects(bnt.mod, conditions = conditions, 
                    effects = "total.prcp_spring")

conditional_effects(bnt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "mean.tmax_summer:latitude_s")
conditional_effects(bnt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "mean.tmax_autumn:latitude_s")
conditional_effects(bnt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "mean.tmax_autumn:latitude_s")
conditional_effects(bnt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "mean.tmax_spring:latitude_s")

conditional_effects(bnt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "total.prcp_summer:latitude_s")
conditional_effects(bnt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "total.prcp_autumn:latitude_s")
conditional_effects(bnt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "total.prcp_winter:latitude_s")
conditional_effects(bnt.mod, conditions = conditions, 
                    int_conditions = int_conditions,
                    effects = "total.prcp_spring:latitude_s")
