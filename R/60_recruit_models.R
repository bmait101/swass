#===========================================#
# Drivers of trout relative abundance
# Dr. Bryan M. Maitland
# 2022-Aug-03

# Goal: build Bayesian hierarchical models to quantify relationship between 
# change in trout population size and covariates
#===========================================#

## Set up ----

# libraries
library(tidyverse)
library(brms)

# global options
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Data ----

source(here::here("R", "30_prep_final.R"))

## Clean up for brms analysis

temp.dat.bkt <- df_analysis_bkt0 |> 
  # filter(huc_names8 %in% c(
  # "Kickapoo", "Lower St. Croix", "Rush-Vermillion","Black","Coon-Yellow")) %>%
  select(
    year_s, species, total_catch, total_effort, reach_id, huc_names8,
    total.prcp_spring, total.prcp_winter,
    total.prcp_summer, total.prcp_autumn,
    mean.tmax_spring, mean.tmax_winter, mean.tmax_autumn, mean.tmax_summer,
    stream_order, gradient, latitude_s, 
    lt_mean.daily.prcp, lt_mean.daily.tmean
  )

temp.dat.bnt <- df_analysis_bnt0 |> 
  # filter(huc_names8 %in% c(
  # "Kickapoo", "Lower St. Croix", "Rush-Vermillion","Black","Coon-Yellow")) %>%
  select(
    year_s, species, total_catch, total_effort, reach_id, huc_names8,
    total.prcp_spring, total.prcp_winter,
    total.prcp_summer, total.prcp_autumn,
    mean.tmax_spring, mean.tmax_winter, mean.tmax_autumn, mean.tmax_summer,
    stream_order, gradient, latitude_s, 
    lt_mean.daily.prcp, lt_mean.daily.tmean
  )


# brook trout ==================================================================

# glmer with random stream intercepts / slopes: 15 min 
bkt.brm1 <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer + (0 + mean.tmax_summer | reach_id) +
                  mean.tmax_autumn + (0 + mean.tmax_autumn | reach_id) +
                  mean.tmax_winter + (0 + mean.tmax_winter | reach_id) +
                  mean.tmax_spring + (0 + mean.tmax_spring | reach_id) +
                  I(mean.tmax_spring^2) + (0 + I(mean.tmax_spring^2) | reach_id) +
                  total.prcp_summer + (0 + total.prcp_summer | reach_id) +
                  total.prcp_autumn + (0 + total.prcp_autumn | reach_id) +
                  total.prcp_winter + (0 + total.prcp_winter | reach_id) +
                  total.prcp_spring + (0 + total.prcp_spring | reach_id) +
                  I(total.prcp_spring^2) + (0 + total.prcp_summer | reach_id) +
                  
                  stream_order + gradient + latitude_s + 
                  s(year_s),
                
           iter = 1000, warmup = 500, 
           chains = 2, thin = 1,
           data   = temp.dat.bkt,
           family = negbinomial(),
           control = list(adapt_delta = 0.99)
)
saveRDS(bkt.brm1, here("output", "models", "brms", "bkt.brm1.rds"))


# no random slopes
bkt.brm2 <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer +
                  mean.tmax_autumn +
                  mean.tmax_winter + 
                  mean.tmax_spring + I(mean.tmax_spring^2) +
                  total.prcp_summer +
                  total.prcp_autumn + 
                  total.prcp_winter +
                  total.prcp_spring + I(total.prcp_spring^2) +
                  (1 | reach_id) +
                  
                  stream_order + gradient + latitude_s + 
                  s(year_s),
                iter = 1000, warmup = 500, 
                chains = 2, thin = 1,
                data   = temp.dat.bkt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)
saveRDS(bkt.brm2, here("output", "models", "brms", "bkt.brm2.rds"))


# no random slopes and no quadratics
bkt.brm2a <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer +
                  mean.tmax_autumn +
                  mean.tmax_winter + 
                  s(mean.tmax_spring) + 
                  total.prcp_summer +
                  total.prcp_autumn + 
                  total.prcp_winter +
                  s(total.prcp_spring) +
                  (1 | reach_id) +
                  
                  stream_order + gradient + latitude_s + 
                  s(year_s),
                iter = 1000, warmup = 500, 
                chains = 2, thin = 1,
                data   = temp.dat.bkt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)
saveRDS(bkt.brm2a, here("output", "models", "brms", "bkt.brm2.rds"))

# no random slopes and additional vars
bkt.brm2b <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer +
                  mean.tmax_autumn +
                  mean.tmax_winter + 
                  mean.tmax_spring + I(mean.tmax_spring^2) +
                  total.prcp_summer +
                  total.prcp_autumn + 
                  total.prcp_winter +
                  total.prcp_spring + I(total.prcp_spring^2) +
                  (1 | reach_id) +
                  
                  stream_order + gradient + latitude_s + 
                  lt_mean.daily.prcp + lt_mean.daily.tmean +
                  s(year_s),
                iter = 1000, warmup = 500, 
                chains = 2, thin = 1,
                data   = temp.dat.bkt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)
saveRDS(bkt.brm2b, here("output", "models", "brms", "bkt.brm2b.rds"))


# add interactions
bkt.brm3 <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer + 
                  mean.tmax_autumn +
                  mean.tmax_winter + 
                  mean.tmax_spring + I(mean.tmax_spring^2) +
                  total.prcp_summer +
                  total.prcp_autumn + 
                  total.prcp_winter +
                  total.prcp_spring + I(total.prcp_spring^2) +
                  stream_order + gradient + latitude_s + 
                  
                  mean.tmax_summer:latitude_s + 
                  mean.tmax_autumn:latitude_s + 
                  mean.tmax_winter:latitude_s +
                  (mean.tmax_spring + I(mean.tmax_spring^2)):latitude_s +
                  total.prcp_summer:latitude_s +
                  total.prcp_autumn:latitude_s + 
                  total.prcp_winter:latitude_s + 
                  (total.prcp_spring + I(total.prcp_spring^2)):latitude_s +
                  
                  (1 | reach_id) + 
                  s(year_s),
                iter = 1000, warmup = 500, 
                chains = 2, thin = 1,
                data   = temp.dat.bkt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)

saveRDS(bkt.brm3, here("output", "models", "brms", "bkt.brm3.rds"))


# add interactions and long-term averages
bkt.brm3a <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer + 
                  mean.tmax_autumn +
                  mean.tmax_winter + 
                  mean.tmax_spring + I(mean.tmax_spring^2) +
                  total.prcp_summer +
                  total.prcp_autumn + 
                  total.prcp_winter +
                  total.prcp_spring + I(total.prcp_spring^2) +
                  stream_order + gradient + latitude_s + 
                  lt_mean.daily.prcp + lt_mean.daily.tmean +
                  
                  mean.tmax_summer:lt_mean.daily.tmean + 
                  mean.tmax_autumn:lt_mean.daily.tmean + 
                  mean.tmax_winter:lt_mean.daily.tmean +
                  (mean.tmax_spring + I(mean.tmax_spring^2)):lt_mean.daily.tmean +
                  total.prcp_summer:lt_mean.daily.tmean +
                  total.prcp_autumn:lt_mean.daily.tmean + 
                  total.prcp_winter:lt_mean.daily.tmean + 
                  (total.prcp_spring + I(total.prcp_spring^2)):lt_mean.daily.tmean +
                  
                  (1 | reach_id) + 
                  s(year_s),
                iter = 1000, warmup = 500, 
                chains = 2, thin = 1,
                data   = temp.dat.bkt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)

saveRDS(bkt.brm3a, here("output", "models", "brms", "bkt.brm3a.rds"))


# full model run
bkt.brm4 <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer + 
                  mean.tmax_autumn +
                  mean.tmax_winter + 
                  mean.tmax_spring + I(mean.tmax_spring^2) +
                  total.prcp_summer +
                  total.prcp_autumn + 
                  total.prcp_winter +
                  total.prcp_spring + I(total.prcp_spring^2) +
                  stream_order + gradient + latitude_s + 
                  
                  mean.tmax_summer:latitude_s + 
                  mean.tmax_autumn:latitude_s + 
                  mean.tmax_winter:latitude_s +
                  (mean.tmax_spring + I(mean.tmax_spring^2)):latitude_s +
                  total.prcp_summer:latitude_s +
                  total.prcp_autumn:latitude_s + 
                  total.prcp_winter:latitude_s + 
                  (total.prcp_spring + I(total.prcp_spring^2)):latitude_s +
                  
                  (1 | reach_id) + 
                  s(year_s),
                iter = 20000, warmup = 10000, 
                chains = 4, thin = 10,
                prior=c(
                  prior(student_t(3, 3, 2.5), class=Intercept),
                  prior(normal(0,1), class=b),
                  prior(student_t(3,0,10), class=sd),
                  prior(student_t(3,0,10), class=sds),
                  prior(gamma(0.01,0.01), class="shape")
                ),
                sample_prior='yes',
                data   = temp.dat.bkt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)

saveRDS(bkt.brm4, here("output", "models", "brms", "bkt.brm4.rds"))



# brown trout --------------------------------------------------------

# glmer with random stream intercepts / slopes: 15 min 
bnt.brm0 <- brm(total_catch | rate(total_effort) ~  
                    total.prcp_summer + (0 + total.prcp_summer | reach_id) +
                    total.prcp_autumn + (0 + total.prcp_autumn | reach_id) +
                    total.prcp_winter + (0 + total.prcp_winter | reach_id) + 
                    total.prcp_spring + (0 + total.prcp_spring | reach_id) + 
                    mean.tmax_summer + (0 + mean.tmax_summer | reach_id) +
                    mean.tmax_autumn + (0 + mean.tmax_autumn | reach_id) +
                    mean.tmax_winter + (0 + mean.tmax_winter | reach_id) +
                    mean.tmax_spring + (0 + mean.tmax_spring | reach_id) +
                    stream_order + gradient + latitude_s + 
                    s(year_s),
                  iter = 500, warmup = 250, 
                  chains = 2, thin = 1,
                  data   = temp.dat.bnt,
                  family = negbinomial()
  )

# no random slopes
bnt.brm1 <- brm(total_catch | rate(total_effort) ~
                  mean.tmax_summer +
                  mean.tmax_autumn +
                  mean.tmax_winter +
                  mean.tmax_spring +
                  total.prcp_summer +
                  total.prcp_autumn +
                  total.prcp_winter +
                  total.prcp_spring +
                  stream_order + gradient + latitude_s +
                  (1 | reach_id) +
                  s(year_s),
                iter = 1000, warmup = 500,
                chains = 2, thin = 1,
                data   = temp.dat.bnt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)
saveRDS(bnt.brm1, here("output", "models", "brms", "bnt.brm1.rds"))

# quadratics
bnt.brm2 <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer + I(mean.tmax_summer^2) +
                  mean.tmax_autumn +
                  mean.tmax_winter + 
                  mean.tmax_spring + I(mean.tmax_spring^2) +
                  total.prcp_summer +
                  total.prcp_autumn + 
                  total.prcp_winter +
                  total.prcp_spring + I(total.prcp_spring^2) +
                  stream_order + gradient + latitude_s + 
                  (1 | reach_id) + 
                  s(year_s),
                iter = 1000, warmup = 500,
                chains = 2, thin = 1,
                data   = temp.dat.bnt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)
saveRDS(bnt.brm2, here("output", "models", "brms", "bnt.brm2.rds"))


# add interactions
bnt.brm3 <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer +
                  mean.tmax_autumn +
                  mean.tmax_winter + 
                  mean.tmax_spring + I(mean.tmax_spring^2) +
                  total.prcp_summer +
                  total.prcp_autumn + 
                  total.prcp_winter +
                  total.prcp_spring + I(total.prcp_spring^2) +
                  stream_order + gradient + latitude_s + 
                  
                  mean.tmax_summer:latitude_s + 
                  mean.tmax_autumn:latitude_s + 
                  mean.tmax_winter:latitude_s +
                  (mean.tmax_spring + I(mean.tmax_spring^2)):latitude_s +
                  total.prcp_summer:latitude_s +
                  total.prcp_autumn:latitude_s + 
                  total.prcp_winter:latitude_s + 
                  (total.prcp_spring + I(total.prcp_spring^2)):latitude_s +

                  (1 | reach_id) + 
                  s(year_s),
                iter = 1000, warmup = 500, 
                chains = 2, thin = 1,
                prior=c(
                  prior(student_t(3, 3, 2.5), class=Intercept),
                  prior(normal(0,1), class=b),
                  prior(student_t(3,0,10), class=sd),
                  prior(student_t(3,0,10), class=sds),
                  prior(gamma(0.01,0.01), class="shape")
                ),
                sample_prior='yes',
                data   = temp.dat.bnt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)
saveRDS(bnt.brm3, here("output", "models", "brms", "bnt.brm3.rds"))


# add interactions and long-term averages
bnt.brm3a <- brm(total_catch | rate(total_effort) ~  
                   mean.tmax_summer + 
                   mean.tmax_autumn +
                   mean.tmax_winter + 
                   mean.tmax_spring + I(mean.tmax_spring^2) +
                   total.prcp_summer +
                   total.prcp_autumn + 
                   total.prcp_winter +
                   total.prcp_spring + I(total.prcp_spring^2) +
                   stream_order + gradient + latitude_s + 
                   lt_mean.daily.prcp + lt_mean.daily.tmean +
                   
                   mean.tmax_summer:latitude_s + 
                   mean.tmax_autumn:latitude_s + 
                   mean.tmax_winter:latitude_s +
                   (mean.tmax_spring + I(mean.tmax_spring^2)):latitude_s +
                   total.prcp_summer:latitude_s +
                   total.prcp_autumn:latitude_s + 
                   total.prcp_winter:latitude_s + 
                   (total.prcp_spring + I(total.prcp_spring^2)):latitude_s +
                   
                   (1 | reach_id) + 
                   s(year_s),
                 iter = 1000, warmup = 500, 
                 chains = 2, thin = 1,
                 data   = temp.dat.bnt,
                 family = negbinomial(),
                 control = list(adapt_delta = 0.99)
)

saveRDS(bnt.brm3a, here("output", "models", "brms", "bnt.brm3a.rds"))


# add interactions and long-term averages
bnt.brm3a <- brm(total_catch | rate(total_effort) ~  
                   mean.tmax_summer + 
                   mean.tmax_autumn +
                   mean.tmax_winter + 
                   mean.tmax_spring + I(mean.tmax_spring^2) +
                   total.prcp_summer +
                   total.prcp_autumn + 
                   total.prcp_winter +
                   total.prcp_spring + I(total.prcp_spring^2) +
                   stream_order + gradient + latitude_s + 
                   lt_mean.daily.prcp + lt_mean.daily.tmean +
                   
                   mean.tmax_summer:lt_mean.daily.tmean + 
                   mean.tmax_autumn:lt_mean.daily.tmean + 
                   mean.tmax_winter:lt_mean.daily.tmean +
                   (mean.tmax_spring + I(mean.tmax_spring^2)):lt_mean.daily.tmean +
                   total.prcp_summer:lt_mean.daily.tmean +
                   total.prcp_autumn:lt_mean.daily.tmean + 
                   total.prcp_winter:lt_mean.daily.tmean + 
                   (total.prcp_spring + I(total.prcp_spring^2)):lt_mean.daily.tmean +
                   
                   (1 | reach_id) + 
                   s(year_s),
                 iter = 1000, warmup = 500, 
                 chains = 2, thin = 1,
                 data   = temp.dat.bnt,
                 family = negbinomial(),
                 control = list(adapt_delta = 0.99)
)

saveRDS(bnt.brm3a, here("output", "models", "brms", "bnt.brm3a.rds"))


# full model
bnt.brm4 <- brm(total_catch | rate(total_effort) ~  
                  mean.tmax_summer + 
                  mean.tmax_autumn +
                  mean.tmax_winter + 
                  mean.tmax_spring + I(mean.tmax_spring^2) +
                  total.prcp_summer +
                  total.prcp_autumn + 
                  total.prcp_winter +
                  total.prcp_spring + I(total.prcp_spring^2) +
                  stream_order + gradient + latitude_s + 
                  
                  mean.tmax_summer:latitude_s + 
                  mean.tmax_autumn:latitude_s + 
                  mean.tmax_winter:latitude_s +
                  (mean.tmax_spring + I(mean.tmax_spring^2)):latitude_s +
                  total.prcp_summer:latitude_s +
                  total.prcp_autumn:latitude_s + 
                  total.prcp_winter:latitude_s + 
                  (total.prcp_spring + I(total.prcp_spring^2)):latitude_s +
                  
                  (1 | reach_id) + 
                  s(year_s),
                iter = 20000, warmup = 10000, 
                chains = 4, thin = 10,
                data   = temp.dat.bnt,
                family = negbinomial(),
                control = list(adapt_delta = 0.99)
)
saveRDS(bnt.brm4, here("output", "models", "brms", "bnt.brm4.rds"))


# ================== END analysis ==============================================
