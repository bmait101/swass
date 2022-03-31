

# Drivers of trout relative abundance analysis
# Bryan Maitland

# Goal: build Bayesian hierarchical models to quantify relationship between 
# change in trout population size and covariates


# libraries
library(brms)

# help Stan run faster
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Data =========================================================================

source(here::here("R", "50_prep_data.R"))

# Clean up dfs for brms anlaysis

temp.dat.bkt <- df_trends_bkt0 %>% 
  # filter(huc_names8 %in% c(
  # "Kickapoo", "Lower St. Croix", "Rush-Vermillion","Black","Coon-Yellow")) %>%
  # filter(trout_class %in% c("CLASS I","CLASS II")) %>% 
  select(
    year_s, species, total_catch, total_effort, reach_id, huc_names8,
    total.prcp_spring, total.prcp_winter,
    total.prcp_summer, total.prcp_autumn,
    mean.tmax_spring, mean.tmax_winter, mean.tmax_autumn, mean.tmax_summer,
    stream_order, gradient, latitude_s
  )

temp.dat.bnt <- df_trends_bnt0 %>% 
  # filter(huc_names8 %in% c(
  # "Kickapoo", "Lower St. Croix", "Rush-Vermillion","Black","Coon-Yellow")) %>%
  # filter(trout_class %in% c("CLASS I","CLASS II")) %>% 
  select(
    year_s, species, total_catch, total_effort, reach_id, huc_names8,
    total.prcp_spring, total.prcp_winter,
    total.prcp_summer, total.prcp_autumn,
    mean.tmax_spring, mean.tmax_winter, mean.tmax_autumn, mean.tmax_summer,
    stream_order, gradient, latitude_s
  )


# brook trout ==================================================================


# glmer with random stream intercepts: 15 min
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


# add interactions
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
saveRDS(bkt.brm2a, here("output", "models", "brms", "bkt.brm2a.rds"))


# add interactions
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


# interactions
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


# bump it up
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

system.time(
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
)


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



# bump it up
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
