

# Drivers of trout relative abundance analysis
# Bryan Maitland

# Goal: build Bayesian hierarchical models to quantify relationship between 
# change in trout population size and covariates


# Preliminaries
library(tidyverse)
library(here)
library(brms)

# help Stan run faster
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# # Define workflow paths ======================================================

figs.dir <- here("output","figs","drivers_analysis")
data.dir <- here("output","data")
model.dir <- here("output","models")


# Data ========================================================================

df_drivers <- readRDS(here(data.dir, "df_drivers.rds"))


# Data prep ===================================================================

# Calculate number of yr-observation for each stream reach
yr_obs <- df_drivers %>%
  distinct(year, reach_id, species, yoy) %>%
  group_by(reach_id, species, yoy) %>%
  summarise(yr_obs = n()) %>%
  ungroup()

# add yr_obs to data
df_drivers <- df_drivers %>%
  left_join(yr_obs, by = c("reach_id", "species", "yoy")) %>%
  relocate(yr_obs, .before=year)

# rename and add factors
df_drivers <- df_drivers %>%
  rename(size_class = yoy) %>% 
  mutate(size_class = if_else(size_class=="N","adult","yoy")) %>% 
  mutate(across(where(is.character), as.factor))

# List of survey purposes to keep for analysis
targs.survey.purpose.cpe <- c(
  "fisheries_assessments_trout_trend",
  "fisheries_assessments_trout_rotation",
  "fisheries_assessments_trout_potential",
  "fisheries_assessments_rivers",
  "baseline_monitoring",
  "comprehensive_survey",
  "general_survey",
  "natural_community_reference",
  "targeted_watershed_assessment",
  "watershed_comprehensive_sites",
  "watershed_long_term_reference_site_monitoring"
)

# Filter data
new.dat <- df_drivers %>%
  filter(! (year == 2020 & gear == "stream_shocker")) %>% 
  filter(primary.survey.purpose %in% targs.survey.purpose.cpe)  %>%
  filter(!is.na(trout_class)) %>% 
  filter(yr_obs >= 5) %>% 
  filter(size_class=="yoy") 

# Remove NAs
# map(new.dat, ~sum(is.na(.)))
new.dat <- new.dat %>% na.omit(total.prcp_winter)

# cound observations by species
new.dat %>% group_by(species) %>% count()

# # check plots
# new.dat %>% 
#   # filter(huc_names8 %in% c("Kickapoo", "Lower St. Croix", "Rush-Vermillion","Black","Coon-Yellow")) %>% 
#   ggplot(aes(x = total.prcp_winter, y = log(cpe))) + 
#   geom_smooth(method = "gam",formula = y~s(x, bs="tp",k=4)) + 
#   geom_point(size=1, color="steelblue", alpha=.5) + facet_wrap(vars(species), scales = 'free')


# Final wrangling
new.dat <- new.dat %>% 
  filter(huc_names8 %in% c("Kickapoo", "Lower St. Croix", "Rush-Vermillion","Black","Coon-Yellow")) %>%
  select(
    year, species, total_catch, total_effort, reach_id, huc_names8,
    total.prcp_spring, total.prcp_winter,
    total.prcp_summer, total.prcp_autumn,
    mean.tmax_spring, mean.tmax_winter, mean.tmax_autumn, mean.tmax_summer,
    stream_order, gradient, latitude_s
  )

# Define Species
species.list <- unique(new.dat$species)
species.list
n.species <- length(species.list)
n.species


# Climate analysis ============================================================

start.time <- date()

for(s in 1:n.species) {
  
  temp.dat <- new.dat %>% filter(species==species.list[s])
  
  brm <- brm(total_catch | rate(total_effort) ~  
               total.prcp_summer + (0 + total.prcp_summer | reach_id) +
               total.prcp_autumn + (0 + total.prcp_autumn | reach_id) +
               total.prcp_winter + (0 + total.prcp_winter | reach_id) + 
               total.prcp_spring + (0 + total.prcp_spring | reach_id) + 
               mean.tmax_summer + (0 + mean.tmax_summer | reach_id) +
               mean.tmax_autumn + (0 + mean.tmax_autumn | reach_id) +
               mean.tmax_winter + (0 + mean.tmax_winter | reach_id) +
               mean.tmax_spring + (0 + mean.tmax_spring | reach_id) +
               stream_order + gradient + latitude_s + 
               s(year),
             
             iter = 200, 
             warmup = 100, 
             chains = 2, 
             thin = 1,
             data   = temp.dat,
             family = negbinomial(), 
             prior=c(
               prior(student_t(3,0,10), class=Intercept),
               prior(normal(0,1), class=b),
               prior(student_t(3,0,10), class=sd),
               prior(student_t(3,0,10), class=sds),
               prior(gamma(0.01,0.01), class="shape")
             ),
             sample_prior='yes',
             control = list(adapt_delta = 0.99)
  )
    
  saveRDS(brm, file=file.path(model.dir, paste0(species.list[s], "_hbmV3_5yr.rds")))

} # next species


end.time <- date()

print(paste("START:",start.time))
print(paste("END:",end.time))

# ================== END analysis ==============================================

model.dir <- here("output","models")
species.list <- unique(new.dat$species)
species.list
n.species <- length(species.list)
n.species

name.model.out <- "hbmV3_5yr"

# load models
hbm.out <- vector("list", length=n.species)
for(s in 1:n.species) {
  print(paste("Loaded:", species.list[s]))
  temp.name <- paste0(species.list[s], "_", name.model.out, ".rds")
  hbm.out[[s]] <- readRDS(file.path(model.dir, temp.name))
}


hbm.out[[1]]
hbm.out[[2]]
