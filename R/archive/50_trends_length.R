
# Trout Size Changes

# To quantify the spatial and temporal extent of body size change, 
# we estimated the average length of fish for each species in each 
# sampling location and year, which we interpret as putative biological 
# populations (henceforth referred to as populations). 

df_drivers <- readRDS(here("output","data", "df_drivers.rds"))


# Summarize length data  ===================================================


# summarize mean and maximum length for adult fish by year and 'population"
sizstr <- df_drivers %>% filter(yoy == "N") 


sizstr <- sizstr %>% 
  group_by(species, reach_id) %>% 
  mutate(nn=n()) %>% 
  ungroup() %>% 
  filter(nn>=5) %>% 
  # filter(year <=2020) %>%
  droplevels()

# ======  EDA =================================


# distributions ----

# histograms
ggplot(sizstr, aes(mean_length)) + 
  geom_histogram(color = "black") + 
  facet_grid(vars(species)) 
ggplot(sizstr, aes(log(mean_length))) + 
  geom_histogram(color = "black") + 
  facet_grid(vars(species)) 



# violin plots of both size metrics by species:
p.vio.meanl <- ggplot(sizstr, aes(species, mean_length, fill = species)) + 
  geom_violin() +
  theme(legend.position = "none")
p.vio.maxl <- ggplot(sizstr, aes(species, max_length, fill = species)) + 
  geom_violin() +
  theme(legend.position = "none")
p.vio.meanl + p.vio.maxl


# distributions for mean and max length look okay; albeit with strong right tails --- outliers?

# outliers  ----

# scatterplot for outliers
ggplot(sizstr, aes(year, mean_length)) + 
  geom_point()+ 
  facet_grid(vars(species)) 
# scatterplot for outliers
ggplot(sizstr, aes(year, max_length)) + 
  geom_point()+ 
  facet_grid(vars(species)) 


# missing data ----
colSums(is.na(sizstr))

# Remove missing data and outliers ----

sizstr <- sizstr %>% 
  filter(!is.na(mean_length)) %>% 
  # filter(! (species == "brook_trout" & max_length > 35)) %>%
  mutate(across(where(is.character), as.factor))
sizstr

#  Analysis: Before-After====================================================

# Calc mean body length by pop during baseline and current periods
sizstr_ba <- sizstr %>% 
  mutate(period = case_when(year >= 2010 ~ "current", year <= 2005 ~ "historic", TRUE ~ "middle")) %>%
  filter(period != "middle") %>% 
  group_by(species, period, WMU_NAME, WMU_CODE, huc8, huc_names, reach_id) %>% 
  summarise(mean_length = mean(mean_length), 
            max_length = mean(max_length), 
            .groups = "drop") %>% 
  pivot_wider(names_from = period, values_from = starts_with("m")) %>% 
  mutate(mean_size_change = (mean_length_current - mean_length_historic)/mean_length_historic, 
         max_size_change = (max_length_current - max_length_historic)/max_length_historic) %>% 
  filter(!is.na(mean_size_change)) %>%
  filter(!is.na(max_size_change))  %>% 
  select(species, WMU_NAME, WMU_CODE, huc8, huc_names, reach_id, mean_size_change, max_size_change) 
sizstr_ba

sizstr_ba %>% group_by(species, WMU_NAME) %>% count() %>% print(n = Inf)
sizstr_ba %>% group_by(species, huc_names) %>% count() %>% print(n = Inf)


# plot by regions
grouping <- sizstr_ba$huc_names

sizstr_ba %>% 
  ggplot(aes(mean_size_change, grouping)) + 
  geom_vline(xintercept = 0, color = "red") +
  geom_boxplot(outlier.shape = NA) +
  # geom_violin(fill = "gray80", size = 0.5, alpha = .5) +
  geom_jitter(data = sizstr_ba, aes(mean_size_change, grouping), alpha = .25, color = "black", size = 0.7, height = 0.1)+
  coord_flip() +
  scale_x_continuous(limits = c(-.75, 1.25), breaks = c(-0.5, 0, .5, 1), labels = c("-0.5%","0%","0.5%","1%")) +
  labs(y = "", x = "Size change") + 
  facet_grid(vars(species), labeller = labeller(species = labels)) + 
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    panel.border = element_rect(color = "black", fill = NA)
  )

save_plot(path = here("plots","explore","size_change_mean_panel_huc8"), width = 8, height = 5)

sizstr_ba %>% 
  ggplot(aes(max_size_change, grouping)) + 
  geom_vline(xintercept = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA) +
  # geom_violin(fill = "gray80", size = .5, alpha = .5) +
  geom_jitter(data = sizstr_ba, aes(max_size_change, grouping), alpha = .25, color = "black", size = 0.7, height = 0.1)+
  coord_flip() +
  scale_x_continuous(limits = c(-.75, 1.25), breaks = c(-0.5, 0, .5, 1), labels = c("-0.5%","0%","0.5%","1%")) +
  labs(y = "", x = "Size change") + 
  facet_grid(vars(species), labeller = labeller(species = labels)) + 
  theme_classic() + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    panel.border = element_rect(color = "black", fill = NA)
  )

save_plot(path = here("plots","explore","size_change_max_panel_huc8"), width = 8, height = 5)




# ===== Analysis: Modeling ====================================================

# split into species groups
sizstr_bkt <- sizstr %>% filter(species == "brook_trout") %>% droplevels()
sizstr_bnt <- sizstr %>% filter(species == "brown_trout") %>% droplevels()



# Brook trout mean length -----------------------------------

sizstr_bkt %>% 
  filter(species == "brook_trout") %>% 
  ggplot(aes(x=year, y=mean_length)) + 
  geom_point(size = .25) + 
  geom_smooth(method="gam", size = .5)


# GAM
m1 <- gamm4(log(mean_length) ~ s(year) + s(year, reach_id, bs="fs"),
                  data = sizstr_bkt, 
                  REML=TRUE)
summary(m1$mer)
summary(m1$gam)
appraise(m1$gam)
gam.check(m1$gam)
plot(m1$gam, pages=1)

# Gam predictions
nd.bkt1.fe = data.frame(
  year = sizstr_bkt$year,
  reach_id = levels(sizstr_bkt$reach_id)[[1]])
pred.bkt1.fe <- predict(m1$gam, newdata = nd.bkt1.fe, terms = "s(year)", se=TRUE)
p.bkt1.fe <- transform(sizstr_bkt, fit = pred.bkt1.fe$fit, se = pred.bkt1.fe$se.fit)

p.bkt1.fe %>% 
  ggplot(aes(x=year, y=mean_length)) +
  # geom_jitter(alpha=0.25) +
  geom_ribbon(aes(ymin=exp(fit-2*se),ymax=exp(fit+2*se)), alpha=0.25) +
  geom_line(aes(y=exp(fit))) + 
  labs(y = expression(atop(Predicted~body~size,(inches))), x = "Year")

# Population-specific predictions and synchrony
nd.bkt1.re = data.frame(
  year = sizstr_bkt$year,
  reach_id = sizstr_bkt$reach_id) 
pred.bkt1.re <- predict(m1$gam, newdata = nd.bkt1.re, se=TRUE)
p.bkt1.re <- transform(sizstr_bkt, fit = pred.bkt1.re$fit, se = pred.bkt1.re$se.fit)

p.bkt1.re %>% 
  ggplot(aes(x=year, y=total_catch, group=reach_id)) +
  geom_line(aes(y=exp(fit))) +  
  facet_wrap(vars(huc_names), scales = "free") +
  labs(y = expression(atop(Predicted~body~size,(inches))), x = "Year") + 
  scale_x_continuous(limits = c(1994,2021), breaks = c(2000,2010,2020))

# Brook trout max length -----------------------------------

sizstr_bkt %>% 
  filter(species == "brook_trout") %>% 
  ggplot(aes(x=year, y=max_length)) + 
  geom_point(size = .25) + 
  geom_smooth(method="gam", size = .5)


# GAM
m2 <- gamm4(log(max_length) ~ s(year) + s(year, reach_id, bs="fs"),
            data = sizstr_bkt, 
            REML=TRUE)
summary(m2$mer)
summary(m2$gam)
appraise(m2$gam)
gam.check(m2$gam)
plot(m2$gam, pages=1)

# Gam predictions
nd.bkt2.fe = data.frame(
  year = sizstr_bkt$year,
  reach_id = levels(sizstr_bkt$reach_id)[[1]])
pred.bkt2.fe <- predict(m2$gam, newdata = nd.bkt2.fe, terms = "s(year)", se=TRUE)
p.bkt2.fe <- transform(sizstr_bkt, fit = pred.bkt2.fe$fit, se = pred.bkt2.fe$se.fit)

p.bkt2.fe %>% 
  ggplot(aes(x=year, y=max_length)) +
  # geom_jitter(alpha=0.25) +
  geom_ribbon(aes(ymin=exp(fit-2*se),ymax=exp(fit+2*se)), alpha=0.25) +
  geom_line(aes(y=exp(fit))) + 
  labs(y = expression(atop(Predicted~mean~body~size,(inches))), x = "Year")

# Population-specific predictions and synchrony
nd.bkt2.re = data.frame(
  year = sizstr_bkt$year,
  reach_id = sizstr_bkt$reach_id) 
pred.bkt2.re <- predict(m1$gam, newdata = nd.bkt2.re, se=TRUE)
p.bkt2.re <- transform(sizstr_bkt, fit = pred.bkt2.re$fit, se = pred.bkt2.re$se.fit) 

p.bkt2.re %>% 
  ggplot(aes(x=year, y=total_catch, group=reach_id)) +
  geom_line(aes(y=exp(fit)), color=col.yoy) +  
  facet_wrap(vars(huc_names), scales = "free") +
  labs(y = expression(atop(Predicted~max~body~size,(inches))), x = "Year") + 
  scale_x_continuous(limits = c(1994,2021), breaks = c(2000,2010,2020))







# Brown trout mean length -----------------------------------

sizstr_bnt %>% 
  ggplot(aes(x=year, y=mean_length)) + 
  geom_point(size = .25) + 
  geom_smooth(method="gam", size = .5)


# GAM
m3 <- gamm4(log(mean_length) ~ s(year) + s(year, reach_id, bs="fs"),
            data = sizstr_bnt, 
            REML=TRUE)
summary(m3$mer)
summary(m3$gam)
appraise(m3$gam)
gam.check(m3$gam)
plot(m3$gam, pages=1)

# Gam predictions
nd.bnt1.fe = data.frame(
  year = sizstr_bnt$year,
  reach_id = levels(sizstr_bnt$reach_id)[[1]])
pred.bnt1.fe <- predict(m3$gam, newdata = nd.bnt1.fe, terms = "s(year)", se=TRUE)
p.bnt1.fe <- transform(sizstr_bnt, fit = pred.bnt1.fe$fit, se = pred.bnt1.fe$se.fit)

p.bnt1.fe %>% 
  ggplot(aes(x=year, y=mean_length)) +
  # geom_jitter(alpha=0.25) +
  geom_ribbon(aes(ymin=exp(fit-2*se),ymax=exp(fit+2*se)), alpha=0.25) +
  geom_line(aes(y=exp(fit))) + 
  labs(y = expression(atop(Predicted~body~size,(inches))), x = "Year")

# Population-specific predictions and synchrony
nd.bnt1.re = data.frame(
  year = sizstr_bnt$year,
  reach_id = sizstr_bnt$reach_id) 
pred.bnt1.re <- predict(m1$gam, newdata = nd.bnt1.re, se=TRUE)
p.bnt1.re <- transform(sizstr_bnt, fit = pred.bnt1.re$fit, se = pred.bnt1.re$se.fit)

p.bnt1.re %>% 
  ggplot(aes(x=year, y=total_catch, group=reach_id)) +
  geom_line(aes(y=exp(fit)), color=col.yoy) +  
  facet_wrap(vars(huc_names), scales = "free") +
  labs(y = expression(atop(Predicted~body~size,(inches))), x = "Year") + 
  scale_x_continuous(limits = c(1994,2021), breaks = c(2000,2010,2020))

# Brown trout max length -----------------------------------

sizstr_bnt %>% 
  ggplot(aes(x=year, y=max_length)) + 
  geom_point(size = .25) + 
  geom_smooth(method="gam", size = .5)


# GAM
m4 <- gamm4(log(max_length) ~ s(year) + s(year, reach_id, bs="fs"),
            data = sizstr_bnt, 
            REML=TRUE)
summary(m4$mer)
summary(m4$gam)
appraise(m4$gam)
gam.check(m4$gam)
plot(m4$gam, pages=1)

# Gam predictions
nd.bnt2.fe = data.frame(
  year = sizstr_bnt$year,
  reach_id = levels(sizstr_bnt$reach_id)[[1]])
pred.bnt2.fe <- predict(m4$gam, newdata = nd.bnt2.fe, terms = "s(year)", se=TRUE)
p.bnt2.fe <- transform(sizstr_bnt, fit = pred.bnt2.fe$fit, se = pred.bnt2.fe$se.fit)

p.bnt2.fe %>% 
  ggplot(aes(x=year, y=max_length)) +
  # geom_jitter(alpha=0.25) +
  geom_ribbon(aes(ymin=exp(fit-2*se),ymax=exp(fit+2*se)), alpha=0.25) +
  geom_line(aes(y=exp(fit))) + 
  labs(y = expression(atop(Predicted~mean~body~size,(inches))), x = "Year")

# Population-specific predictions and synchrony
nd.bnt2.re = data.frame(
  year = sizstr_bnt$year,
  reach_id = sizstr_bnt$reach_id) 
pred.bnt2.re <- predict(m4$gam, newdata = nd.bnt2.re, se=TRUE)
p.bnt2.re <- transform(sizstr_bnt, fit = pred.bnt2.re$fit, se = pred.bnt2.re$se.fit) 

p.bnt2.re %>% 
  ggplot(aes(x=year, y=max_length, group=reach_id)) +
  geom_line(aes(y=exp(fit)), color=col.yoy) +  
  facet_wrap(vars(huc_names), scales = "free") +
  labs(y = expression(atop(Predicted~max~body~size,(inches))), x = "Year") + 
  scale_x_continuous(limits = c(1994,2021), breaks = c(2000,2010,2020))


# Plots ============================================================


p1 <- p.bkt1.fe %>% 
  ggplot(aes(x=year, y=mean_length)) +
  # geom_jitter(alpha=0.25) +
  geom_ribbon(aes(ymin=exp(fit-2*se),ymax=exp(fit+2*se)), alpha=0.25) +
  geom_line(aes(y=exp(fit))) + 
  labs(y = expression(atop(Predicted~mean~body~size,(inches))), x = "Year", 
       title="Brook trout mean size")


p2 <- p.bkt2.fe %>% 
  ggplot(aes(x=year, y=max_length)) +
  # geom_jitter(alpha=0.25) +
  geom_ribbon(aes(ymin=exp(fit-2*se),ymax=exp(fit+2*se)), alpha=0.25) +
  geom_line(aes(y=exp(fit))) + 
  labs(y = expression(atop(Predicted~max~body~size,(inches))), x = "Year", 
       title="Brook torut max size")

p3 <- p.bnt1.fe %>% 
  ggplot(aes(x=year, y=mean_length)) +
  # geom_jitter(alpha=0.25) +
  geom_ribbon(aes(ymin=exp(fit-2*se),ymax=exp(fit+2*se)), alpha=0.25) +
  geom_line(aes(y=exp(fit))) + 
  labs(y = expression(atop(Predicted~mean~body~size,(inches))), x = "Year", 
       title="Brown trout mean size")

p4 <- p.bnt2.fe %>% 
  ggplot(aes(x=year, y=max_length)) +
  # geom_jitter(alpha=0.25) +
  geom_ribbon(aes(ymin=exp(fit-2*se),ymax=exp(fit+2*se)), alpha=0.25) +
  geom_line(aes(y=exp(fit))) + 
  labs(y = expression(atop(Predicted~max~body~size,(inches))), x = "Year", 
       title="Brown trout max size")


p1 + p2 + p3 + p4
