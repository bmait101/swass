
# YOY-adult abundance relationships

# # Subset trend data and summarize density per stream reach
new.dat2 <-   df_drivers %>%
  filter(primary.survey.purpose=="fisheries_assessments_trout_trend") %>% 
  select(reach_id, year, visit.fish.seq.no,  species, size_class, cpe) %>% 
  group_by(reach_id, year, species, size_class) %>% 
  summarise(mean_cpe = mean(cpe, na.rm = TRUE))

# Filter for reaches with 5 yeras of consecutive data

tmp <- df_drivers %>% 
  filter(species == "brook_trout") %>% 
  select(year, site.seq.no, visit.fish.seq.no, size_class, cpe) %>% 
  pivot_wider(names_from=size_class, values_from=cpe) %>% 
  arrange(site.seq.no, year) %>% 
  group_by(site.seq.no, grp = cumsum(c(1, diff(year) != 1)))%>% 
  filter(n() >= 15) %>% 
  group_by(site.seq.no) %>% 
  mutate(adult_prev = lag(adult)) %>% 
  na.omit()


tmp %>% 
  ggplot(aes(x = adult_prev, y = yoy)) +
  geom_point()+
  facet_wrap(vars(site.seq.no), scales = "free")







tmp <- tmp %>% filter(site.seq.no == "135441") %>% 
  mutate(year_post = year - 2009)

plot <- 
  ggplot(tmp, aes(x = year, y = yoy)) +
  geom_point() +
  facet_wrap(vars(site.seq.no), scales = "free")
plot


m_exp <- glm(yoy ~ year_post, 
             family = poisson(link = "log"), data = tmp)
exp(coef(m_exp))

plot +
  geom_line(aes(y = predict(m_exp, type = "response")), color = "darkgreen")


m_process <- glm(yoy ~ 1, offset = log(adult_prev),
                 family = poisson(link = "log"), data = tmp)
exp(coef(m_process))
0.3247163 * 50


ggplot(tmp, aes(x = year, y = yoy)) +
  geom_point() +
  geom_line(aes(y = predict(m_process, type = "response")), color = "orange")

m_rick <- glm(yoy ~ adult_prev, offset = log(adult_prev),
              family = poisson, data = tmp)

ggplot(tmp, aes(x = year, y = yoy)) +
  geom_point() +
  geom_line(aes(y = predict(m_rick, type = "response")), color = "brown")

tmp <- df_drivers %>% 
  filter(species == "brook_trout") %>% 
  select(reach_id, year, visit.fish.seq.no, size_class, cpe) %>% 
  pivot_wider(names_from=size_class, values_from=cpe) %>% 
  na.omit() %>% 
  group_by(reach_id, grp = cumsum(c(1, diff(year) != 1))) %>% 
  filter(n() >= 10) %>% 
  droplevels() %>% 
  mutate(N = log(N), Y = log(Y)) %>% 
  ungroup() %>% select(-grp,-species)


new.dat2.bnt <- new.dat2 %>% filter(species == "brown_trout") %>% 
  pivot_wider(names_from=yoy, values_from=mean_cpe) %>% 
  na.omit()%>% 
  group_by(reach_id, grp = cumsum(c(1, diff(year) != 1))) %>% 
  filter(n() >= 10) %>% 
  droplevels() %>% 
  mutate(N = log(N), Y = log(Y))


lag <- new.dat2.bkt %>% select(reach_id, year, N) %>% mutate(year=year+1)
new.dat2.bkt %>% select(-N) %>% left_join(lag, by=c("reach_id", "year"))


(new.dat2.bkt %>% 
  ggplot(aes(Y, N, group=reach_id)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=FALSE, color="black", alpha=0.2, size=0.5) +
    new.dat2.bkt %>% 
  ggplot(aes(N, Y, group=reach_id)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=FALSE, color="black", alpha=0.2, size=0.5)) /

(new.dat2.bnt %>% 
  ggplot(aes(Y, N, group=reach_id)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=FALSE, color="black", alpha=0.2, size=0.5) +
  new.dat2.bnt %>% 
  ggplot(aes(N, Y, group=reach_id)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=FALSE, color="black", alpha=0.2, size=0.5) )


m1_bkt <- lmer(N ~ Y + (1 + Y | reach_id), data = new.dat2.bkt)
m2_bkt <- lmer(Y ~ N + (1 + N | reach_id), data = new.dat2.bkt)
m1_bnt <- lmer(N ~ Y + (1 + Y | reach_id), data = new.dat2.bnt)
m2_bnt <- lmer(Y ~ N + (1 + N | reach_id), data = new.dat2.bnt)

ggpredict(m1_bkt, terms = "Y") %>% plot()
ggpredict(m2_bkt, terms = "N") %>% plot()
ggpredict(m1_bnt, terms = "Y") %>% plot()
ggpredict(m2_bnt, terms = "N") %>% plot()

summary(m1_bkt); performance::r2(m1_bkt)
summary(m2_bkt); performance::r2(m2_bkt)
summary(m1_bnt); performance::r2(m1_bnt)
summary(m2_bnt); performance::r2(m2_bnt)




# Brookies -------------


new.dat2.bkt.nest <- new.dat2.bkt %>% 
  group_by(reach_id) %>% 
  nest() 
regressions <- new.dat2.bkt.nest %>% 
  mutate(fit_yoy = map(data, ~lm(N ~ Y, data = .x)), 
         fit_ad = map(data, ~lm(Y ~ N, data = .x)))

tmp <- regressions %>% 
  mutate(glance_yoy=map(fit_yoy, glance), 
         glance_ad=map(fit_ad, glance)) 
tmp %>% unnest(glance_yoy)
tmp %>% unnest(glance_ad)

regressions %>% 
  mutate(glance=map(fit_ad, glance)) %>% 
  unnest(glance) %>% 
  select(reach_id, r.squared, p.value)

spp_model_summaries <- 
  regressions %>% 
      unnest(glance, .drop = TRUE) %>% 
  select(reach_id, r.squared, p.value)
spp_model_summaries %>%
  ggplot(aes(reach_id, p.value)) +
  geom_point() +
  geom_point(data = spp_model_summaries %>% filter(p.value<=.1), aes(reach_id, p.value), color="red", size=2) + 
  coord_flip() +
  scale_y_continuous(breaks = seq(0,1,.1))

spp_model_summaries %>% ungroup() %>% filter(p.value<=.1) %>% count() / spp_model_summaries %>% ungroup() %>%  count()


regressionsN <- new.dat2.bkt.nest %>% 
  mutate(lm = map(data, ~lm(Y ~ N, data = .x))) %>% 
  mutate(tidy = map(lm, tidy), 
         glance = map(lm, glance), 
         aug = map(lm, augment)
  )




# Browns ---------------

new.dat2.bnt %>% 
  ggplot(aes(Y, N)) + 
  geom_point() + 
  geom_smooth(method = "lm")

new.dat2.bnt %>% 
  ggplot(aes(Y, N, group=reach_id)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=FALSE, color="black", alpha=0.2, size=0.5) 


m1_bnt <- lmer(log(Y) ~ log(N) + (1 + log(N) | reach_id), data = new.dat2.bnt)
coef(m1_bkt)
summary(m1_bnt)
plot(m1_bnt)
MuMIn::r.squaredGLMM(m1_bnt)  # much better than only varying intercept model
sjPlot::plot_model(m1_bnt, type = "re")

m2_bnt <- lmer(log(N) ~ log(Y) + (1 + log(Y) | reach_id), data = new.dat2.bnt)
summary(m2_bnt)
plot(m2_bnt)
MuMIn::r.squaredGLMM(m2_bnt)  # much better than only varying intercept model


new.dat2.bnt.nest <- new.dat2.bnt %>% 
  group_by(reach_id) %>% 
  nest() 
regressions2 <- new.dat2.bnt.nest %>% 
  mutate(lm = map(data, ~lm(N ~ Y, data = .x))) %>% 
  mutate(tidy = map(lm, tidy), 
         glance = map(lm, glance), 
         aug = map(lm, augment)
  )

spp_model_summaries2 <- 
  regressions2 %>% 
  unnest(glance, .drop = TRUE) %>% 
  select(reach_id, r.squared, p.value, df.residual, nobs) %>% 
  print(n= Inf)
spp_model_summaries2 %>%
  ggplot(aes(reach_id, p.value)) +
  geom_point() +
  geom_point(data = spp_model_summaries2 %>% filter(p.value<=.1), aes(reach_id, p.value), color="red", size=2) + 
  coord_flip() +
  scale_y_continuous(breaks = seq(0,1,.1))

spp_model_summaries2 %>% ungroup() %>% filter(p.value<=.1) %>% count() / spp_model_summaries2 %>% ungroup() %>%  count()




# Plots -----------------

# Global
p.bkt.1 <- new.dat2.bkt %>% 
  ggplot(aes(log(Y), log(N))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(x="log(YOY abundance at time t)", y="log(adult abundance at time t+1")
p.bkt.2 <- new.dat2.bkt %>% 
  ggplot(aes(log(N), log(Y))) + 
  geom_point() + 
  geom_smooth(method = "lm")

p.bnt.1 <- new.dat2.bnt %>% 
  ggplot(aes(log(Y), log(N))) + 
  geom_point() + 
  geom_smooth(method = "lm")+ 
  labs(x="log(YOY abundance at time t)", y="log(adult abundance at time t+1")
p.bnt.2 <- new.dat2.bnt %>% 
  ggplot(aes(log(N), log(Y))) + 
  geom_point() + 
  geom_smooth(method = "lm")

(p.bkt.1) +
  (p.bnt.1) + 
  plot_layout(nrow=1)

ggsave(here("output","figs","yoy_adult_relationship.png"), device=agg_png, res=300, height = 6, width = 14)


# Reaches
p.bkt.1 <- new.dat2.bkt %>% 
  ggplot(aes(log(Y), log(N), group=reach_id)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=FALSE, color="black", alpha=0.2, size=0.5) + 
  guides(color="none")
p.bkt.2 <- new.dat2.bkt %>% 
  ggplot(aes(log(N), log(Y), group=reach_id)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=FALSE, color="black", alpha=0.2, size=0.5) + 
  guides(color="none")

p.bnt.1 <- new.dat2.bnt %>% 
  ggplot(aes(log(Y), log(N),group=reach_id)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=FALSE, color="black", alpha=0.2, size=0.5) + 
  guides(color="none")
p.bnt.2 <- new.dat2.bnt %>% 
  ggplot(aes(log(N), log(Y),group=reach_id)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se=FALSE, color="black", alpha=0.2, size=0.5) + 
  guides(color="none")

(p.bkt.1 + p.bkt.2) / 
  (p.bnt.1 + p.bnt.2) + 
  plot_layout(nrow=2)

save_plot(path = here("output","figs","yoy-adult-reaches"), height = 6, width = 8)
