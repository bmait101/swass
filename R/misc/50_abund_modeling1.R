library(lme4)


df_surveys <- read_rds(here("data", "df_trout-surveys-cln.rds"))

trout_cpe <- 
  here("data","df_cpe.rds") %>%
  read_rds() %>% 
  mutate(across(c(site.seq.no,reach_id), as.character))

trout_cpe <- 
  trout_cpe %>% 
  left_join(df_surveys %>% select(survey.seq.no, primary.survey.purpose), by = "survey.seq.no")

surve <- trout_cpe %>% count(primary.survey.purpose) %>% arrange(desc(n)) %>% slice_head(n=3) %>% pull(primary.survey.purpose)

trout_cpe_trend <- 
  trout_cpe %>% 
  filter(primary.survey.purpose %in% surve)

trout_cpe_bkt <- 
  trout_cpe_trend %>%
  filter(species=="brook_trout")



# Effect of time on mean stream trout CPEs across the state ====================

# summarize data 
df <- 
  trout_cpe_bkt %>% 
  group_by(survey.year) %>% 
  summarise(cpe = mean(cpe, na.rm = TRUE)) 


# historgram of responce data (CPEs)

(p.cpe.hist <- df %>% 
  ggplot(aes(cpe)) + 
  geom_histogram() )


# Not zero inflated (only ~200 0s that we inputed) 
# stil highly left-skeyed


# plot data over time
(p.cpe <-
    df %>% 
    ggplot(aes(x = survey.year, y = cpe)) +
    geom_point() +
    geom_smooth(method = glm) +
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
    labs(x = " ", y = "CPE"))


# GLM
cpe.m <- glm(cpe ~ survey.year, family = poisson, data = df)
summary(cpe.m)


# Effect of time on mean stream trout CPEs across the state ====================

# summarize data 
df <- trout_cpe_bkt %>% 
  group_by(survey.year, gear) %>% 
  summarise(cpe = mean(cpe, na.rm = TRUE)) 


# historgram of responce data (CPEs)

df %>% 
    ggplot(aes(cpe)) + 
    geom_histogram()

df %>% 
  ggplot(aes(gear, cpe)) + 
  geom_boxplot()


# plot data over time
(p.cpe <-
    df %>% 
    ggplot(aes(x = survey.year, y = cpe, color = gear)) +
    geom_point() +
    geom_smooth(method = lm)+
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
    labs(x = " ", y = "CPE"))

cpe.lm <- lm(cpe ~ survey.year + gear, data = df)
summary(cpe.lm)


cpe.lmer <- lmer(cpe ~ survey.year + (1 | gear), data = df)
summary(cpe.lmer)
5506/(5506 + 7345)  # ~42 %

plot(cpe.lmer)
qqnorm(resid(cpe.lmer))
qqline(resid(cpe.lmer))



# =============


# summarize data 
df <- trout_cpe_bkt %>% 
  group_by(survey.year) %>% 
  summarise(cpe = mean(cpe, na.rm = TRUE)) 


plot(df$cpe ~ df$survey.year)
