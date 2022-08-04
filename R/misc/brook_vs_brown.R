

adult_compare <- df_drivers %>% 
  filter(yoy=="N") %>% 
  select(visit.fish.seq.no, species, year, cpe) %>% 
  pivot_wider(names_from = species, values_from=cpe) %>% 
  na.omit()

adult_compare %>% 
  ggplot(aes(brown_trout, brook_trout)) +
  geom_point() + 
  geom_smooth(method = "lm", formula= (adult_compare$brook_trout ~ exp(adult_compare$brown_trout)))
