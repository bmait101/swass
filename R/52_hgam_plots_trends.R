
library(tidyverse)
library(png)
library(patchwork)
library(grid)
library(ggimage)
# library(ragg)
# library(showtext)
# font_add_google("News Cycle")
# showtext_auto()



p.data <- bind_rows(
  pred.bkt0 %>% mutate(group = "a", species="Brook Trout", age="YOY"),
  pred.bkt1 %>% mutate(group = "a", species="Brook Trout", age="Age-1+"), 
  pred.bnt0 %>% mutate(group = "b", species="Brown Trout", age="YOY"), 
  pred.bnt1 %>% mutate(group = "b", species="Brown Trout", age="Age-1+")
) %>% 
  mutate(year_f = 2010) %>% 
  relocate(year_f, .after = reach_id) %>% 
  bind_rows(
    pred.bkt0.y2y %>% mutate(group = "c", species="Brook Trout", age="YOY"),
    pred.bkt1.y2y %>% mutate(group = "c", species="Brook Trout", age="Age-1+"), 
    pred.bnt0.y2y %>% mutate(group = "d", species="Brown Trout", age="YOY"), 
    pred.bnt1.y2y %>% mutate(group = "d", species="Brown Trout", age="Age-1+")
  ) %>% 
  mutate(
    age = factor(age, levels = c("YOY", "Age-1+")), 
    group = factor(group, levels = c("a","b","c","d")))

plot_titles <- c(
  `a` = "(a) Brook Trout, without year random effects",
  `b` = "(b) Brown Trout, without year random effects",
  `c` = "(c) Brook Trout, with year random effects",
  `d` = "(d) Brown Trout, with year random effects"
)


p.pred.trends <- p.data %>% 
  ggplot(aes(x = year, y = exp(fit))) +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper), x = year, 
                  fill = age), 
              alpha = 0.5, inherit.aes = FALSE) +
  geom_line(aes(color = age, linetype = age)) + 
  lemon::facet_rep_wrap(
    vars(group), nrow = 2, scales="free", 
    labeller = as_labeller(plot_titles)) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1994, 2020))+ 
  # scale_y_continuous(limits = c(0,410)) + 
  scale_linetype_manual(values = c("solid", "solid")) + 
  scale_color_manual(values = c("forestgreen", "black")) +
  scale_fill_manual(values = c("forestgreen", "grey80")) +
  # scale_color_manual(values = c("#D8B365", "#5AB4AC")) +
  # scale_fill_manual(values = c("#D8B365", "#5AB4AC")) +
  labs(
    x = "Year",
    y = expression(Relative~density~(fish~mile^{-1}))
    ) + 
  theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = "black", size = 1),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    panel.grid = element_blank(),
    panel.spacing = unit(.5, "lines"),
    axis.ticks = element_line(size = 0.5, color = "black"),
    axis.ticks.length = unit(.1, 'cm'),
    strip.text = element_text(size = rel(1), hjust = 0, 
                              margin=margin(.3,0,.3,0,'cm')),
    # strip.background = element_rect(fill = "white", color = "black", size=1),
    strip.background = element_rect(fill = "white", color = NA),
    legend.position = 'right' , 
    legend.margin = margin(0,0,0,0,'cm')
  ) 
p.pred.trends

# 
# ggsave(here("output","figs","trend_prep_panel.png"), p.pred.trends, 
#        device=ragg::agg_png, res=300, height = 6, width = 9)
# ggsave(here("output","figs","trend_prep_panel.pdf"), p.pred.trends, 
#        device=cairo_pdf, height = 6, width = 9)

