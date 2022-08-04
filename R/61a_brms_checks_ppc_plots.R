
p.1 <- 
  pp_check(bkt.mod, ndraws=100,type='dens_overlay') + 
  labs(title = "(A) YOY Brook Trout")+ xlim(0,1000)

p.2 <- 
  pp_check(bnt.mod, ndraws=100,type='dens_overlay')+ 
  labs(title = "(B) YOY Brown Trout")+ xlim(0,1000)


p.3 <- 
  pp_check(bkt.mod, ndraws=100,type='ecdf_overlay') + 
  labs(title = "(C) YOY Brook Trout")+ xlim(0,1000)

p.4 <- 
  pp_check(bnt.mod, ndraws=100,type='ecdf_overlay')+ 
  labs(title = "(D) YOY Brown Trout")+ xlim(0,1000)


panel <-(p.1 + p.2) /
  (p.3 + p.4) +
  plot_layout(guides = "collect")

ggsave(here("output","figs","supmat_brm_ppc.png"), panel, 
       device=ragg::agg_png, res=300, height = 4, width = 8)

