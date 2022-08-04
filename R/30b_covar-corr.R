## Covariate correlations ----

names(df_drivers)
tmp <- df_drivers |> select("latitude", 29:ncol(df_drivers)) 
tmp <- cor(tmp, use='pairwise.complete.obs')
corrplot::corrplot.mixed(tmp, upper='ellipse', tl.pos='lt',tl.srt=45)

