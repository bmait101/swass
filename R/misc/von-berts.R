# ----------------------------------------------------------------------#
# VBGF - brookies
# ----------------------------------------------------------------------#


# Fit VBGF Brook Trout
wf14T <- la_clean %>% 
  filter(species == "Brook Trout") %>% 
  # filter(!is.na(age)) %>% 
  select(species, age, length) %>% 
  rename(tl = length)
as_tibble(wf14T)


# Need the min and max observed ages
agesum <- wf14T %>% 
  summarize(minage=min(age),
            maxage=max(age), 
            minlen=min(tl), 
            maxlen=max(tl))
agesum


# Get function for typical VBGF:
( vb <- vbFuns(param="Typical") )

# Reasonable starting values for optimizing algorithm:
( f.starts <- vbStarts(tl ~ age, data = wf14T) )

# f.starts$Linf <- 26.4
f.starts

# nls() is used to estimate parameters of the VBGF from the observed data. 
# We use the vb function and starting values created above:
f.fit <- nls(tl ~ vb(age, Linf, K, t0), 
             data = wf14T, 
             start = f.starts)

# Extract parameter estimates
coef(f.fit)

# Bootstraped CIs for the parameter estimates:
f.boot1 <- Boot(f.fit)
confint(f.boot1)

# Predicted lengths-at-age from the fitted VBGF needed to plot fitted curve
predict(f.fit, data.frame(age = 1:5))

# But we need predicted mean lengths at ages for each bootstraped sample
predict2 <- function(x) predict(x, data.frame(age=ages))
ages <- 1:5
predict2(f.fit)  # demonstrates same result as predict() above

# Predicted mean lengths at age with booted CIs
ages <- seq(-1,8, by = 0.2)
f.boot2 <- Boot(f.fit, f = predict2)

# Vector of ages, predicted mean lengths at age (from predict) and booted CIs:
preds1 <- data.frame(ages,
                     predict(f.fit, data.frame(age=ages)),
                     confint(f.boot2))
names(preds1) <- c("age","fit","LCI","UCI")
headtail(preds1)

preds2 <- filter(preds1, age>=agesum$minage, age<=agesum$maxage)
headtail(preds2)

# Plot
ggplot() + 
  geom_ribbon(data=preds2,aes(x=age,ymin=LCI,ymax=UCI),fill="gray90")

vbFitPlot <- ggplot() + 
  geom_ribbon(data=preds2,aes(x=age,ymin=LCI,ymax=UCI),fill="gray90") +
  geom_point(data=wf14T,aes(y=tl,x=age),size=2,alpha=0.1) +
  geom_line(data=preds1,aes(y=fit,x=age),size=1,linetype=2) +
  geom_line(data=preds2,aes(y=fit,x=age),size=1) +
  scale_y_continuous(name="Total Length (mm)",limits=c(0,500),expand=c(0,0)) +
  scale_x_continuous(name="Age (years)",expand=c(0,0),
                     limits=c(-1,9),breaks=seq(0,9,2)) +
  labs(title = "Brook Trout VBGF") + 
  theme(panel.grid=element_blank())
vbFitPlot

makeVBEqnLabel <- function(fit) {
  # Isolate coefficients (and control decimals)
  cfs <- coef(fit)
  Linf <- formatC(cfs[["Linf"]],format="f",digits=1)
  K <- formatC(cfs[["K"]],format="f",digits=3)
  # Handle t0 differently because of minus in the equation
  t0 <- cfs[["t0"]]
  t0 <- paste0(ifelse(t0<0,"+","-"),formatC(abs(t0),format="f",digits=3))
  # Put together and return
  paste0("TL==",Linf,"~bgroup('(',1-e^{-",K,"~(age",t0,")},')')")
}

vbFitPlot + annotate(geom="text",label=makeVBEqnLabel(f.fit),parse=TRUE,
                     size=4,x=Inf,y=-Inf,hjust=1.1,vjust=-0.5)

ggsave("./output/explore/vbgf_bkb.png", plot = last_plot(), width = 4, height = 3)

# ----------------------------------------------------------------------#
# VBGF - browns
# ----------------------------------------------------------------------#

# Fit VBGF Brown Trout
wf14T <- la_clean %>% 
  filter(species == "Brown Trout") %>% 
  # filter(!is.na(age)) %>% 
  select(species, age, length) %>% 
  rename(tl = length)
as_tibble(wf14T)

# Need the min and max observed ages
agesum <- wf14T %>% 
  summarize(minage=min(age),
            maxage=max(age), 
            minlen=min(tl), 
            maxlen=max(tl))
agesum

# Get function for typical VBGF:
( vb <- vbFuns(param="Typical") )

# Reasonable starting values for optimizing algorithm:
( f.starts <- vbStarts(tl ~ age, data = wf14T) )
# f.starts$Linf <- 26.4
f.starts
# nls() is used to estimate parameters of the VBGF from the observed data. 
# We use the vb function and starting values created above:
f.fit <- nls(tl ~ vb(age, Linf, K, t0), 
             data = wf14T, 
             start = f.starts)

# Extract parameter estimates
coef(f.fit)

# Bootstraped CIs for the parameter estimates:
f.boot1 <- Boot(f.fit)
confint(f.boot1)

# Prepare predicted values for plotting
# Predicted lengths-at-age from the fitted VBGF needed to plot fitted curve
predict(f.fit, data.frame(age = 1:6))

# But we need predicted mean lengths at ages for each bootstraped sample
predict2 <- function(x) predict(x, data.frame(age=ages))
ages <- 1:6
predict2(f.fit)  # demonstrates same result as predict() above

# Predicted mean lengths at age with booted CIs
ages <- seq(-1,12, by = 0.2)
f.boot2 <- Boot(f.fit, f = predict2)

# Vector of ages, predicted mean lengths at age (from predict) and booted CIs:
preds1 <- data.frame(ages,
                     predict(f.fit, data.frame(age=ages)),
                     confint(f.boot2))
names(preds1) <- c("age","fit","LCI","UCI")
headtail(preds1)

preds2 <- filter(preds1, age>=agesum$minage, age<=agesum$maxage)
headtail(preds2)

# plot
ggplot() + 
  geom_ribbon(data=preds2,aes(x=age,ymin=LCI,ymax=UCI),fill="gray90")

vbFitPlot <- ggplot() + 
  geom_ribbon(data=preds2,aes(x=age,ymin=LCI,ymax=UCI),fill="gray90") +
  geom_point(data=wf14T,aes(y=tl,x=age),size=2,alpha=0.1) +
  geom_line(data=preds1,aes(y=fit,x=age),size=1,linetype=2) +
  geom_line(data=preds2,aes(y=fit,x=age),size=1) +
  scale_y_continuous(name="Total Length (mm)",limits=c(0,500),expand=c(0,0)) +
  scale_x_continuous(name="Age (years)",expand=c(0,0),
                     limits=c(-1,9),breaks=seq(0,9,2)) +
  labs(title = "Brown Trout VBGF") + 
  theme(panel.grid=element_blank())
vbFitPlot

makeVBEqnLabel <- function(fit) {
  # Isolate coefficients (and control decimals)
  cfs <- coef(fit)
  Linf <- formatC(cfs[["Linf"]],format="f",digits=1)
  K <- formatC(cfs[["K"]],format="f",digits=3)
  # Handle t0 differently because of minus in the equation
  t0 <- cfs[["t0"]]
  t0 <- paste0(ifelse(t0<0,"+","-"),formatC(abs(t0),format="f",digits=3))
  # Put together and return
  paste0("TL==",Linf,"~bgroup('(',1-e^{-",K,"~(age",t0,")},')')")
}

vbFitPlot + annotate(geom="text",label=makeVBEqnLabel(f.fit),parse=TRUE,
                     size=4,x=Inf,y=-Inf,hjust=1.1,vjust=-0.5)

ggsave("./output/explore/vbgf_bnt.png", plot = last_plot(), width = 4, height = 3)

