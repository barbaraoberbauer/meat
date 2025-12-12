#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
#---

# Determine sample size for second study using simR: https://humburg.github.io/Power-Analysis/simr_power_analysis.html 

# Load packages and read data ------------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()


### Install packages -------

# List of packages to check and install if necessary
packages <- c("simr", "lme4", "dplyr", "ggplot2", "ggthemes")

# Function to check if a package is installed
is_package_installed <- function(package_name) {
  is.element(package_name, installed.packages()[, "Package"])
}

# Iterate through the list of packages
for (package in packages) {
  if (!is_package_installed(package)) {
    # Install the package
    install.packages(package)
  }
}

# Load required libraries
library(simr)
library(lme4)
library(dplyr)
library(ggplot2)
library(ggthemes)

rm(package, packages, is_package_installed)

# is this necessary?
simrOptions(progress=FALSE)

### Load data ---------

df <- readRDS("data/df.rds")

### Create function for plotting -----

FUNPlotPower <- function(myData,
                         myBreaks = c(200, 300, 400, 500, 600, 700, 800),
                         myLegendTitle = waiver(),
                         myLegendLabels = waiver()) {
  plot <- myData %>% 
    ggplot(aes(
      x = nlevels,
      y = mean,
      ymin = lower,
      ymax = upper,
      color = type,
      fill = type
    )) +
    geom_ribbon(alpha = .1, color = NA) +
    geom_errorbar(width = 1.5) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = .8, color = "grey70", linetype = "dashed") +
    scale_x_continuous(breaks = myBreaks) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, .05),
      labels = paste0(seq(0, 100, 5), "%")
    ) +
    ggthemes::scale_color_colorblind(labels = myLegendLabels) +
    ggthemes::scale_fill_colorblind(labels = myLegendLabels) +
    labs(
      x = "Number of Participants",
      y = "Power",
      color = myLegendTitle,
      fill = myLegendTitle
    ) +
    theme_bw() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(.875, .21),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black")
    )
  return(plot)
}

# Run regressions -------

### Attention -------

contrasts(df$consumption_translation) <- contr.treatment(levels(df$consumption_translation), base = 1)

m.attention <- lmer(
  diff_t_options ~ (1|id) + (1|task) + session * consumption_translation,
  data = df,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(m.attention)

### Choice --------

contrasts(df$consumption_translation) <- contr.treatment(levels(df$consumption_translation), base = 1)

m.choice <- glmer(
  choice ~ (session | id) + (1 | task) + session * consumption_translation, 
  data = df, 
  family = binomial(link = "logit"), 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=2e5))
)

# Power analysis -----

### Create covariates ----

# Define variables
session <- 1:2
id <- 1:1000
task <- 1:12
condition_names <- c("control", "emission_add", "emission_rep", "rating_add", "rating_rep")
condition <- rep(condition_names, length.out = length(id))  # Assign conditions evenly

# Create a dataframe
X <- expand.grid(session = session, id = id, task = task)

# Merge the condition assignment
condition_df <- data.frame(id = id, condition = condition)
X <- merge(X, condition_df, by = "id")

# mutate variables to factors
X <- X %>% 
  mutate(across(c(session, condition, task, id), as.factor))

### Attention ------

###### Define fixed and random effects ----

# Define fixed effect coefficients
fixed_effects <- c(
  "(Intercept)" = fixef(m.attention)['(Intercept)'] %>% as.numeric(),
  "session2" = fixef(m.attention)['session2'] %>% as.numeric(),
  "conditionemission_add" = fixef(m.attention)['consumption_translationemissions'] %>% as.numeric(),
  "conditionemission_rep" = fixef(m.attention)['consumption_translationemissions'] %>% as.numeric(),
  "conditionrating_add" = fixef(m.attention)['consumption_translationenvironmental_friendliness'] %>% as.numeric(),
  "conditionrating_rep" = fixef(m.attention)['consumption_translationenvironmental_friendliness'] %>% as.numeric(),
  "session2:conditionemission_add" = fixef(m.attention)['session2:consumption_translationemissions'] %>% as.numeric(),
  "session2:conditionemission_rep" = fixef(m.attention)['session2:consumption_translationemissions'] %>% as.numeric(),
  "session2:conditionrating_add" = fixef(m.attention)['session2:consumption_translationenvironmental_friendliness'] %>% as.numeric(),
  "session2:conditionrating_rep" = fixef(m.attention)['session2:consumption_translationenvironmental_friendliness'] %>% as.numeric()
)

# Define random effect standard deviations
random_effects <- list(
  id = matrix(0.04888^2, 1, 1, dimnames = list("(Intercept)", "(Intercept)")),
  task = matrix(0.07585^2, 1, 1, dimnames = list("(Intercept)", "(Intercept)"))
)

# Define residual standard deviation
residual_sd <- 0.21053

###### Make model using simR ------

sim_m.attention <- simr::makeLmer(
  formula = diff_t_options ~ session * condition + (1|id) + (1|task),
  fixef = fixed_effects,
  VarCorr = random_effects,
  sigma = residual_sd,
  data = X
)

###### Run power analysis ------

# number of simulations
nSims <- 1000

# create a power curve data set
powC.sim_attention <- simr::powerCurve(
  fit = sim_m.attention,
  test = fixed(xname = "session2:conditionrating_add", method = "z"),
  along = "id",
  breaks = c(200, 300, 400, 500, 600, 700),
  nsim = nSims,
  progress = TRUE
)

print(powC.sim_attention)

# save simulation results in a data frame
time <- format(Sys.time(), "%Y%m%d_%H%M")
fileName <- paste0("powC.sim_attention", "_", time, ".RData")

save(
  sim_m.attention,
  powC.sim_attention,
  file = file.path("data/", fileName)
)

###### Plot power analysis -----

# create and display plot
p.powC.sim_attention <- summary(powC.sim_attention) %>% 
  ggplot(aes(
    x = nlevels,
    y = mean,
    ymin = lower,
    ymax = upper
  )) +
  geom_ribbon(fill = "grey94", alpha = .8) +
  geom_errorbar(color = "grey40", width = 1.5) +
  geom_line(color = "black") +
  geom_point() +
  geom_hline(yintercept = .8, color = "grey70", linetype = "dashed") +
  scale_x_continuous(breaks = summary(powC.sim_attention)$nlevels) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, .05),
    labels = paste0(seq(0, 100, 5), "%")
  ) +
  labs(
    x = "Number of Participants",
    y = "Power"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )

###### Run power analysis for lower effect sizes -----

# choose smaller effect sizes and save these in new models

# will choose the new effect size to be 10% smaller than in Mertens data
m.powC.sim_attention_minus10prcnt <- sim_m.attention
fixef(m.powC.sim_attention_minus10prcnt)["session2:conditionrating_add"] <- (1-.1)*fixef(sim_m.attention)["session2:conditionrating_add"]

# will choose the new effect size to be 20% smaller than in the Mertens data
m.powC.sim_attention_minus20prcnt <- sim_m.attention
fixef(m.powC.sim_attention_minus20prcnt)["session2:conditionrating_add"] <- (1-.2)*fixef(sim_m.attention)["session2:conditionrating_add"]

# will choose the new effect size to be 30% smaller than in the Mertens data
m.powC.sim_attention_minus30prcnt <- sim_m.attention
fixef(m.powC.sim_attention_minus30prcnt)["session2:conditionrating_add"] <- (1-.3)*fixef(sim_m.attention)["session2:conditionrating_add"]

# will choose the new effect size to be 40% smaller than in the Mertens data
m.powC.sim_attention_minus40prcnt <- sim_m.attention
fixef(m.powC.sim_attention_minus40prcnt)["session2:conditionrating_add"] <- (1-.4)*fixef(sim_m.attention)["session2:conditionrating_add"]

# will choose the new effect size to be 50% smaller than in the Mertens data
m.powC.sim_attention_minus50prcnt <- sim_m.attention
fixef(m.powC.sim_attention_minus50prcnt)["session2:conditionrating_add"] <- (1-.5)*fixef(sim_m.attention)["session2:conditionrating_add"]

# test whether simulation will work
doTest(m.powC.sim_attention_minus50prcnt, fixed("session2:conditionrating_add", "z"))

## how many simulations should be done for each number of subjects?
nSims <- 1000

# what are the breaks for number of subjects we would like to calculate power for?
breaks_subj <- c(200, 300, 400, 500, 600, 700, 800)

# create power curve data sets

powC.sim_attention_minus10prcnt <- powerCurve(
  fit = m.powC.sim_attention_minus10prcnt,
  test = fixed("session2:conditionrating_add", 
               method = "z"),
  along = "id",
  breaks = breaks_subj,
  nsim = nSims,
  progress = TRUE
)
powC.sim_attention_minus20prcnt <- powerCurve(
  fit = m.powC.sim_attention_minus20prcnt,
  test = fixed("session2:conditionrating_add", 
               method = "z"),
  along = "id",
  breaks = breaks_subj,
  nsim = nSims,
  progress = TRUE
)
powC.sim_attention_minus30prcnt <- powerCurve(
  fit = m.powC.sim_attention_minus30prcnt,
  test = fixed("session2:conditionrating_add", 
               method = "z"),
  along = "id",
  breaks = breaks_subj,
  nsim = nSims,
  progress = TRUE
)
powC.sim_attention_minus40prcnt <- powerCurve(
  fit = m.powC.sim_attention_minus40prcnt,
  test = fixed("session2:conditionrating_add", 
               method = "z"),
  along = "id",
  breaks = breaks_subj,
  nsim = nSims,
  progress = TRUE
)
powC.sim_attention_minus50prcnt <- powerCurve(
  fit = m.powC.sim_attention_minus50prcnt,
  test = fixed("session2:conditionrating_add", 
               method = "z"),
  along = "id",
  breaks = breaks_subj,
  nsim = nSims,
  progress = TRUE
)

# combine power curve data sets
powC.sim_attention.effectsize <- rbind(
  summary(powC.sim_attention) %>% 
    mutate(type = "MertensData"),
  summary(powC.sim_attention_minus10prcnt) %>% 
    mutate(type = "effectsize.minus10%"),
  summary(powC.sim_attention_minus20prcnt) %>% 
    mutate(type = "effectsize.minus20%"),
  summary(powC.sim_attention_minus30prcnt) %>% 
    mutate(type = "effectsize.minus30%"),
  summary(powC.sim_attention_minus40prcnt) %>% 
    mutate(type = "effectsize.minus40%"),
  summary(powC.sim_attention_minus50prcnt) %>% 
    mutate(type = "effectsize.minus50%")
)

# save simulation results in a data frame
time <- format(Sys.time(), "%Y%m%d_%H%M")
fileName <- paste0("powC.sim_attention.effectsize", "_", time, ".RData")

save(
  powC.sim_attention_minus10prcnt,
  powC.sim_attention_minus20prcnt,
  powC.sim_attention_minus30prcnt,
  powC.sim_attention_minus40prcnt,
  powC.sim_attention_minus50prcnt,
  powC.sim_attention.effectsize,
  file = file.path("data/", fileName)
)

####### Plot power analysis with fewer effect sizes -----

# prepare data for plot: add + 2 on x axis for every type in order to prevent
# overprinting of errorbars
dataForPlot <- rbind(
  filter(powC.sim_attention.effectsize, type == "MertensData"),
  filter(powC.sim_attention.effectsize, type == "effectsize.minus10%") %>% 
    mutate(nlevels = nlevels + 1*2),
  filter(powC.sim_attention.effectsize, type == "effectsize.minus20%") %>% 
    mutate(nlevels = nlevels + 2*2),
  filter(powC.sim_attention.effectsize, type == "effectsize.minus30%") %>% 
    mutate(nlevels = nlevels + 3*2),
  filter(powC.sim_attention.effectsize, type == "effectsize.minus40%") %>% 
    mutate(nlevels = nlevels + 4*2),
  filter(powC.sim_attention.effectsize, type == "effectsize.minus50%") %>% 
    mutate(nlevels = nlevels + 5*2)
)
# define levels of type
dataForPlot <- dataForPlot %>% 
  mutate(type = factor(type,
                       levels = c(
                         "MertensData",
                         "effectsize.minus10%",
                         "effectsize.minus20%",
                         "effectsize.minus30%",
                         "effectsize.minus40%",
                         "effectsize.minus50%"
                       )))

tmp.legendLabels <- c(
  paste0("Mertens data = ", round(fixef(sim_m.attention)['session2:conditionrating_add'], 4)),
  paste0("-", seq(10, 50, 10), "% = ", round(fixef(sim_m.attention)['session2:conditionrating_add']*(1-seq(.1, .5, .1)), digits = 4))
)



# create and display plot
p.powC.sim_attention.effectSizes <- FUNPlotPower(
  myData= dataForPlot,
  myLegendTitle = "Effect Size",
  myLegendLabels = tmp.legendLabels
)

p.powC.sim_attention.effectSizes


### Choice ------

###### Define fixed and random effects ----

# Define fixed effect coefficients
fixed_effects_choice <- c(
  "(Intercept)" = fixef(m.choice)['(Intercept)'] %>% as.numeric(),
  "session2" = fixef(m.choice)['session2'] %>% as.numeric(),
  "conditionemission_add" = fixef(m.choice)['consumption_translationemissions'] %>% as.numeric(),
  "conditionemission_rep" = fixef(m.choice)['consumption_translationemissions'] %>% as.numeric(),
  "conditionrating_add" = fixef(m.choice)['consumption_translationenvironmental_friendliness'] %>% as.numeric(),
  "conditionrating_rep" = fixef(m.choice)['consumption_translationenvironmental_friendliness'] %>% as.numeric(),
  "session2:conditionemission_add" = fixef(m.choice)['session2:consumption_translationemissions'] %>% as.numeric(),
  "session2:conditionemission_rep" = fixef(m.choice)['session2:consumption_translationemissions'] %>% as.numeric(),
  "session2:conditionrating_add" = fixef(m.choice)['session2:consumption_translationenvironmental_friendliness'] %>% as.numeric(),
  "session2:conditionrating_rep" = fixef(m.choice)['session2:consumption_translationenvironmental_friendliness'] %>% as.numeric()
)

# Define random effect standard deviations
random_effects_choice <- list(
  id = matrix(c(
    2.412^2, -0.37 * 2.412 * 1.580,  # Variance of intercept & covariance
    -0.37 * 2.412 * 1.580, 1.580^2   # Covariance & variance of session2
  ), 2, 2, dimnames = list(c("(Intercept)", "session2"), c("(Intercept)", "session2"))),
  
  task = matrix(3.462^2, 1, 1, dimnames = list("(Intercept)", "(Intercept)"))
)

# Define residual standard deviation
residual_sd_choice <- 1

###### Make model using simR ------

sim_m.choice <- simr::makeLmer(
  formula = choice ~ session * condition + (session|id) + (1|task),
  fixef = fixed_effects_choice,
  VarCorr = random_effects_choice,
  sigma = residual_sd_choice,
  data = X
)

###### Run power analysis ------

# number of simulations
nSims <- 1000

# create a power curve data set
powC.sim_choice <- simr::powerCurve(
  fit = sim_m.choice,
  test = fixed(xname = "session2:conditionrating_add", method = "z"),
  along = "id",
  breaks = c(200, 300, 400, 500, 600, 700),
  nsim = nSims,
  progress = TRUE
)

print(powC.sim_choice)

# save simulation results in a data frame
time <- format(Sys.time(), "%Y%m%d_%H%M")
fileName <- paste0("powC.sim_choice", "_", time, ".RData")

save(
  sim_m.choice,
  powC.sim_choice,
  file = file.path("data/", fileName)
)

###### Plot power analysis -----

# create and display plot
p.powC.sim_choice <- summary(powC.sim_choice) %>%
  ggplot(aes(
    x = nlevels,
    y = mean,
    ymin = lower,
    ymax = upper
  )) +
  geom_ribbon(fill = "grey94", alpha = .8) +
  geom_errorbar(color = "grey40", width = 1.5) +
  geom_line(color = "black") +
  geom_point() +
  geom_hline(yintercept = .8, color = "grey70", linetype = "dashed") +
  scale_x_continuous(breaks = summary(powC.sim_attention)$nlevels) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, .05),
    labels = paste0(seq(0, 100, 5), "%")
  ) +
  labs(
    x = "Number of Participants",
    y = "Power"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )

# ###### Run power analysis for lower effect sizes -----
# 
# # choose smaller effect sizes and save these in new models
# 
# # will choose the new effect size to be 10% smaller than in Mertens data
# m.powC.sim_attention_minus10prcnt <- sim_m.attention
# fixef(m.powC.sim_attention_minus10prcnt)["session2:conditionrating_add"] <- (1-.1)*fixef(sim_m.attention)["session2:conditionrating_add"]
# 
# # will choose the new effect size to be 20% smaller than in the Mertens data
# m.powC.sim_attention_minus20prcnt <- sim_m.attention
# fixef(m.powC.sim_attention_minus20prcnt)["session2:conditionrating_add"] <- (1-.2)*fixef(sim_m.attention)["session2:conditionrating_add"]
# 
# # will choose the new effect size to be 30% smaller than in the Mertens data
# m.powC.sim_attention_minus30prcnt <- sim_m.attention
# fixef(m.powC.sim_attention_minus30prcnt)["session2:conditionrating_add"] <- (1-.3)*fixef(sim_m.attention)["session2:conditionrating_add"]
# 
# # will choose the new effect size to be 40% smaller than in the Mertens data
# m.powC.sim_attention_minus40prcnt <- sim_m.attention
# fixef(m.powC.sim_attention_minus40prcnt)["session2:conditionrating_add"] <- (1-.4)*fixef(sim_m.attention)["session2:conditionrating_add"]
# 
# # will choose the new effect size to be 50% smaller than in the Mertens data
# m.powC.sim_attention_minus50prcnt <- sim_m.attention
# fixef(m.powC.sim_attention_minus50prcnt)["session2:conditionrating_add"] <- (1-.5)*fixef(sim_m.attention)["session2:conditionrating_add"]
# 
# # test whether simulation will work
# doTest(m.powC.sim_attention_minus50prcnt, fixed("session2:conditionrating_add", "z"))
# 
# ## how many simulations should be done for each number of subjects?
# nSims <- 1000
# 
# # what are the breaks for number of subjects we would like to calculate power for?
# breaks_subj <- c(200, 300, 400, 500, 600, 700, 800)
# 
# # create power curve data sets
# 
# powC.sim_attention_minus10prcnt <- powerCurve(
#   fit = m.powC.sim_attention_minus10prcnt,
#   test = fixed("session2:conditionrating_add", 
#                method = "z"),
#   along = "id",
#   breaks = breaks_subj,
#   nsim = nSims,
#   progress = TRUE
# )
# powC.sim_attention_minus20prcnt <- powerCurve(
#   fit = m.powC.sim_attention_minus20prcnt,
#   test = fixed("session2:conditionrating_add", 
#                method = "z"),
#   along = "id",
#   breaks = breaks_subj,
#   nsim = nSims,
#   progress = TRUE
# )
# powC.sim_attention_minus30prcnt <- powerCurve(
#   fit = m.powC.sim_attention_minus30prcnt,
#   test = fixed("session2:conditionrating_add", 
#                method = "z"),
#   along = "id",
#   breaks = breaks_subj,
#   nsim = nSims,
#   progress = TRUE
# )
# powC.sim_attention_minus40prcnt <- powerCurve(
#   fit = m.powC.sim_attention_minus40prcnt,
#   test = fixed("session2:conditionrating_add", 
#                method = "z"),
#   along = "id",
#   breaks = breaks_subj,
#   nsim = nSims,
#   progress = TRUE
# )
# powC.sim_attention_minus50prcnt <- powerCurve(
#   fit = m.powC.sim_attention_minus50prcnt,
#   test = fixed("session2:conditionrating_add", 
#                method = "z"),
#   along = "id",
#   breaks = breaks_subj,
#   nsim = nSims,
#   progress = TRUE
# )
# 
# # combine power curve data sets
# powC.sim_attention.effectsize <- rbind(
#   summary(powC.sim_attention) %>% 
#     mutate(type = "MertensData"),
#   summary(powC.sim_attention_minus10prcnt) %>% 
#     mutate(type = "effectsize.minus10%"),
#   summary(powC.sim_attention_minus20prcnt) %>% 
#     mutate(type = "effectsize.minus20%"),
#   summary(powC.sim_attention_minus30prcnt) %>% 
#     mutate(type = "effectsize.minus30%"),
#   summary(powC.sim_attention_minus40prcnt) %>% 
#     mutate(type = "effectsize.minus40%"),
#   summary(powC.sim_attention_minus50prcnt) %>% 
#     mutate(type = "effectsize.minus50%")
# )
# 
# # save simulation results in a data frame
# time <- format(Sys.time(), "%Y%m%d_%H%M")
# fileName <- paste0("powC.sim_attention.effectsize", "_", time, ".RData")
# 
# save(
#   powC.sim_attention_minus10prcnt,
#   powC.sim_attention_minus20prcnt,
#   powC.sim_attention_minus30prcnt,
#   powC.sim_attention_minus40prcnt,
#   powC.sim_attention_minus50prcnt,
#   powC.sim_attention.effectsize,
#   file = file.path("data/", fileName)
# )
# 
# ####### Plot power analysis with fewer effect sizes -----
# 
# # prepare data for plot: add + 2 on x axis for every type in order to prevent
# # overprinting of errorbars
# dataForPlot <- rbind(
#   filter(powC.sim_attention.effectsize, type == "MertensData"),
#   filter(powC.sim_attention.effectsize, type == "effectsize.minus10%") %>% 
#     mutate(nlevels = nlevels + 1*2),
#   filter(powC.sim_attention.effectsize, type == "effectsize.minus20%") %>% 
#     mutate(nlevels = nlevels + 2*2),
#   filter(powC.sim_attention.effectsize, type == "effectsize.minus30%") %>% 
#     mutate(nlevels = nlevels + 3*2),
#   filter(powC.sim_attention.effectsize, type == "effectsize.minus40%") %>% 
#     mutate(nlevels = nlevels + 4*2),
#   filter(powC.sim_attention.effectsize, type == "effectsize.minus50%") %>% 
#     mutate(nlevels = nlevels + 5*2)
# )
# # define levels of type
# dataForPlot <- dataForPlot %>% 
#   mutate(type = factor(type,
#                        levels = c(
#                          "MertensData",
#                          "effectsize.minus10%",
#                          "effectsize.minus20%",
#                          "effectsize.minus30%",
#                          "effectsize.minus40%",
#                          "effectsize.minus50%"
#                        )))
# 
# tmp.legendLabels <- c(
#   paste0("Mertens data = ", round(fixef(sim_m.attention)['session2:conditionrating_add'], 4)),
#   paste0("-", seq(10, 50, 10), "% = ", round(fixef(sim_m.attention)['session2:conditionrating_add']*(1-seq(.1, .5, .1)), digits = 4))
# )
# 
# # function for plotting
# FUNPlotPower <- function(myData,
#                          myBreaks = c(200, 300, 400, 500, 600, 700, 800),
#                          myLegendTitle = waiver(),
#                          myLegendLabels = waiver()) {
#   plot <- myData %>% 
#     ggplot(aes(
#       x = nlevels,
#       y = mean,
#       ymin = lower,
#       ymax = upper,
#       color = type,
#       fill = type
#     )) +
#     geom_ribbon(alpha = .1, color = NA) +
#     geom_errorbar(width = 1.5) +
#     geom_line() +
#     geom_point() +
#     geom_hline(yintercept = .8, color = "grey70", linetype = "dashed") +
#     scale_x_continuous(breaks = myBreaks) +
#     scale_y_continuous(
#       limits = c(0, 1),
#       breaks = seq(0, 1, .05),
#       labels = paste0(seq(0, 100, 5), "%")
#     ) +
#     ggthemes::scale_color_colorblind(labels = myLegendLabels) +
#     ggthemes::scale_fill_colorblind(labels = myLegendLabels) +
#     labs(
#       x = "Number of Participants",
#       y = "Power",
#       color = myLegendTitle,
#       fill = myLegendTitle
#     ) +
#     theme_bw() +
#     theme(
#       legend.position = "inside",
#       legend.position.inside = c(.875, .21),
#       panel.grid.minor = element_blank(),
#       panel.border = element_blank(),
#       axis.line = element_line(color = "black")
#     )
#   return(plot)
# }
# 
# # create and display plot
# p.powC.sim_attention.effectSizes <- FUNPlotPower(
#   myData= dataForPlot,
#   myLegendTitle = "Effect Size",
#   myLegendLabels = tmp.legendLabels
# )
# 
# p.powC.sim_attention.effectSizes
