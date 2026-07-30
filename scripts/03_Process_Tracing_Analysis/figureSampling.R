#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot sampling of options and attributes throughout trial
#---

# Load packages and read data ------

### Clear environment -------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()

### Install packages -------

# List of packages to check and install if necessary
packages <- c("tidyverse",
              "dplyr",
              "ggplot2",
              "patchwork",
              "rlang"
              )

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
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(rlang)

# Load required function
source("R/functions/fun_calculate_sampling_probability.R")
source("R/functions/fun_plot_sampling_probability.R")

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ---------

load("data/behavior/preprocessedDataReplication.RData")

# Prepare data ---------

# split attribute and option info
dfReplicationProcess$attended_option <- ifelse(grepl("NonEco", dfReplicationProcess$name), 
                                               "other", 
                                               "eco")

dfReplicationProcess$attended_option <- as.factor(dfReplicationProcess$attended_option)

# strip off option description
dfReplicationProcess$attended_attribute <- dfReplicationProcess$name %>%
  gsub("NonEco", "", .) %>%
  gsub("Eco", "", .)

dfReplicationProcess$attended_attribute <- as.factor(dfReplicationProcess$attended_attribute)

# add info about whether attended option was chosen
dfReplicationProcess$attended_chosen <- ifelse((dfReplicationProcess$choice == 1 & dfReplicationProcess$attended_option == "eco") |
                                                 (dfReplicationProcess$choice == 0 & dfReplicationProcess$attended_option == "other"), "chosen", "not_chosen")

dfReplicationProcess$attended_chosen <- as.factor(dfReplicationProcess$attended_chosen)

# add reversed fixation count to plot response-locked
dfReplicationProcess <- dfReplicationProcess %>%
  group_by(id, session, consumption_translation, trial) %>%
  arrange(fixNum, .by_group = TRUE) %>%
  mutate(fixNumRev = n() - row_number() + 1) %>%
  ungroup()

# define valid combinations of options and consumption translations
valid_option_combos <- dfReplicationProcess %>%
  distinct(consumption_translation, attended_option, session)

# define valid combinations of chosen options and consumption translations
valid_chosen_option_combos <- dfReplicationProcess %>%
  distinct(consumption_translation, attended_chosen, session)

# define valid combinations of attributes and consumption translations
valid_attribute_combos <- dfReplicationProcess %>%
  distinct(consumption_translation, attended_attribute, session)

minRequiredFixations <- 100


# Eco - Other Option-level sampling -------

### Stimulus locked -----

option_level_sampling <- calculate_sampling_prob(dfReplicationProcess,
                                                 attended_option,
                                                 fixNum,
                                                 valid_combos = valid_option_combos,
                                                 ci_level = 0.95)

color_choice <- rev(color_choice)

option_level_sampling_plot <- plot_sampling_probability(option_level_sampling[["group"]],
                                                 minRequiredFixations,
                                                 fixNum,
                                                 attended_option,
                                                 color_choice,
                                                 labelsChoice)

option_level_sampling_plot <- option_level_sampling_plot + ggtitle("Stimulus-Locked")

### Response locked ------

option_level_sampling_rev <- calculate_sampling_prob(dfReplicationProcess,
                                                     attended_option,
                                                     fixNumRev,
                                                     valid_combos = valid_option_combos,
                                                     ci_level = 0.95)

option_level_sampling_plot_rev <- plot_sampling_probability(option_level_sampling_rev[["group"]],
                                                            minRequiredFixations,
                                                            fixNumRev,
                                                            attended_option,
                                                            color_choice,
                                                            labelsChoice)

option_level_sampling_plot_rev <- option_level_sampling_plot_rev + ggtitle("Response-Locked")


# Chosen - Not Chosen Option-level sampling -------

### Stimulus locked -----

chosen_option_level_sampling <- calculate_sampling_prob(dfReplicationProcess,
                                                         attended_chosen,
                                                         fixNum,
                                                         valid_combos = valid_chosen_option_combos,
                                                         ci_level = 0.95)


chosen_option_level_sampling_plot <- plot_sampling_probability(chosen_option_level_sampling[["group"]],
                                                                minRequiredFixations,
                                                                fixNum,
                                                                attended_chosen,
                                                                color_chosen,
                                                                labelsChosen)

chosen_option_level_sampling_plot <- chosen_option_level_sampling_plot + ggtitle("Stimulus-Locked")

### Response locked ------

chosen_option_level_sampling_rev <- calculate_sampling_prob(dfReplicationProcess,
                                                     attended_chosen,
                                                     fixNumRev,
                                                     valid_combos = valid_chosen_option_combos,
                                                     ci_level = 0.95)

chosen_option_level_sampling_plot_rev <- plot_sampling_probability(chosen_option_level_sampling_rev[["group"]],
                                                            minRequiredFixations,
                                                            fixNumRev,
                                                            attended_chosen,
                                                            color_chosen,
                                                            labelsChosen)










# calculate subject-level option sampling probabilities
subj_option_sampling_prob <- dfReplicationProcess %>%
  count(id, session, consumption_translation, fixNum, attended_option, name = "n_fix") %>%
  complete(nesting(id, session, consumption_translation, fixNum), attended_option,
           fill = list(n_fix = 0)) %>%
  group_by(id, session, consumption_translation, fixNum) %>%
  mutate(prob_fix = n_fix / sum(n_fix)) %>%
  ungroup()

# aggregate
group_option_sampling_prob <- subj_option_sampling_prob %>%
  group_by(session, consumption_translation, fixNum, attended_option) %>%
  summarize(
    mean_prob = mean(prob_fix, na.rm = TRUE),
    sd_prob   = sd(prob_fix, na.rm = TRUE),
    n         = sum(!is.na(prob_fix)),
    .groups = "drop"
  ) %>%
  mutate(
    se_prob  = ifelse(n >= 2, sd_prob / sqrt(n), NA_real_),
    ci_lower = ifelse(n >= 2, mean_prob - qt(0.975, df = pmax(n - 1, 1)) * se_prob, NA_real_),
    ci_upper = ifelse(n >= 2, mean_prob + qt(0.975, df = pmax(n - 1, 1)) * se_prob, NA_real_)
  )

group_option_sampling_prob %>%
  filter(n > 5) %>%
  ggplot(aes(x = fixNum,
             y = mean_prob,
             color = attended_option,
             group = attended_option)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_point() +
  geom_line() +
  facet_grid(consumption_translation ~ session)











# Calculate sampling probabilities --------





# calculate subject-level sampling probabilities
subj_sampling_prob <- dfReplicationProcess %>%
  count(id, session, consumption_translation, fixNum, attended_attribute, name = "n_fix") %>%
  complete(nesting(id, session, consumption_translation, fixNum), attended_attribute,
           fill = list(n_fix = 0)) %>%
  semi_join(valid_combos, by = c("consumption_translation", "attended_attribute", "session")) %>%
  group_by(id, session, consumption_translation, fixNum) %>%
  mutate(prob_fix = n_fix / sum(n_fix)) %>%
  ungroup()
  
# aggregate
group_sampling_prob <- subj_sampling_prob %>%
  group_by(session, consumption_translation, fixNum, attended_attribute) %>%
  summarize(
    mean_prob = mean(prob_fix, na.rm = TRUE),
    sd_prob   = sd(prob_fix, na.rm = TRUE),
    n         = sum(!is.na(prob_fix)),
    .groups = "drop"
  ) %>%
  mutate(
    se_prob  = ifelse(n >= 2, sd_prob / sqrt(n), NA_real_),
    ci_lower = ifelse(n >= 2, mean_prob - qt(0.975, df = pmax(n - 1, 1)) * se_prob, NA_real_),
    ci_upper = ifelse(n >= 2, mean_prob + qt(0.975, df = pmax(n - 1, 1)) * se_prob, NA_real_)
  )

# Plot -------

group_sampling_prob %>%
  filter(n > 5) %>%
  ggplot(aes(x = fixNum,
             y = mean_prob,
             color = attended_attribute,
             group = attended_attribute)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_point() +
  geom_line() +
  facet_grid(consumption_translation ~ session)



