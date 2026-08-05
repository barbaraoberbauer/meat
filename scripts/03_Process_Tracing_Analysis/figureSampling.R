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

# rename attribute translations simply to "translation"
dfReplicationProcess$attended_attribute[dfReplicationProcess$attended_attribute == "rating" |
                                          dfReplicationProcess$attended_attribute == "emission"] <- "translation"

dfReplicationProcess$attended_attribute <- factor(dfReplicationProcess$attended_attribute,
                                                  levels = c("price", "energy", "popularity", "translation"))

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

minRequiredFixations <- 50

# Aggregate data -----

### Eco - Other Option-level sampling -------

###### Stimulus locked -----

option_level_sampling <- calculate_sampling_prob(dfReplicationProcess,
                                                 attended_option,
                                                 fixNum,
                                                 valid_combos = valid_option_combos,
                                                 ci_level = 0.95)

###### Response locked ------

option_level_sampling_rev <- calculate_sampling_prob(dfReplicationProcess,
                                                     attended_option,
                                                     fixNumRev,
                                                     valid_combos = valid_option_combos,
                                                     ci_level = 0.95)

### Chosen - Not Chosen Option-level sampling -------

###### Stimulus locked -----

chosen_option_level_sampling <- calculate_sampling_prob(dfReplicationProcess,
                                                        attended_chosen,
                                                        fixNum,
                                                        valid_combos = valid_chosen_option_combos,
                                                        ci_level = 0.95)


###### Response locked ------

chosen_option_level_sampling_rev <- calculate_sampling_prob(dfReplicationProcess,
                                                            attended_chosen,
                                                            fixNumRev,
                                                            valid_combos = valid_chosen_option_combos,
                                                            ci_level = 0.95)

### Attribute-level sampling -------

###### Stimulus-locked -------

attribute_level_sampling <- calculate_sampling_prob(dfReplicationProcess,
                                                    attended_attribute,
                                                    fixNum,
                                                    valid_combos = valid_attribute_combos,
                                                    ci_level = 0.95)

###### Response-locked -------

attribute_level_sampling_rev <- calculate_sampling_prob(dfReplicationProcess,
                                                        attended_attribute,
                                                        fixNumRev,
                                                        valid_combos = valid_attribute_combos,
                                                        ci_level = 0.95)


# Analyze data -----

### Eco - Other Option-level sampling -------

# transform data to wide format
option_level_subject <- option_level_sampling[["subject"]] %>%
  select(-n_fix) %>%
  pivot_wider(names_from = attended_option,
              values_from = prob_fix) 

# get first and final fixations
fix_first_final <- option_level_subject %>%
  group_by(id, session, consumption_translation) %>%
  mutate(fix_type = case_when(
    fixNum == 1               ~ "first",
    fixNum == max(fixNum)     ~ "final",
    TRUE                      ~ NA_character_
  )) %>%
  filter(!is.na(fix_type)) %>%
  ungroup()

# check that first fixations are at chance level
first_fix_vs_chance <- fix_first_final %>%
  filter(fix_type == "first") %>%
  group_by(session, consumption_translation) %>%
  summarise(
    n         = n(),
    mean_eco  = mean(eco, na.rm = TRUE),
    sd_eco    = sd(eco, na.rm = TRUE),
    test      = list(wilcox.test(eco, mu = 0.5)),
    .groups   = "drop"
  ) %>%
  mutate(
    statistic = map_dbl(test, ~ .x$statistic),
    p_value   = map_dbl(test, ~ .x$p.value)
  ) %>%
  select(-test)

# test eco vs. non eco for first and final fixations
comparison_results <- fix_first_final %>%
  group_by(session, consumption_translation, fix_type) %>%
  summarise(
    n           = n(),
    mean_eco    = mean(eco, na.rm = TRUE),
    mean_other  = mean(other, na.rm = TRUE),
    test        = list(wilcox.test(eco, other, paired = TRUE)),
    .groups     = "drop"
  ) %>%
  mutate(
    statistic = map_dbl(test, ~ .x$statistic),
    p_value   = map_dbl(test, ~ .x$p.value)
  ) %>%
  select(-test)

### Chosen - Not Chosen Option-level sampling -------

# transform data to wide format
chosen_option_level_subject <- chosen_option_level_sampling[["subject"]] %>%
  select(-n_fix) %>%
  pivot_wider(names_from = attended_chosen,
              values_from = prob_fix) 

# get first and final fixations
chosen_fix_first_final <- chosen_option_level_subject %>%
  group_by(id, session, consumption_translation) %>%
  mutate(fix_type = case_when(
    fixNum == 1               ~ "first",
    fixNum == max(fixNum)     ~ "final",
    TRUE                      ~ NA_character_
  )) %>%
  filter(!is.na(fix_type)) %>%
  ungroup()

# test chosen vs. not chosen for first and final fixations
chosen_comparison_results <- chosen_fix_first_final %>%
  group_by(session, consumption_translation, fix_type) %>%
  summarise(
    n           = n(),
    mean_eco    = mean(chosen, na.rm = TRUE),
    mean_other  = mean(not_chosen, na.rm = TRUE),
    test        = list(wilcox.test(chosen, not_chosen, paired = TRUE)),
    .groups     = "drop"
  ) %>%
  mutate(
    statistic = map_dbl(test, ~ .x$statistic),
    p_value   = map_dbl(test, ~ .x$p.value)
  ) %>%
  select(-test)


### Attribute-level sampling -------




# Plot data -----

ylim_option <- c(0.2, 0.8)

color_choice <- rev(color_choice)

### Eco - Other Option-level sampling -------

###### Stimulus locked -----

option_level_sampling_plot <- plot_sampling_probability(option_level_sampling[["group"]],
                                                 minRequiredFixations,
                                                 fixNum,
                                                 attended_option,
                                                 color_choice,
                                                 labelsChoice,
                                                 ylim_option)

###### Response locked ------

option_level_sampling_plot_rev <- plot_sampling_probability(option_level_sampling_rev[["group"]],
                                                            minRequiredFixations,
                                                            fixNumRev,
                                                            attended_option,
                                                            color_choice,
                                                            labelsChoice,
                                                            ylim_option)


### Chosen - Not Chosen Option-level sampling -------

####### Stimulus locked -----

chosen_option_level_sampling_plot <- plot_sampling_probability(chosen_option_level_sampling[["group"]],
                                                                minRequiredFixations,
                                                                fixNum,
                                                                attended_chosen,
                                                                color_chosen,
                                                                labelsChosen,
                                                                ylim_option)


###### Response locked ------

chosen_option_level_sampling_plot_rev <- plot_sampling_probability(chosen_option_level_sampling_rev[["group"]],
                                                            minRequiredFixations,
                                                            fixNumRev,
                                                            attended_chosen,
                                                            color_chosen,
                                                            labelsChosen,
                                                            ylim_option)

### Combine option-level plot -----

setMargin <- margin(5, 5, 5, 5)

remove_y_strip <- theme(strip.text.y = element_blank())

# plots stimulus-locked
stimulus_locked <- ((option_level_sampling_plot + remove_y_strip) +
                      chosen_option_level_sampling_plot) +
  plot_layout(
    axis_titles = 'collect',
  ) +
  plot_annotation(
    title = "Stimulus-Locked"
  ) &
  theme(legend.position = 'top',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin = setMargin)

# plots response-locked
response_locked <- ((option_level_sampling_plot_rev + remove_y_strip) +
                      chosen_option_level_sampling_plot_rev) +
  plot_layout(
    axis_titles = 'collect',
  ) +
  plot_annotation(
    title = "Response-Locked"
  ) &
  theme(legend.position = 'none',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin = setMargin)

# combine plots

option_level_sampling_plot <- wrap_elements(full = stimulus_locked) / 
  wrap_elements(full = response_locked)

#  save plot 
ggsave("figures/optionLevelSampling.png",
       option_level_sampling_plot,
       width = 13,
       height = 12,
       units = "in")



### Attribute-level sampling -------

ylim_attribute <- c(0, 0.75)

colorAttributes <- c(color_attributes, "#2EA0A8")

###### Stimulus-locked -------

attribute_level_sampling_plot <- plot_sampling_probability(attribute_level_sampling[["group"]],
                                                          minRequiredFixations,
                                                          fixNum,
                                                          attended_attribute,
                                                          colorAttributes,
                                                          labelsAttributes,
                                                          ylim_attribute)

attribute_level_sampling_plot <- attribute_level_sampling_plot + ggtitle("Stimulus-Locked")

### Response-locked -------

attribute_level_sampling_plot_rev <- plot_sampling_probability(attribute_level_sampling_rev[["group"]],
                                                           minRequiredFixations,
                                                           fixNumRev,
                                                           attended_attribute,
                                                           colorAttributes,
                                                           labelsAttributes,
                                                           ylim_attribute)

attribute_level_sampling_plot_rev <- attribute_level_sampling_plot_rev + ggtitle("Response-Locked")



### Combine attribute-level plot -----

setMargin <- margin(5, 5, 5, 5)

remove_y_strip <- theme(strip.text.y = element_blank())

# combine plot
attribute_level_sampling_plot <- ((attribute_level_sampling_plot + remove_y_strip) +
                      attribute_level_sampling_plot_rev) +
  plot_layout(
    axis_titles = 'collect',
    guides = 'collect'
  )  &
  theme(legend.position = 'top',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin = setMargin)

#  save plot 
ggsave("figures/attributeLevelSampling.png",
       attribute_level_sampling_plot,
       width = 12,
       height = 6,
       units = "in")









