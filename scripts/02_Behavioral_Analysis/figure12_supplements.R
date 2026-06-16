#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
# produced under R version: 2024.09.0
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
              "cowplot")

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
library(cowplot)

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ---------

load("data/behavior/preprocessedDataOriginal.RData")

df <- dfOriginal

# Aggregate choice probabilities ------

df_agg_id <- df %>%
  group_by(id, session, consumption_translation, price_translation) %>%
  summarize(p_choice = mean(choice))

df_agg <- df_agg_id %>%
  group_by(session, consumption_translation, price_translation) %>%
  summarize(p_choice = mean(p_choice))


# Plot choice probabilities -------

fig_choice_prob <- 
df_agg_id %>%
  ggplot(aes(x = consumption_translation, 
             y = p_choice, 
             fill = session)) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             linewidth = 1) +
  geom_point(aes(color = session),
             position = position_jitterdodge(dodge.width = 0.5, 
                                             jitter.width = 0.2,
                                             jitter.height = 0),
             size = 1.1, alpha = 0.7, show.legend = FALSE) +
  stat_summary(fun = mean, 
               geom = "bar", 
               linewidth = 1.2,
               position = "dodge",
               width = 0.6, 
               color = "black") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.6), 
               width = 0.2, 
               color = "black",
               linewidth = 1.2) + 
  facet_grid(~price_translation,
             labeller = as_labeller(c("0" = "Price Translation Absent",
                                      "1" = "Price Translation Present"))) +
  scale_fill_manual(values = scales::alpha(color_sessions, 0.4)) +  
  scale_color_manual(values = color_sessions) +
  scale_x_discrete(labels = labelsOriginal) +
  labs(x = "Experimental Condition", 
       y = "Probability of Choosing \nMore Ecological Option", 
       fill = "Session") +
  theme(strip.background = element_rect(color = "black",
                                        fill = "white"),
        strip.text = element_text(face = "bold",
                                  size = 12))

# Save plot
ggsave("figures/choice_probability.png", fig_choice_prob, width = 10, height = 5)

