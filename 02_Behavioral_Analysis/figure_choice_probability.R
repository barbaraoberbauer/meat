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


rm(package, packages, is_package_installed)


### Load data ---------

df <- readRDS("data/df.rds")


# Aggregate choice probabilities ------

df_agg_id <- df %>%
  group_by(id, session, consumption_translation, price_translation) %>%
  summarize(p_choice = mean(choice))

df_agg <- df_agg_id %>%
  group_by(session, consumption_translation, price_translation) %>%
  summarize(p_choice = mean(p_choice))


# Plot choice probabilities -------

color_sessions <- c("#6A66A3", "#C33C54")

fig_choice_prob <- 
df_agg_id %>%
  ggplot(aes(x = consumption_translation, y = p_choice, fill = session)) +
  # geom_dotplot(binaxis = "y",
  #              dotsize = 0.6,
  #              position = position_dodge(width = 0.75)) +
  geom_point(aes(color = session),
             position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.2),
             size = 1.1, alpha = 0.7, show.legend = FALSE) +
  stat_summary(fun = mean, 
               geom = "bar", 
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
  scale_x_discrete(labels = c("control" = "Control",
                              "operating_costs" = "Operating\nCosts",
                              "emissions" = "Carbon\nEmissions",
                              "environmental_friendliness" = "Rating")) +
  labs(x = "Translation of Energy and Water Consumption", 
       y = "Probability of Choosing More Ecological Option", 
       title = '',
       fill = "Session") + 
  theme(axis.text.x=element_text(size=10),
        plot.margin = margin(t = 10,
                             r = 10,
                             b = 10,
                             l = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 12),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top") 

# Save plot
ggsave("figures/choice_probability.png", fig_choice_prob, width = 10, height = 6)

