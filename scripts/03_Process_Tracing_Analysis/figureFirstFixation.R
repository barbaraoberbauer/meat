#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot probability of looking at consumption first
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
              "patchwork")

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

# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataReplication.RData")

# Create variable to code first fixations ----

dfReplicationProcess <- dfReplicationProcess %>%
  mutate(
    ConsumptionLookedAt = as.integer(
      grepl("energy", name, fixed = TRUE) |
        grepl("rating", name, fixed = TRUE) |
        grepl("emission", name, fixed = TRUE)),
    PriceLookedAt = as.integer(
      grepl("price", name, fixed = TRUE))
  )

# Get first fixations

firstFixations <- dfReplicationProcess %>%
  filter(fixNum == 1)

# Probability of eco fixation

firstFixationsAttribute <- firstFixations %>%
  group_by(id, session, consumption_translation) %>%
  summarize(PFirstFixationConsumption = sum(ConsumptionLookedAt)/n(),
            PFirstFixationPrice = sum(PriceLookedAt)/n())

# Plot data -------

hlinesConsumption <- data.frame(
  consumption_translation = c("control", "control", 
                              "emission_replace", "emission_replace",
                              "rating_replace", "rating_replace",
                              "emission_add", "emission_add",
                              "rating_add", "rating_add"),
  session                 = c(1, 2, 
                              1, 2, 
                              1, 2, 
                              1, 2,
                              1, 2),
  yintercept              = c(0.33, 0.33, 
                              0.33, 0.33,
                              0.33, 0.33,
                              0.33, 0.5,
                              0.33, 0.5),
  x_pos                   = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
)

hlinesConsumption$x_pos <- hlinesConsumption$x_pos + ifelse(hlinesConsumption$session == 1, -0.15, 0.15)  # shift per session


hlinesPrice <- data.frame(
  consumption_translation = c("control", "control", 
                              "emission_replace", "emission_replace",
                              "rating_replace", "rating_replace",
                              "emission_add", "emission_add",
                              "rating_add", "rating_add"),
  session                 = c(1, 2, 
                              1, 2, 
                              1, 2, 
                              1, 2,
                              1, 2),
  yintercept              = c(0.33, 0.33, 
                              0.33, 0.33,
                              0.33, 0.33,
                              0.33, 0.25,
                              0.33, 0.25),
  x_pos                   = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
)

hlinesPrice$x_pos <- hlinesPrice$x_pos + ifelse(hlinesConsumption$session == 1, -0.15, 0.15)  # shift per session


plotFirstFixation <- function(attribute, hlines, ytitle){
  
  plot <- 
    ggplot(data = firstFixationsAttribute,
         aes(x = consumption_translation,
             y = attribute,
             fill = session)) +
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
      geom_segment(data = hlines,
                   aes(x     = x_pos - 0.15,   # adjust width to match bar width
                       xend  = x_pos + 0.15,
                       y     = yintercept,
                       yend  = yintercept),
                   linewidth = 1.5,
                   inherit.aes = FALSE,
                   color = "darkred") +
      stat_summary(fun.data = mean_se, 
                   geom = "errorbar", 
                   position = position_dodge(width = 0.6), 
                   width = 0.2, 
                   color = "black",
                   linewidth = 1.2) + 
      scale_fill_manual(values = scales::alpha(color_sessions, 0.4)) +  
      scale_color_manual(values = color_sessions) +
      scale_x_discrete(labels = labelsReplication) +
      labs(x = "Experimental Condition", 
           y = ytitle,
           title = "Replication",
           fill = "Session")
  
  return(plot)
  
}

plotFirstFixationConsumption <- plotFirstFixation(PFirstFixationConsumption,
                                                  hlinesConsumption,
                                                  "P(First Fixation on \nConsumption")









