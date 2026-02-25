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

# Load functions
#source("R/functions/regression_functions.R")

rm(package, packages, is_package_installed)


### Load data ---------

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")

# Aggregate choice probabilities ------

aggregate_function <- function(data){
  
  df_agg_id <- data %>%
    group_by(id, session, consumption_translation) %>%
    summarize(p_choice = mean(choice))
  
  df_agg <- df_agg_id %>%
    group_by(session, consumption_translation) %>%
    summarize(p_choice = mean(p_choice))
  
  return(list(
    df_agg_id = df_agg_id,
    df_agg = df_agg
    )
  )
  
}

aggOriginal <- aggregate_function(dfOriginal)
aggReplication <- aggregate_function(dfReplication)


# Plot choice probabilities -------

fun_plot_choice_prob <- function(data, condition_labels, title){
  
  plot_choice_prob <- 
    ggplot(data = data,
           aes(x = consumption_translation,
               y = p_choice,
               fill = session)) +
    geom_hline(yintercept = 0.5,
               linetype = "dashed",
               linewidth = 1)+ 
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
    scale_fill_manual(values = scales::alpha(color_sessions, 0.4)) +  
    scale_color_manual(values = color_sessions) +
    scale_x_discrete(labels = condition_labels) +
    labs(x = "Experimental Condition", 
         y = "Probability of Choosing \nMore Ecological Option",
         title = title,
         fill = "Session") 
  
  return(plot_choice_prob)
  
} 

plotChoiceProbOriginal <- fun_plot_choice_prob(aggOriginal[["df_agg_id"]], 
                                               labelsOriginal, 
                                               "Original")

plotChoiceProbReplication <- fun_plot_choice_prob(aggReplication[["df_agg_id"]], 
                                                  labelsReplication, 
                                                  "Replication")

# Combine plots

plotChoiceProb <- 
  plotChoiceProbOriginal + 
  plotChoiceProbReplication + 
  plot_layout(guides = 'collect',
              axis_title = 'collect') &
  theme(legend.position = 'top')



# Save plot
saveRDS(plotChoiceProb, "figures/figure3a.rds")
#ggsave("figures/choice_probability_me.png", fig_choice_prob, width = 6, height = 5)


