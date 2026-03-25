#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot dwell time and fixation frequency for attributes
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

load("data/preprocessedDataOriginal.RData")
load("data/preprocessedDataReplication.RData")

# for comparison, only use data without price_translation in original study
dfOriginalSubset <- dfOriginal %>%
  filter(price_translation == 0)

# Calculate fix count and dwell time -----

calcFixData <- function(data){
  
  # calculate relative fix count and dwell time for each participant, session, and consumption translation
  # and summarize within participant
  fixData <- data %>%
    group_by(id, consumption_translation, session) %>%
    select(f_price0, f_price1,
           f_consumption0, f_consumption1,
           f_popularity0, f_popularity1,
           f_consumption_translation0, f_consumption_translation1,
           t_price0, t_price1,
           t_consumption0, t_consumption1,
           t_popularity0, t_popularity1,
           t_consumption_translation0, t_consumption_translation1,
           f_total, t_option0, t_option1) %>%  # include divisor columns
    mutate(
      across(starts_with("f_") & !matches("f_total"), \(x) x / f_total),
      across(starts_with("t_") & !matches("t_option0|t_option1"), \(x) x / (t_option0 + t_option1))
    ) %>%
    select(-f_total, -t_option0, -t_option1) %>%  # drop divisor columns
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  
  # transform into long format
  fixData <- fixData %>%
    pivot_longer(
      cols = 'f_price0':'t_consumption_translation1',
      names_to = "attribute",
      values_to = "fix_info"
    )
  
  return(fixData)
  
}

fixDataOriginal <- calcFixData(dfOriginalSubset)
fixDataReplication <- calcFixData(dfReplication)

# Get fixation count and dwell times ------

fixCountOriginal <- fixDataOriginal %>%
  filter(str_detect(attribute, "^f_"))

fixCountReplication <- fixDataReplication %>%
  filter(str_detect(attribute, "^f_"))

dwellTimeOriginal <- fixDataOriginal %>%
  filter(str_detect(attribute, "^t_"))

dwellTimeReplication <- fixDataReplication %>%
  filter(str_detect(attribute, "^t_"))


# Plot dwell time -------

plotDwellTime <- function(data, labels, title){
  
  dwellTime_plot <- data %>%
    #filter(consumption_translation == "rating_add") %>%
    mutate(
      attribute_plot = str_replace(attribute, "t_consumption_translation(\\d)", "t_consumption\\1"),
      bar_group = ifelse(str_detect(attribute, "t_consumption_translation"), 
                         "translation", 
                         "original")
    )
  
  plot <- 
    ggplot(data = dwellTime_plot,
           aes(x = attribute_plot, 
               y = fix_info, 
               fill = interaction(session, bar_group))) +
    stat_summary(
      fun = mean,
      geom = "bar",
      position = position_dodge(width = 0.6),
      width = 0.6,
      color = "black",
      linewidth = 1.2
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      position = position_dodge(width = 0.6),
      width = 0.2,
      color = "black",
      linewidth = 1.2
    ) +
    scale_fill_manual(values = scales::alpha(c(color_sessions, "white"), 0.4),
                      labels = c("1", "2", "Attribute Translation")) +
    scale_x_discrete(labels = labelsAttributesT) +
    coord_cartesian(ylim = c(0, 0.25)) +
    facet_wrap(~consumption_translation, 
               nrow = 2,
               labeller = labeller(consumption_translation = labels)) +
    labs(x = "Attribute",
         y = "Relative Dwell Time",
         title = title,
         fill = "Session") +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12,
                                    face = "bold"),  # style for facet labels
          panel.border = element_rect(color = "black", fill = NA),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
  
  return(plot)
  
}

plotDwellTimeOriginal <- plotDwellTime(dwellTimeOriginal, labelsOriginal, "Original")
plotDwellTimeReplication <- plotDwellTime(dwellTimeReplication, labelsReplication, "Replication")


# Combine plots

plotDwellTimeAll <- 
  plotDwellTimeOriginal + 
  plotDwellTimeReplication + 
  plot_layout(widths = c(1,1.5),
              guides = 'collect',
              axis_title = 'collect') &
  theme(legend.position = 'bottom')


# Save plot
ggsave("figures/figureDescriptivesDwellTime.pdf", 
       plotDwellTimeAll, 
       width = 18,
       height = 10,
       units = "in",
       device = cairo_pdf)


# Plot fixation count -------

plotFixCount <- function(data, labels, title){
  
  fixCount_plot <- data %>%
    #filter(consumption_translation == "rating_add") %>%
    mutate(
      attribute_plot = str_replace(attribute, "f_consumption_translation(\\d)", "f_consumption\\1"),
      bar_group = ifelse(str_detect(attribute, "f_consumption_translation"), 
                         "translation", 
                         "original")
    )
  
  plot <- 
    ggplot(data = fixCount_plot,
           aes(x = attribute_plot, 
               y = fix_info, 
               fill = interaction(session, bar_group))) +
    stat_summary(
      fun = mean,
      geom = "bar",
      position = position_dodge(width = 0.6),
      width = 0.6,
      color = "black",
      linewidth = 1.2
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      position = position_dodge(width = 0.6),
      width = 0.2,
      color = "black",
      linewidth = 1.2
    ) +
    scale_fill_manual(values = scales::alpha(c(color_sessions, "white"), 0.4),
                      labels = c("1", "2", "Attribute Translation")) +
    scale_x_discrete(labels = labelsAttributesF) +
    coord_cartesian(ylim = c(0, 0.25)) +
    facet_wrap(~consumption_translation, 
               nrow = 2,
               labeller = labeller(consumption_translation = labels)) +
    labs(x = "Attribute",
         y = "Relative Fixation Count",
         title = title,
         fill = "Session") +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12,
                                    face = "bold"),  # style for facet labels
          panel.border = element_rect(color = "black", fill = NA),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
  
  return(plot)
  
}

plotFixCountOriginal <- plotFixCount(fixCountOriginal, labelsOriginal, "Original")
plotDwellTimeReplication <- plotFixCount(fixCountReplication, labelsReplication, "Replication")


# Combine plots

plotFixCountAll <- 
  plotFixCountOriginal + 
  plotDwellTimeReplication + 
  plot_layout(widths = c(1,1.5),
              guides = 'collect',
              axis_title = 'collect') &
  theme(legend.position = 'bottom')


# Save plot
ggsave("figures/figureDescriptivesFixCount.pdf", 
       plotFixCountAll, 
       width = 18,
       height = 10,
       units = "in",
       device = cairo_pdf)









