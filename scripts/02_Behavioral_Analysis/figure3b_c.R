#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
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

load("data/behavioralResults.RData")

# Reverse factor levels for plot
behavioralResultsOriginal[["att"]]$consumption_translation <- 
  factor(x = behavioralResultsOriginal[["att"]]$consumption_translation,
         levels = rev(levels(behavioralResultsOriginal[["att"]]$consumption_translation)))

behavioralResultsOriginal[["rt"]]$consumption_translation <- 
  factor(x = behavioralResultsOriginal[["rt"]]$consumption_translation,
         levels = rev(levels(behavioralResultsOriginal[["rt"]]$consumption_translation)))

behavioralResultsReplication[["att"]]$consumption_translation <- 
  factor(x = behavioralResultsReplication[["att"]]$consumption_translation,
         levels = rev(levels(behavioralResultsReplication[["att"]]$consumption_translation)))

behavioralResultsReplication[["rt"]]$consumption_translation <- 
  factor(x = behavioralResultsReplication[["rt"]]$consumption_translation,
         levels = rev(levels(behavioralResultsReplication[["rt"]]$consumption_translation)))


# Plot model estimates -----

### Attention ----

fun_plot_attention <- function(data, condition_labels){
  
  plot_attention <-
    ggplot(data = data,
           mapping = aes(x = estimate, 
                         y = consumption_translation)) +
    geom_vline(xintercept = 0, 
               linetype = 'dashed', 
               size = 1.5, 
               color = "lightgrey") +
    geom_point(size = 3.5) +
    geom_errorbar(aes(xmin = asymp.LCL, 
                      xmax = asymp.UCL), 
                  width = .2, 
                  size = 1.5) +
    labs(
      x = "Effect on Relative Difference in Dwell Time",
      y = "Experimental Condition"
    ) +
    coord_cartesian(xlim = c(-0.05, 0.1)) +
    scale_y_discrete(labels = condition_labels) +
    theme(panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90")
          )
  
  return(plot_attention)
    
}

plotAttOriginal <- fun_plot_attention(behavioralResultsOriginal[["att"]],
                   labelsOriginal)

plotAttReplication <- fun_plot_attention(behavioralResultsReplication[["att"]],
                   labelsReplication)


### RT ----

fun_plot_rt <- function(data, condition_labels){
  
  plot_rt <-
  ggplot(data = data,
         mapping = aes(x = estimate, 
                       y = consumption_translation)) +
    geom_vline(xintercept = 0, 
               linetype = 'dashed', 
               size = 1.5, 
               color = "lightgrey") +
    geom_point(size = 3.5) +
    geom_errorbar(aes(xmin = asymp.LCL, 
                      xmax = asymp.UCL), 
                  width = .2, 
                  size = 1.5) +
    labs(
      x = "Effect on Response Times (in sec)",
      y = "Experimental Condition"
    ) +
    coord_cartesian(xlim = c(-3.3, 4.2)) +
    scale_y_discrete(labels = condition_labels) +
    theme(panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90")
    )
  
  return(plot_rt)
  
}

plotRtOriginal <- fun_plot_rt(behavioralResultsOriginal[["rt"]],
            labelsOriginal)

plotRtReplication <- fun_plot_rt(behavioralResultsReplication[["rt"]],
            labelsReplication)

plotRt <- 
  plotRtOriginal + 
  plotRtReplication + 
  plot_layout(ncol = 1,
              guides = 'collect',
              axis_title = 'collect') 

# Combine plots

plotAtt <- 
(
  (plotAttOriginal / plotAttReplication) +
    plot_layout(
      heights = c(4, 5),
      axis_titles = "collect"
    )
) 

plotRt <- 
  (
    (plotRtOriginal / plotRtReplication) +
      plot_layout(
        heights = c(4, 5),
        axis_titles = "collect"
      )
  )


# Save plot
save(plotAtt, plotRt, file = "figures/figure3b_c.RData")

  




