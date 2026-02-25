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
              "ggplot2")

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
  
  #plot_attention <-
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
    
}

fun_plot_attention(behavioralResultsOriginal[["att"]],
                   labelsOriginal)

fun_plot_attention(behavioralResultsReplication[["att"]],
                   labelsReplication)


# TODO


### RT ----

p_rt <- 
  ggplot(data = results_rt_red,
         mapping = aes(x = estimate, y = consumption_translation)) +
  geom_vline(xintercept = 0, linetype = 'dashed', size = 1.5, color = "darkgrey") +
  geom_point(size = 3.5) +
  geom_errorbar(aes(xmin=asymp.LCL, xmax=asymp.UCL), width = .2, size = 1.5) +
  labs(
    x = "Effect on Response Times (in sec)",
    y = "Consumption Translation"
  ) +
  coord_cartesian(xlim = c(-2.5, 5)) +
  scale_y_discrete(labels = c("control" = "Control",
                              "operating_costs" = "Operating\nCosts",
                              "emissions" = "Carbon\nEmissions",
                              "environmental_friendliness" = "Rating")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14,
                                    margin = margin(t = 15, r = 0, b = 0 ,l = 0)),
        axis.title.y = element_text(size = 14,
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = margin(t = 15,
                             r = 15,
                             b = 15,
                             l = 15)
  )


# Save plots
saveRDS(p_att, "figures/p_att.rds")
saveRDS(p_rt, "figures/p_rt.rds")

  
# ### Arrange grids -----
# 
# plot_att_rt <- plot_grid(p_att,
#                          p_rt,
#                          ncol = 1,
#                          labels = c("a", "b"),
#                          label_size = 20
# )
# 
# # save plot
# ggsave("figures/plot_att_rt_me.png", plot_att_rt, width = 6, height = 9)



