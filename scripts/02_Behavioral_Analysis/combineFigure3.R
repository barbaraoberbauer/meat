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


### Load plots ---------

fig_choice_prob <- readRDS("figures/choice_probability_me.rds")
p_att <- readRDS("figures/p_att.rds")
p_rt <- readRDS("figures/p_rt.rds")


# Combine plots --------

plot_att_rt <- plot_grid(p_att, p_rt,
                         ncol = 1,
                         labels = c("b", "c"),
                         label_size = 20,
                         hjust = 0.5)

plot_choice_prob <- plot_grid(fig_choice_prob,
                              ncol = 1,
                              labels = "a",
                              label_size = 20)


plot_behavior <- plot_grid(plot_choice_prob,
                           plot_att_rt,
                           ncol = 2)

# Save plot ------------

ggsave("figures/plot_behavior.png", plot_behavior, width = 10, height = 6)
