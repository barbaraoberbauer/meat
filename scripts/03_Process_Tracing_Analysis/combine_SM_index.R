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
              "patchwork",
              "grid")

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
library(grid)

rm(package, packages, is_package_installed)


### Load plots ---------

load("figures/figureSMIndexChoice.RData")
load("figures/figureSMIndexAttributeWeights.RData")


# Combine plots --------

upper_plot <- figureSMIndexChoice

lower_plot <- finalPlot + plot_layout(heights = c(1, 1, 1))

plot <- (free(upper_plot) / wrap_elements(finalPlot)) +
  plot_layout(heights = c(1, 3)) +
  plot_annotation(
    tag_levels = list(c('a', 'b'))
  ) &
  theme(plot.tag = element_text(size = 20, face = "bold"))


# Save plot ------------

ggsave("figures/figureSMIndexCorrelations.png", 
       plot, 
       width = 10,
       height = 12,
       units = "in")
