#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-02-17"
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

# Load required functions
source("functions/fun_plot_model_estimates.R")
source("functions/fun_nudgedodge.R")


rm(package, packages, is_package_installed)


### Load data ---------

results_att_rt <- readRDS("data/results_att_rt.rds")

results_att <- as.data.frame(results_att_rt$results_att)
results_att_red <- as.data.frame(results_att_rt$results_att_red)
results_rt <- as.data.frame(results_att_rt$results_rt)
results_rt_red <- as.data.frame(results_att_rt$results_rt_red)


# Check data structure -------

str(results_att)
str(results_att_red)
str(results_rt)
str(results_rt_red)

### reverse factor levels for plot ----

results_att$consumption_translation <- factor(x = results_att$consumption_translation,
                                              levels = rev(levels(results_att$consumption_translation)))

results_att$price_translation <- factor(x = results_att$price_translation,
                                        levels = rev(levels(results_att$price_translation)))

results_att_red$consumption_translation <- factor(x = results_att_red$consumption_translation,
                                              levels = rev(levels(results_att_red$consumption_translation)))

results_rt$consumption_translation <- factor(x = results_rt$consumption_translation,
                                              levels = rev(levels(results_rt$consumption_translation)))

results_rt$price_translation <- factor(x = results_rt$price_translation,
                                        levels = rev(levels(results_rt$price_translation)))

results_rt_red$consumption_translation <- factor(x = results_rt_red$consumption_translation,
                                             levels = rev(levels(results_rt_red$consumption_translation)))


# Plot model estimates -----

color_price <- c("#2D3142", "#BFC0C0") # present, absent

### Attention ----

p_att <- plot_model_estimates(results_att,
                              results_att_red,
                              "Effect on Relative Difference in Dwell Time",
                              c(-0.05, 0.1)
                              )

### RT ----

p_rt <- plot_model_estimates(results_rt,
                             results_rt_red,
                              "Effect on Response Times (in sec)",
                              c(-3, 6)
)

  
### Arrange grids -----

plot_att_rt <- plot_grid(p_att,
                         p_rt,
                         ncol = 2,
                         labels = c("a", "b")
)

# save plot
ggsave("figures/plot_att_rt.png", plot_att_rt, width = 9, height = 4.5)



