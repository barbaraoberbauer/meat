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


rm(package, packages, is_package_installed)


### Load plots ---------

load("figures/figure3a.RData")
load("figures/figure3b_c.RData")


# Combine plots --------

top <- 
  wrap_elements(plotChoiceProb) +
  labs(tag = "a")

left_bottom <-
  wrap_elements(plotAtt) +
  labs(tag = "b")

right_bottom <-
  wrap_elements(plotRt) +
  labs(tag = "c")

bottom <- left_bottom | right_bottom

final_plot <- (top / bottom) &
  theme(
    plot.margin = margin(t = 2,
                         r = 5,
                         b = 2,
                         l = 5)
  )


# Save plot ------------

ggsave("figures/figure3.pdf", 
       final_plot, 
       width = 12,
       height = 12,
       units = "in",
       device = cairo_pdf)
