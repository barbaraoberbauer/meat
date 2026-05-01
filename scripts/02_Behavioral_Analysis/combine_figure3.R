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

load("figures/plots_choiceprob.RData")
load("figures/plots_rt.RData")
load("figures/plots_att.RData")


# Combine plots --------

top <- plotChoiceProbOriginal + plotChoiceProbReplication +
  plot_layout(guides = 'collect', 
              axis_titles = 'collect') &
  theme(legend.position = 'top')

left_bottom <- (plotRtOriginal / plotRtReplication +
  plot_layout(
        heights = c(4, 5),
        guides = 'collect', 
        axis_titles = "collect"
      )) &
  theme(legend.position = 'bottom')

right_bottom <- (plotAttOriginal / plotAttReplication +
  plot_layout(
    heights = c(4, 5),
    guides = 'collect', 
    axis_titles = "collect"
  )) &
  theme(legend.position = 'bottom')
  
bottom <- wrap_plots(left_bottom, right_bottom, ncol = 2)


final_plot <- (top / bottom) +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    tag_levels = list(c('a', '', 'b', '', 'c', ''))  
  ) &
  theme(
    plot.margin = margin(t = 2, r = 5, b = 2, l = 5),
    plot.tag = element_text(size = 20, face = "bold")  
  )


# Save plot ------------

ggsave("figures/figure3.pdf", 
       final_plot, 
       width = 12,
       height = 12,
       units = "in",
       device = cairo_pdf)
