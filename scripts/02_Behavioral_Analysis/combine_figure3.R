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

load("figures/plots_choiceprob.RData")
load("figures/plots_rt.RData")
load("figures/plots_att.RData")


# Combine plots --------


top <- plotChoiceProbOriginal + plotChoiceProbReplication +
  plot_layout(guides = 'collect', 
              axis_titles = 'collect') &
  theme(legend.position = 'top')

blank <- wrap_elements(grid::textGrob(""))

top_with_spacer <- wrap_plots(blank, top, 
                              ncol = 2,
                              widths = c(0.1, 2))

# Add row labels as left-side plot titles
original_label <- wrap_elements(
  grid::textGrob("Study 1", 
                 rot = 90,
                 #hjust = 0.5,
                 #vjust = 1,  
                 gp = grid::gpar(fontsize = 18, fontface = "bold"))
)

replication_label <- wrap_elements(
  grid::textGrob("Study 2", 
                 rot = 90, 
                 #hjust = 0.5,
                 #vjust = 1,  
                 gp = grid::gpar(fontsize = 18, fontface = "bold"))
)

# Stack labels vertically as a narrow column
label_column <- original_label / replication_label +
  plot_layout(heights = c(4, 5))  

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

# Add label column as first column in bottom
bottom <- wrap_plots(label_column, left_bottom, right_bottom, 
                     ncol = 3,
                     widths = c(0.1, 1, 1)) &
  theme(plot.margin = margin(t = 2, r = 17, b = 2, l = 17))

final_plot <- (top_with_spacer / bottom) +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    tag_levels = list(c('', 'a', '', '', '', 'b', '', 'c', ''))
  ) &
  theme(
    plot.tag = element_text(size = 20, face = "bold")
  )



# Save plot ------------

ggsave("figures/figure3.png", 
       final_plot, 
       width = 11,
       height = 10,
       units = "in")
