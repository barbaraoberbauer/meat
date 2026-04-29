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

parameterEstimatesOriginal <- readRDS("figures/group_param_estimates_forest_DDM_Dirichlet_original_environmental_friendliness_20260424_0727.rds")
parameterEstimatesReplication <- readRDS("figures/group_param_estimates_forest_DDM_Dirichlet_replication_rating_add_20260422_2135.rds")

posteriorPredictivesOriginal <- readRDS("figures/posterior_predictives_ddm_dirichlet_original_environmental_friendliness_20260424_0727.rds")
posteriorPredictivesReplication <- readRDS("figures/posterior_predictives_ddm_dirichlet_replication_rating_add_20260422_2135.rds")

RhatsOriginal <- readRDS("figures/RhatsDDMDirichlet_original_environmental_friendliness_20260424_0727.rds")
RhatsReplication <- readRDS("figures/RhatsDDMDirichlet_replication_rating_add_20260422_2135.rds")


# Combine plots --------

parameterEstimatesOriginal <- parameterEstimatesOriginal + plot_annotation(title = "Original",
                                             theme = theme(plot.title = element_text(size = 18)))

parameterEstimatesReplication <- parameterEstimatesReplication + plot_annotation(title = "Replication",
                                                                           theme = theme(plot.title = element_text(size = 18)))

top <- (wrap_elements(parameterEstimatesOriginal & 
                        theme(plot.margin = margin(0,0,0,0))) +
          wrap_elements(parameterEstimatesReplication & 
                          theme(plot.margin = margin(0,0,0,0))) + 
          plot_layout(ncol = 2)) &
  theme(legend.position = 'bottom')


middle <- (wrap_elements(posteriorPredictivesOriginal & 
                           theme(plot.margin = margin(0,0,0,0))) +
             wrap_elements(posteriorPredictivesReplication & 
                             theme(plot.margin = margin(0,0,0,0))) + 
             plot_layout(ncol = 2)) &
  theme(legend.position = 'bottom')

bottom <- (wrap_elements(RhatsOriginal & 
                           theme(plot.margin = margin(0,0,0,0))) +
             wrap_elements(RhatsReplication & 
                             theme(plot.margin = margin(0,0,0,0))) + 
             plot_layout(ncol = 2))


final_plot <- (top / middle / bottom) + 
  plot_layout(heights = c(2, 1, 1))


# Save plot ------------

ggsave("figures/DDMDirichletResults.pdf", 
       final_plot, 
       width = 20,
       height = 20,
       units = "in",
       device = cairo_pdf)
