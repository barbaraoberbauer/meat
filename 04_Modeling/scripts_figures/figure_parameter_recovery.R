#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-01-23"
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
              "rjags",
              "cowplot",
              "runjags")

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
library(rjags)
library(cowplot)
library(runjags)

# Load required functions
source("functions/fun_plot_group_means_hdis.R")

rm(package, packages, is_package_installed)


### Load data ------

runJagsOut <- readRDS("data/runJagsOut_environmental_friendliness_nobounds.rds")
mcmcfin = as.mcmc.list(runJagsOut)
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))
hdi_recoveries <- readRDS("data/hdi_recoveries.rds")
true_parent_parameters <- readRDS("data/true_parent_parameters.rds")

# 1 - Ability to correctly infer group mean -----------

### Combine hdi_recoveries and true_parent_parameters -----

infer_group_means <- left_join(hdi_recoveries, 
                  true_parent_parameters,
                  join_by(sim, parameters))

### Create plots ---------

plot_price <- plot_group_means_hdis(infer_group_means, "mu_w1", "Weight Price")
plot_dprice <- plot_group_means_hdis(infer_group_means, "mu_dw1", "Weight Price\nChange")

plot_consumption <- plot_group_means_hdis(infer_group_means, "mu_w2", "Weight Consumption")
plot_dconsumption <- plot_group_means_hdis(infer_group_means, "mu_dw2", "Weight Consumption\nChange")

plot_theta <- plot_group_means_hdis(infer_group_means, "mu_theta", "Theta")
plot_dtheta <- plot_group_means_hdis(infer_group_means, "mu_dtheta", "Theta\nChange")

plot_phi <- plot_group_means_hdis(infer_group_means, "mu_phi", "Phi")
plot_dphi <- plot_group_means_hdis(infer_group_means, "mu_dphi", "Phi\nChange")

plot_alpha <- plot_group_means_hdis(infer_group_means, "mu_alpha", "Boundary Separation")
plot_dalpha <- plot_group_means_hdis(infer_group_means, "mu_dalpha", "Boundary Separation\nChange")

plot_scaling <- plot_group_means_hdis(infer_group_means, "mu_scaling", "Drift Scaling")
plot_dscaling <- plot_group_means_hdis(infer_group_means, "mu_dscaling", "Drift Scaling\nChange")

plot_tau <- plot_group_means_hdis(infer_group_means, "mu_tau", "Non-Decision Time")
plot_dtau <- plot_group_means_hdis(infer_group_means, "mu_dtau", "Non-Decision Time\nChange")

plot_sp <- plot_group_means_hdis(infer_group_means, "mu_sp", "Starting Point Bias")
plot_dsp <- plot_group_means_hdis(infer_group_means, "mu_dsp", "Starting Point Bias\nChange")

### Combine plots ------

plot_recovery_hdi <- plot_grid(plot_price, plot_dprice,
                                       plot_consumption, plot_dconsumption,
                                       plot_theta, plot_dtheta,
                                       plot_phi, plot_dphi,
                                       plot_alpha, plot_dalpha,
                                       plot_scaling, plot_dscaling,
                                       plot_tau, plot_dtau,
                                       plot_sp, plot_dsp,
                                       ncol = 2)
                                       # labels = c("a", "",
                                       #            "b", "",
                                       #            "c", "",
                                       #            "d", "",
                                       #            "e", "",
                                       #            "f", "",
                                       #            "g", "",
                                       #            "h", ""),
                                       # label_size = 20)

# Save plot 
ggsave("figures/plot_recovery_hdi.png", plot_recovery_hdi, width = 12, height = 17)




