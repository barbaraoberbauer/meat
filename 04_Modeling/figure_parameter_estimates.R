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
library(rjags)
library(cowplot)

# Load required functions
source("functions/fun_plot_posterior_distributions.R")
source("functions/fun_plot_change_parameters.R")

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

group_of_interest <- "environmental_friendliness"
# groups: "control", "emissions", "operating_costs", "environmental_friendliness"

filename <- paste0("data/runJagsOut_", group_of_interest, ".rds")

runJagsOut <- readRDS(filename)

rm(filename)


# Combine Chains in Data -------

# store as mcmc object
mcmcfin = as.mcmc.list(runJagsOut)

# combine chains
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))


# Plot Posterior Distributions ------

Nbins <- 60

### Boundary ------

pboundary <- plot_posterior_dist(combined_mcmcfin,
                                 combined_mcmcfin$mu_alpha,
                                 (combined_mcmcfin$mu_alpha + combined_mcmcfin$mu_dalpha),
                                 Nbins,
                                 "Boundary Separation")

### Scaling ------

pscaling <- plot_posterior_dist(combined_mcmcfin,
                                combined_mcmcfin$mu_scaling,
                                (combined_mcmcfin$mu_scaling + combined_mcmcfin$mu_dscaling),
                                Nbins,
                                "Drift Scaling")

### Tau ------

ptau <- plot_posterior_dist(combined_mcmcfin,
                            combined_mcmcfin$mu_tau,
                            (combined_mcmcfin$mu_tau + combined_mcmcfin$mu_dtau),
                            Nbins,
                            "Non-Decision Time")


### Theta ------

# unbounded parameters
ptheta <- plot_posterior_dist(combined_mcmcfin,
                              combined_mcmcfin$mu_theta,
                              (combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta),
                              Nbins,
                              "Discounting Unattended Option")


### Phi ------

# unbounded parameters
pphi <- plot_posterior_dist(combined_mcmcfin,
                            combined_mcmcfin$mu_phi,
                            (combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi),
                            Nbins,
                            "Discounting Unattended Attribute")


### Weight Price ------

pprice <- plot_posterior_dist(combined_mcmcfin, 
                              pnorm(combined_mcmcfin$mu_w1), 
                              pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1), 
                              Nbins, 
                              "Weight Price")


### Weight Energy ------

penergy <- plot_posterior_dist(combined_mcmcfin, 
                               pnorm(combined_mcmcfin$mu_w2), 
                               pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2), 
                               Nbins, 
                               "Weight Sustainability")


### Weight Popularity ------

combined_mcmcfin$mu_w3 <- 1 - pnorm(combined_mcmcfin$mu_w1) - pnorm(combined_mcmcfin$mu_w2)
combined_mcmcfin$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1) - 
  pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2)


ppopularity <- plot_posterior_dist(combined_mcmcfin,
                                   combined_mcmcfin$mu_w3, 
                                   combined_mcmcfin$mu_w3_AT, 
                                   Nbins, 
                                   "Weight Popularity")


### Starting point -------

psp <- plot_posterior_dist(combined_mcmcfin,
                           combined_mcmcfin$mu_sp,
                           (combined_mcmcfin$mu_sp + combined_mcmcfin$mu_dsp),
                           Nbins,
                           "Starting Point Bias")


# Plot change parameters -------

# d stands for change (difference)

### Boundary -------

pdBoundary <- plot_change_param(combined_mcmcfin,
                                combined_mcmcfin$mu_dalpha,
                                Nbins)

### Scaling ------

pdScaling <- plot_change_param(combined_mcmcfin,
                               combined_mcmcfin$mu_dscaling,
                               Nbins)

### Tau ------

pdTau <- plot_change_param(combined_mcmcfin,
                           combined_mcmcfin$mu_dtau,
                           Nbins)

### Theta ------

# unbounded
pdTheta <- plot_change_param(combined_mcmcfin,
                             combined_mcmcfin$mu_dtheta,
                             Nbins)


### Phi ------

# unbounded
pdPhi <- plot_change_param(combined_mcmcfin,
                           combined_mcmcfin$mu_dphi,
                           Nbins)


### Weight Price ------

# plot netto difference instead of phi-transformed change parameter
pdPrice <- plot_change_param(combined_mcmcfin,
                             pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1) -
                               pnorm(combined_mcmcfin$mu_w1),
                             Nbins)


### Weight Energy ------

# plot netto difference instead of phi-transformed change parameter
pdEnergy <- plot_change_param(combined_mcmcfin,
                              pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2) -
                                pnorm(combined_mcmcfin$mu_w2),
                              Nbins)


### Weight Popularity ------

pdPopularity <- plot_change_param(combined_mcmcfin,
                                  (combined_mcmcfin$mu_w3_AT -  
                                     combined_mcmcfin$mu_w3),
                                  Nbins)


### Starting point bias ----

pdsp <- plot_change_param(combined_mcmcfin,
                          combined_mcmcfin$mu_dsp,
                          Nbins)



# Arrange plots --------

group_param_estimates <- plot_grid(# plots
  pprice,
  pdPrice,
  penergy,
  pdEnergy,
  ppopularity,
  pdPopularity,
  pboundary,
  pdBoundary,
  ptheta,
  pdTheta,
  pphi,
  pdPhi,
  ptau,
  pdTau,
  pscaling,
  pdScaling,
  psp,
  pdsp,
  
  # settings
  ncol = 4,
  labels = c("a", "",
             "b", "",
             "c", "",
             "d", "",
             "e", "",
             "f", "",
             "g", "",
             "h", "",
             "i", ""
  )
)


# save plot
ggsave("figures/group_param_estimates.png", group_param_estimates, width = 16, height = 12)



