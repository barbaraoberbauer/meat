#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-05-10"
# produced under R version: 2024.09.0
#---

# plot group differences for consumption weight, theta, boundary separation

# Load packages and read data ------

### Clear environment -------

#clear working environment
rm(list=ls())

#clear all plots
if(!is.null(dev.list())) dev.off()

### Install packages -------

# List of packages to check and install if necessary
packages <- c("tidyverse",
              "runjags",
              "dplyr")

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
library(runjags)
library(dplyr)


# Load functions for plotting
source("functions/fun_plot_posterior_distributions.R")
source("functions/fun_plot_change_parameters.R")

rm(package, packages, is_package_installed)


### Load data ------

runJagsOut_environmental_friendliness <- readRDS("data/runJagsOut_environmental_friendliness_nobounds.rds")
runJagsOut_control <- readRDS("data/runJagsOut_control_nobounds.rds")
runJagsOut_emissions <- readRDS("data/runJagsOut_emissions_nobounds.rds")

### Combine all chains ------

# store as mcmc object
mcmcfin_environmental_friendliness = as.mcmc.list(runJagsOut_environmental_friendliness)
mcmcfin_control = as.mcmc.list(runJagsOut_control)
mcmcfin_emissions = as.mcmc.list(runJagsOut_emissions)


# combine chains
combined_mcmcfin_environmental_friendliness <- as.data.frame(do.call(rbind, mcmcfin_environmental_friendliness))
combined_mcmcfin_control <- as.data.frame(do.call(rbind, mcmcfin_control))
combined_mcmcfin_emissions <- as.data.frame(do.call(rbind, mcmcfin_emissions))


# Calculate group differences -------

### Weight consumption ---------

# rating vs. control group
consumption_rc <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dw2) -
  pnorm(combined_mcmcfin_control$mu_dw2)

# rating vs. carbon emission group
consumption_re <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dw2) -
  pnorm(combined_mcmcfin_emissions$mu_dw2)

### Theta -------

# rating vs. control group
theta_rc <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dtheta) -
  pnorm(combined_mcmcfin_control$mu_dtheta)

# rating vs. carbon emission group
theta_re <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dtheta) -
  pnorm(combined_mcmcfin_emissions$mu_dtheta)

### Boundary separation -------

# rating vs. control group
alpha_rc <- combined_mcmcfin_environmental_friendliness$mu_dalpha -
  combined_mcmcfin_control$mu_dalpha

# carbon emissions vs. control group
alpha_ec <- combined_mcmcfin_emissions$mu_dalpha -
  combined_mcmcfin_control$mu_dalpha


# Store parameters together -------

boundary <- data.frame(rating = 
                         combined_mcmcfin_environmental_friendliness$mu_dalpha,
                       control =
                         combined_mcmcfin_control$mu_dalpha,
                       emissions =
                         combined_mcmcfin_emissions$mu_dalpha,
                       alpha_rc = alpha_rc,
                       alpha_ec = alpha_ec
                       )

theta <- data.frame(rating = 
                      combined_mcmcfin_environmental_friendliness$mu_dtheta,
                    control =
                      combined_mcmcfin_control$mu_dtheta,
                    emissions =
                      combined_mcmcfin_emissions$mu_dtheta,
                    theta_rc = theta_rc,
                    theta_re = theta_re
                    )

consumption <- data.frame(rating = 
                            pnorm(combined_mcmcfin_environmental_friendliness$mu_w2 + combined_mcmcfin_environmental_friendliness$mu_dw2) -
                            pnorm(combined_mcmcfin_environmental_friendliness$mu_w2),
                          control = 
                            pnorm(combined_mcmcfin_control$mu_w2 + combined_mcmcfin_control$mu_dw2) -
                            pnorm(combined_mcmcfin_control$mu_w2),
                          emissions = 
                            pnorm(combined_mcmcfin_emissions$mu_w2 + combined_mcmcfin_emissions$mu_dw2) -
                            pnorm(combined_mcmcfin_emissions$mu_w2),
                          consumption_rc = consumption_rc,
                          consumption_re = consumption_re
                          )






plot_posterior_dist(boundary,
                    boundary$rating,
                    boundary$control,
                    25,
                    "Boundary")


plot_posterior_dist(theta,
                    theta$rating,
                    theta$control,
                    25,
                    "Theta")


plot_posterior_dist(consumption,
                    consumption$rating,
                    consumption$control,
                    25,
                    "Consumption")

