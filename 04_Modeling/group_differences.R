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


# Load functions written by John Kruschke
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
source("functions/DBDA2E-utilities.R")


rm(package, packages, is_package_installed)


### Load data ------

runJagsOut_environmental_friendliness <- readRDS("data/runJagsOut_environmental_friendliness_nobounds.rds")
runJagsOut_control <- readRDS("data/runJagsOut_control_nobounds.rds")
runJagsOut_operating_costs <- readRDS("data/runJagsOut_operating_costs_nobounds.rds")

### Combine all chains ------

# store as mcmc object
mcmcfin_environmental_friendliness = as.mcmc.list(runJagsOut_environmental_friendliness)
mcmcfin_control = as.mcmc.list(runJagsOut_control)
mcmcfin_operating_costs = as.mcmc.list(runJagsOut_operating_costs)


# combine chains
combined_mcmcfin_environmental_friendliness <- as.data.frame(do.call(rbind, mcmcfin_environmental_friendliness))
combined_mcmcfin_control <- as.data.frame(do.call(rbind, mcmcfin_control))
combined_mcmcfin_operating_costs <- as.data.frame(do.call(rbind, mcmcfin_operating_costs))


# Calculate group differences -------

### Weights -----------

###### Rating vs. Control ------

# price
price_rc <- 
pnorm(combined_mcmcfin_environmental_friendliness$mu_dw1) -
  pnorm(combined_mcmcfin_control$mu_dw1)

HDIofMCMC(price_rc)

# consumption
consumption_rc <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dw2) -
  pnorm(combined_mcmcfin_control$mu_dw2)

HDIofMCMC(consumption_rc)

# popularity

# calculate mu_w3 (baseline) and mu_w3_AT (manipulation)
combined_mcmcfin_environmental_friendliness$mu_w3 <- 1 - 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_w1) - 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_w2)

combined_mcmcfin_environmental_friendliness$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_w1 + 
          combined_mcmcfin_environmental_friendliness$mu_dw1) - 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_w2 + 
          combined_mcmcfin_environmental_friendliness$mu_dw2)


combined_mcmcfin_control$mu_w3 <- 1 - 
  pnorm(combined_mcmcfin_control$mu_w1) - 
  pnorm(combined_mcmcfin_control$mu_w2)

combined_mcmcfin_control$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin_control$mu_w1 + 
          combined_mcmcfin_control$mu_dw1) - 
  pnorm(combined_mcmcfin_control$mu_w2 + 
          combined_mcmcfin_control$mu_dw2)

popularity_rc <- (combined_mcmcfin_environmental_friendliness$mu_w3_AT -
                    combined_mcmcfin_environmental_friendliness$mu_w3) -
  (combined_mcmcfin_control$mu_w3_AT -
     combined_mcmcfin_control$mu_w3)

HDIofMCMC(popularity_rc)


###### Rating vs. Operating Costs ------

# price
price_ro <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dw1) -
  pnorm(combined_mcmcfin_operating_costs$mu_dw1)

HDIofMCMC(price_ro)

# consumption
consumption_ro <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dw2) -
  pnorm(combined_mcmcfin_operating_costs$mu_dw2)

HDIofMCMC(consumption_ro)

# popularity

# calculate mu_w3 (baseline) and mu_w3_AT (manipulation)
combined_mcmcfin_operating_costs$mu_w3 <- 1 - 
  pnorm(combined_mcmcfin_operating_costs$mu_w1) - 
  pnorm(combined_mcmcfin_operating_costs$mu_w2)

combined_mcmcfin_operating_costs$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin_operating_costs$mu_w1 + 
          combined_mcmcfin_operating_costs$mu_dw1) - 
  pnorm(combined_mcmcfin_operating_costs$mu_w2 + 
          combined_mcmcfin_operating_costs$mu_dw2)


popularity_ro <- (combined_mcmcfin_environmental_friendliness$mu_w3_AT -
                    combined_mcmcfin_environmental_friendliness$mu_w3) -
  (combined_mcmcfin_operating_costs$mu_w3_AT -
     combined_mcmcfin_operating_costs$mu_w3)

HDIofMCMC(popularity_ro)

### Theta --------

###### Rating vs. Control ------

theta_rc <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dtheta) -
  pnorm(combined_mcmcfin_control$mu_dtheta)

HDIofMCMC(theta_rc)

###### Rating vs. Operating Costs ------

theta_ro <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dtheta) -
  pnorm(combined_mcmcfin_operating_costs$mu_dtheta)

HDIofMCMC(theta_ro)

###### Operating Costs vs. Control --------

theta_oc <- 
  pnorm(combined_mcmcfin_operating_costs$mu_dtheta) -
  pnorm(combined_mcmcfin_control$mu_dtheta)

HDIofMCMC(theta_oc)


### Alpha --------

###### Rating vs. Control ------

alpha_rc <- combined_mcmcfin_environmental_friendliness$mu_dalpha -
  combined_mcmcfin_control$mu_dalpha

HDIofMCMC(alpha_rc)


###### Rating vs. Operating Costs ------

alpha_ro <- combined_mcmcfin_environmental_friendliness$mu_dalpha -
  combined_mcmcfin_operating_costs$mu_dalpha

HDIofMCMC(alpha_ro)

###### Operating Costs vs. Control ------

alpha_oc <- combined_mcmcfin_operating_costs$mu_dalpha -
  combined_mcmcfin_control$mu_dalpha

HDIofMCMC(alpha_oc)
