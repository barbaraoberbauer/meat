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


###### Rating vs. Carbon Emissions ------

# price
price_re <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dw1) -
  pnorm(combined_mcmcfin_emissions$mu_dw1)

HDIofMCMC(price_re)

# consumption
consumption_re <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dw2) -
  pnorm(combined_mcmcfin_emissions$mu_dw2)

HDIofMCMC(consumption_re)

# popularity

# calculate mu_w3 (baseline) and mu_w3_AT (manipulation)
combined_mcmcfin_emissions$mu_w3 <- 1 - 
  pnorm(combined_mcmcfin_emissions$mu_w1) - 
  pnorm(combined_mcmcfin_emissions$mu_w2)

combined_mcmcfin_emissions$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin_emissions$mu_w1 + 
          combined_mcmcfin_emissions$mu_dw1) - 
  pnorm(combined_mcmcfin_emissions$mu_w2 + 
          combined_mcmcfin_emissions$mu_dw2)


popularity_re <- (combined_mcmcfin_environmental_friendliness$mu_w3_AT -
                    combined_mcmcfin_environmental_friendliness$mu_w3) -
  (combined_mcmcfin_emissions$mu_w3_AT -
     combined_mcmcfin_emissions$mu_w3)

HDIofMCMC(popularity_re)

### Theta --------

###### Rating vs. Control ------

theta_rc <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dtheta) -
  pnorm(combined_mcmcfin_control$mu_dtheta)

HDIofMCMC(theta_rc)

###### Rating vs. Carbon Emissions ------

theta_re <- 
  pnorm(combined_mcmcfin_environmental_friendliness$mu_dtheta) -
  pnorm(combined_mcmcfin_emissions$mu_dtheta)

HDIofMCMC(theta_re)

###### Carbon Emissions vs. Control --------

theta_ec <- 
  pnorm(combined_mcmcfin_emissions$mu_dtheta) -
  pnorm(combined_mcmcfin_control$mu_dtheta)

HDIofMCMC(theta_ec)


### Alpha --------

###### Rating vs. Control ------

alpha_rc <- combined_mcmcfin_environmental_friendliness$mu_dalpha -
  combined_mcmcfin_control$mu_dalpha

HDIofMCMC(alpha_rc)


###### Rating vs. Carbon Emissions ------

alpha_re <- combined_mcmcfin_environmental_friendliness$mu_dalpha -
  combined_mcmcfin_emissions$mu_dalpha

HDIofMCMC(alpha_re)

###### Carbon Emissions vs. Control ------

alpha_ec <- combined_mcmcfin_emissions$mu_dalpha -
  combined_mcmcfin_control$mu_dalpha

HDIofMCMC(alpha_ec)
