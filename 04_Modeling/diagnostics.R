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
              "rjags",
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
library(rjags)
library(runjags)


rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

group_of_interest <- "environmental_friendliness"
# groups: "control", "emissions", "operating_costs", "environmental_friendliness"

filename <- paste0("data/runJagsOut_", group_of_interest, ".rds")

runJagsOut <- readRDS(filename)

rm(filename)

# Diagnostics --------

# retrieve summary statistics (including HPD & Rhat/Gelman-Rubin statistic)
# HPD = highest posterior density (HPD) credible interval 
# see: https://cran.r-project.org/web/packages/runjags/runjags.pdf 
summary_statistics <- add.summary(runJagsOut,
                                  vars = c("mu_dalpha",    # boundary separation
                                           "mu_dscaling",  # scaling
                                           "mu_dtau",      # non-decision time
                                           "mu_dtheta",    # theta
                                           "mu_dphi",      # phi
                                           "mu_dw1",       # weight price
                                           "mu_dw2",       # weight sustainability
                                           "mu_dsp"))      # starting point bias

# Rhats (Gelman-Rubin statistic) --------

Rhats <- summary_statistics$psrf


# Monte Carlo Standard Error --------
# acceptable size of MCSE depends, some recommend 5%, others 6.27% 
# of its associated marginal posterior standard deviation

MCSE <- summary_statistics$mcse


# Effective Sample Size --------
# Kruschke (2014) recommends an ESS of 10,000 for accurate and stable 
# estimates of the limits of the 95% HDI

ESS <- MCSE$sseff





