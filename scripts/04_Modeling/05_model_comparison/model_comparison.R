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
              "dplyr",
              "LaplacesDemon")

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
library(LaplacesDemon)


rm(package, packages, is_package_installed)


### Load data ------

group_of_interest <- "environmental_friendliness"
# groups: "control", "emissions", "operating_costs", "environmental_friendliness"

filename_bounds <- paste0("data/runJagsOut_", group_of_interest, "_bounds",  ".rds")
filename_nobounds <- paste0("data/runJagsOut_", group_of_interest, "_nobounds",  ".rds")
  
runJagsOut_bounds <- readRDS(filename_bounds)
runJagsOut_nobounds <- readRDS(filename_nobounds)

# Calculate WAIC ----

### Bounded Model -----

mcmc_chains_bounds <- as.mcmc.list(runJagsOut_bounds)
mcmcMat_bounds <- as.matrix(mcmc_chains_bounds,chains=TRUE)
mcmc_loglik_bounds <- mcmcMat_bounds[,grep("^loglik",colnames(mcmcMat_bounds))]

waic_bounds <- WAIC(mcmc_loglik_bounds)


### Unbounded Model -----

mcmc_chains_nobounds <- as.mcmc.list(runJagsOut_nobounds)
mcmcMat_nobounds <- as.matrix(mcmc_chains_nobounds,chains=TRUE)
mcmc_loglik_nobounds <- mcmcMat_nobounds[,grep("^loglik",colnames(mcmcMat_nobounds))]

waic_nobounds <- WAIC(mcmc_loglik_nobounds)

# Compare models -------

# Does the bounded model fit the data better than the unbounded model?
waic_bounds$WAIC < waic_nobounds$WAIC



