#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: compare DDM and maaDDM
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
              "LaplacesDemon",
              "coda")

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
library(coda)


rm(package, packages, is_package_installed)


### Load data ------

# unbounded model
filenameUnbounded <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", "original", "_", "environmental_friendliness", "_", "20260519_0532", ".rds")

filenameBounded <- paste0("data/modeling/runJagsOutmaaDDMDirichlet_original_environmental_friendliness_bounded_20260617_1338.rds")

runJagsOutUnbounded <- readRDS(filenameUnbounded)
runJagsOutBounded <- readRDS(filenameBounded)


# Calculate WAIC ----

calculate_waic <- function(runJagsOut) {
  
  mcmc_chains <- as.mcmc.list(runJagsOut)
  mcmcMat <- as.matrix(mcmc_chains, 
                       chains = TRUE)
  mcmc_loglik <- mcmcMat[,grep("^loglik",
                               colnames(mcmcMat))]
  
  waic <- WAIC(mcmc_loglik)
  
  return(waic)
  
}

waicUnbounded <- calculate_waic(runJagsOutUnbounded)  
waicBounded <- calculate_waic(runJagsOutBounded)  

# Compare models -------

# Does the unbounded model fit the data better than the bounded maaDDM?
waicUnbounded$WAIC < waicBounded$WAIC

# Delta WAIC
deltaWAIC <- waicUnbounded$WAIC - waicBounded$WAIC


