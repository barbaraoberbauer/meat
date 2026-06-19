#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: compare phi and 2phi
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
filename1Phi <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", "original", "_", "environmental_friendliness", "_", "20260519_0532", ".rds")

filename2Phi <- paste0("data/modeling/runJagsOutmaaDDM2phiDirichlet_original_environmental_friendliness_20260619_0816.rds")

runJagsOut1Phi <- readRDS(filename1Phi)
runJagsOut2Phi <- readRDS(filename2Phi)


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

waic1Phi <- calculate_waic(runJagsOut1Phi)  
waic2Phi <- calculate_waic(runJagsOut2Phi)  

# Compare models -------

# Does the single phi model fit the data better than the multiple phi?
waic1Phi$WAIC < waic2Phi$WAIC

# Delta WAIC
deltaWAIC <- waic1Phi$WAIC - waic2Phi$WAIC


