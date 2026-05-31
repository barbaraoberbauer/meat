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

# original

filenameDDMOriginal <- paste0("data/modeling/runJagsOutDDMDirichlet", "_", "original", "_", "environmental_friendliness", "_", "20260517_1131", ".rds")

filenamemaaDDMOriginal <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", "original", "_", "environmental_friendliness", "_", "20260519_0532", ".rds")

runJagsOutDDMOriginal <- readRDS(filenameDDMOriginal)
runJagsOutmaaDDMOriginal <- readRDS(filenamemaaDDMOriginal)

# replication

filenameDDMReplication <- paste0("data/modeling/runJagsOutDDMDirichlet", "_", "replication", "_", "rating_add", "_", "20260517_0459", ".rds")

filenamemaaDDMReplication <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", "replication", "_", "rating_add", "_", "20260518_2319", ".rds")

runJagsOutDDMReplication <- readRDS(filenameDDMReplication)
runJagsOutmaaDDMReplication <- readRDS(filenamemaaDDMReplication)

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

waicDDMOriginal <- calculate_waic(runJagsOutDDMOriginal)  
waicmaaDDMOriginal <- calculate_waic(runJagsOutmaaDDMOriginal)  

waicDDMReplication <- calculate_waic(runJagsOutDDMReplication)  
waicmaaDDMReplication <- calculate_waic(runJagsOutmaaDDMReplication) 

# Compare models -------

# Does the maaDDM fit the data better than the DDM?
waicmaaDDMOriginal$WAIC < waicDDMOriginal$WAIC
waicmaaDDMReplication$WAIC < waicDDMReplication$WAIC

# Delta WAIC
deltaWAICoriginal <- waicmaaDDMOriginal$WAIC - waicDDMOriginal$WAIC
deltaWAICreplication <- waicmaaDDMReplication$WAIC - waicDDMReplication$WAIC


