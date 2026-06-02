#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: Compare parameter estimates
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
source("R/functions/DBDA2E-utilities.R")


rm(package, packages, is_package_installed)


### Load data ------

# specify groups 

runJagsOutGroup1 <- readRDS("data/modeling/runJagsOutmaaDDMDirichlet_replication_emission_add_20260521_1608.rds")

runJagsOutGroup2 <- readRDS("data/modeling/runJagsOutmaaDDMDirichlet_replication_emission_replace_20260522_2028.rds")


# Combine chains -----

combine_chains <- function(runJagsOut){
  
  mcmcfin <- as.mcmc.list(runJagsOut)
  combinedMcmcfin <- as.data.frame(do.call(rbind, mcmcfin))
  
  return(combinedMcmcfin)
  
}

combinedMcmcfinGroup1 <-
  combine_chains(runJagsOutGroup1)

combinedMcmcfinGroup2 <-
  combine_chains(runJagsOutGroup2)


# Calculate weights ---

calculate_weights <- function(combinedMcmcfin) {
  
  # Store weights session 2
  mu_w_AT <- list()
  
  # Extract posterior samples
  mu_w_samples    <- as.matrix(combinedMcmcfin)[, c("mu_w[1]", 
                                                     "mu_w[2]", 
                                                     "mu_w[3]")]
  mu_dalr1_samples <- as.matrix(combinedMcmcfin)[, "mu_dalr1"]
  mu_dalr2_samples <- as.matrix(combinedMcmcfin)[, "mu_dalr2"]
  
  # Forward transform group mean Session 1 weights to ALR space
  mu_alr1 <- log(mu_w_samples[,1] / mu_w_samples[,3])
  mu_alr2 <- log(mu_w_samples[,2] / mu_w_samples[,3])
  
  # Add group mean change
  mu_alr1_AT <- mu_alr1 + mu_dalr1_samples
  mu_alr2_AT <- mu_alr2 + mu_dalr2_samples
  
  # Back-transform to simplex
  exp1_AT <- exp(mu_alr1_AT)
  exp2_AT <- exp(mu_alr2_AT)
  denom_AT <- 1 + exp1_AT + exp2_AT
  
  mu_w_AT$w1 <- exp1_AT / denom_AT   # group mean price weight, Session 2
  mu_w_AT$w2 <- exp2_AT / denom_AT   # group mean energy weight, Session 2
  mu_w_AT$w3 <- 1       / denom_AT   # group mean popularity weight, Session 2
  
  return(mu_w_AT)
  
}

weightsATGroup1 <- 
  calculate_weights(combinedMcmcfinGroup1)

weightsATGroup2 <- 
  calculate_weights(combinedMcmcfinGroup2)


# Calculate condition differences -----

calculate_condition_differences <- function(combinedMcmcfinGroup1,
                                            weightsATGroup1,
                                            combinedMcmcfinGroup2,
                                            weightsATGroup2) {
  
  condition_differences <- list()
  
  # calculate group differences weights 
  weight1 <- (unname(weightsATGroup2$w1) - combinedMcmcfinGroup2$`mu_w[1]`) -
    (unname(weightsATGroup1$w1) - combinedMcmcfinGroup1$`mu_w[1]`)
  weight2 <- (unname(weightsATGroup2$w2) - combinedMcmcfinGroup2$`mu_w[2]`) -
    (unname(weightsATGroup1$w2) - combinedMcmcfinGroup1$`mu_w[2]`)
  weight3 <- (unname(weightsATGroup2$w3) - combinedMcmcfinGroup2$`mu_w[3]`) -
    (unname(weightsATGroup1$w3) - combinedMcmcfinGroup1$`mu_w[3]`)
  

  # calculate group differences other
  theta <- combinedMcmcfinGroup2$mu_dtheta -
    combinedMcmcfinGroup1$mu_dtheta
  phi <- combinedMcmcfinGroup2$mu_dphi -
    combinedMcmcfinGroup1$mu_dphi
  alpha <- combinedMcmcfinGroup2$mu_dalpha -
    combinedMcmcfinGroup1$mu_dalpha
  scaling <- combinedMcmcfinGroup2$mu_dscaling -
    combinedMcmcfinGroup1$mu_dscaling 
  tau <- combinedMcmcfinGroup2$mu_dtau -
    combinedMcmcfinGroup1$mu_dtau
  sp <- combinedMcmcfinGroup2$mu_dsp -
    combinedMcmcfinGroup1$mu_dsp
  
  
  # store group differences
  condition_differences$weight1 <- HDIofMCMC(weight1)
  condition_differences$weight2 <- HDIofMCMC(weight2)
  condition_differences$weight3 <- HDIofMCMC(weight3)
  condition_differences$theta <- HDIofMCMC(theta)
  condition_differences$phi <- HDIofMCMC(phi)
  condition_differences$alpha <- HDIofMCMC(alpha)
  condition_differences$scaling <- HDIofMCMC(scaling)
  condition_differences$tau <- HDIofMCMC(tau)
  condition_differences$sp <- HDIofMCMC(sp)
  
  # return group differences
  return(condition_differences)
  
}

conditionDifferences <- calculate_condition_differences(
  combinedMcmcfinGroup1,
  weightsATGroup1,
  combinedMcmcfinGroup2,
  weightsATGroup2
)







