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

# Load function written by John Kruschke for diagnostics
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
source("functions/DBDA2E-utilities.R")



rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

group_of_interest <- "emissions"
# groups: "control", "emissions", "operating_costs", "environmental_friendliness"


# bounded or unbounded attentional parameters? 

bounded <- FALSE # set to TRUE for bounded model, set to FALSE for unbounded model

if (bounded == TRUE) {
  
  file_extension <- "_bounds"
  
} else if (bounded == FALSE) {
  
  file_extension <- "_nobounds"
  
}

filename <- paste0("data/runJagsOut_", group_of_interest, file_extension, ".rds")

runJagsOut <- readRDS(filename)

mcmcfin <- as.mcmc.list(runJagsOut)

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

# Visual Inspection (following Kruschke, 2014) ------

### Boundary -----

diagMCMC(codaObject = mcmcfin, parName = "mu_dalpha")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dalpha"], main="mu_dalpha", xlab=bquote(mu_dalpha))


###### Scaling ----

diagMCMC(codaObject = mcmcfin, parName = "mu_dscaling")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dscaling"], main="mu_dscaling", xlab=bquote(mu_dscaling))


###### Tau -----

diagMCMC(codaObject = mcmcfin, parName = "mu_dtau")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dtau"], main="mu_dtau", xlab=bquote(mu_dtau))


###### Theta -----

diagMCMC(codaObject = mcmcfin, parName = "mu_dtheta")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dtheta"], main="mu_dtheta", xlab=bquote(mu_dtheta))


###### Phi -----

diagMCMC(codaObject = mcmcfin, parName = "mu_dphi")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dphi"], main="mu_dphi", xlab=bquote(mu_dphi))


###### Weight Price ----

diagMCMC(codaObject = mcmcfin, parName = "mu_dw1")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dw1"], main="mu_dw1", xlab=bquote(mu_dw1))


###### Weight Energy ----

diagMCMC(codaObject = mcmcfin, parName = "mu_dw2")

openGraph(height=5, width=7)
plotPost(mcmcfin[, "mu_dw2"], main="mu_dw2", xlab=bquote(mu_dw2))




