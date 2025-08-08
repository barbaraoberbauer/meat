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
              "parallel",
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
library(parallel)

# Load functions written by John Kruschke
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
source("functions/DBDA2E-utilities.R")
source("functions/fun_parameter_recovery_hdis.R")
source("functions/fun_parameter_recovery_subjectParameters.R")

# Load required modules
load.module("wiener")

rm(package, packages, is_package_installed)


### Load data ------

runJagsOut <- readRDS("data/runJagsOut_environmental_friendliness_nobounds.rds")
runJagsOut_recovery1 <- readRDS("data/runJagsOut_recovery1_environmental_friendliness_nobounds.rds")
runJagsOut_recovery2 <- readRDS("data/runJagsOut_recovery2_environmental_friendliness_nobounds.rds")
runJagsOut_recovery3 <- readRDS("data/runJagsOut_recovery3_environmental_friendliness_nobounds.rds")
runJagsOut_recovery4 <- readRDS("data/runJagsOut_recovery4_environmental_friendliness_nobounds.rds")
runJagsOut_recovery5 <- readRDS("data/runJagsOut_recovery5_environmental_friendliness_nobounds.rds")
runJagsOut_recovery6 <- readRDS("data/runJagsOut_recovery6_environmental_friendliness_nobounds.rds")
runJagsOut_recovery7 <- readRDS("data/runJagsOut_recovery7_environmental_friendliness_nobounds.rds")
runJagsOut_recovery8 <- readRDS("data/runJagsOut_recovery8_environmental_friendliness_nobounds.rds")
runJagsOut_recovery9 <- readRDS("data/runJagsOut_recovery9_environmental_friendliness_nobounds.rds")
runJagsOut_recovery10 <- readRDS("data/runJagsOut_recovery10_environmental_friendliness_nobounds.rds")

### Store as MCMC objects and combine chains --------

mcmcfin = as.mcmc.list(runJagsOut)
mcmcfin_recovery1 = as.mcmc.list(runJagsOut_recovery1)
mcmcfin_recovery2 = as.mcmc.list(runJagsOut_recovery2)
mcmcfin_recovery3 = as.mcmc.list(runJagsOut_recovery3)
mcmcfin_recovery4 = as.mcmc.list(runJagsOut_recovery4)
mcmcfin_recovery5 = as.mcmc.list(runJagsOut_recovery5)
mcmcfin_recovery6 = as.mcmc.list(runJagsOut_recovery6)
mcmcfin_recovery7 = as.mcmc.list(runJagsOut_recovery7)
mcmcfin_recovery8 = as.mcmc.list(runJagsOut_recovery8)
mcmcfin_recovery9 = as.mcmc.list(runJagsOut_recovery9)
mcmcfin_recovery10 = as.mcmc.list(runJagsOut_recovery10)

combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))
combined_mcmcfin_recovery1 <- as.data.frame(do.call(rbind, mcmcfin_recovery1))
combined_mcmcfin_recovery2 <- as.data.frame(do.call(rbind, mcmcfin_recovery2))
combined_mcmcfin_recovery3 <- as.data.frame(do.call(rbind, mcmcfin_recovery3))
combined_mcmcfin_recovery4 <- as.data.frame(do.call(rbind, mcmcfin_recovery4))
combined_mcmcfin_recovery5 <- as.data.frame(do.call(rbind, mcmcfin_recovery5))
combined_mcmcfin_recovery6 <- as.data.frame(do.call(rbind, mcmcfin_recovery6))
combined_mcmcfin_recovery7 <- as.data.frame(do.call(rbind, mcmcfin_recovery7))
combined_mcmcfin_recovery8 <- as.data.frame(do.call(rbind, mcmcfin_recovery8))
combined_mcmcfin_recovery9 <- as.data.frame(do.call(rbind, mcmcfin_recovery9))
combined_mcmcfin_recovery10 <- as.data.frame(do.call(rbind, mcmcfin_recovery10))

rm(runJagsOut_recovery1,
   runJagsOut_recovery2,
   runJagsOut_recovery3,
   runJagsOut_recovery4,
   runJagsOut_recovery5,
   runJagsOut_recovery6,
   runJagsOut_recovery7,
   runJagsOut_recovery8,
   runJagsOut_recovery9,
   runJagsOut_recovery10,
   mcmcfin_recovery1,
   mcmcfin_recovery2,
   mcmcfin_recovery3,
   mcmcfin_recovery4,
   mcmcfin_recovery5,
   mcmcfin_recovery6,
   mcmcfin_recovery7,
   mcmcfin_recovery8,
   mcmcfin_recovery9,
   mcmcfin_recovery10)


# 1 - Ability to correctly infer group mean -----------

### Get Generating Parent Parameters ------

mcmcMat <- as.matrix(mcmcfin,chains=TRUE)
mcmc_loglik <- mcmcMat[,grep("^loglik",colnames(mcmcMat))]

loglik <- rowSums(mcmc_loglik)

# sort loglik to determine most likely values
loglik_sorted <- sort(loglik, decreasing = TRUE)

# get positions of most likely parameters
idx <- sapply(loglik_sorted[1:10], 
              function(val) which(loglik == val))

# Define relevant columns
cols_to_keep <- c("mu_w1", "mu_dw1",
                  "mu_w2", "mu_dw2",
                  "mu_theta", "mu_dtheta",
                  "mu_phi", "mu_dphi",
                  "mu_alpha", "mu_dalpha",
                  "mu_scaling", "mu_dscaling",
                  "mu_tau", "mu_dtau",
                  "mu_sp", "mu_dsp")


# Get generating parameters
true_parent_parameters <- combined_mcmcfin[idx, cols_to_keep]
true_parent_parameters$sim <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Shape into long format
true_parent_parameters <- true_parent_parameters %>%
  pivot_longer(cols = -sim,
               names_to = "parameter",
               values_to = "generating_value")

true_parent_parameters <- as.data.frame(true_parent_parameters)

### Get HDIs of recoveries -------

hdi_recovery1 <- parameter_recovery_hdis(combined_mcmcfin_recovery1, 1)
hdi_recovery2 <- parameter_recovery_hdis(combined_mcmcfin_recovery2, 2)
hdi_recovery3 <- parameter_recovery_hdis(combined_mcmcfin_recovery3, 3)
hdi_recovery4 <- parameter_recovery_hdis(combined_mcmcfin_recovery4, 4)
hdi_recovery5 <- parameter_recovery_hdis(combined_mcmcfin_recovery5, 5)
hdi_recovery6 <- parameter_recovery_hdis(combined_mcmcfin_recovery6, 6)
hdi_recovery7 <- parameter_recovery_hdis(combined_mcmcfin_recovery7, 7)
hdi_recovery8 <- parameter_recovery_hdis(combined_mcmcfin_recovery8, 8)
hdi_recovery9 <- parameter_recovery_hdis(combined_mcmcfin_recovery9, 9)
hdi_recovery10 <- parameter_recovery_hdis(combined_mcmcfin_recovery10, 10)

hdi_recoveries <- rbind(hdi_recovery1,
                        hdi_recovery2,
                        hdi_recovery3,
                        hdi_recovery4,
                        hdi_recovery5,
                        hdi_recovery6,
                        hdi_recovery7,
                        hdi_recovery8,
                        hdi_recovery9,
                        hdi_recovery10)


# save data
saveRDS(true_parent_parameters, file = "data/true_parent_parameters.rds")
saveRDS(hdi_recoveries, file = "data/hdi_recoveries.rds")


rm(hdi_recovery1,
   hdi_recovery2,
   hdi_recovery3,
   hdi_recovery4,
   hdi_recovery5,
   hdi_recovery6,
   hdi_recovery7,
   hdi_recovery8,
   hdi_recovery9,
   hdi_recovery10)


# 2 - Ability to correctly infer individual parameters -------

### Get Generating Subject Parameters --------

# Select columns that match any pattern
cols_to_keep <- grepl(pattern, colnames(combined_mcmcfin))

# Subset the data frame
true_subject_parameters <- combined_mcmcfin[idx, cols_to_keep]
true_subject_parameters$sim <- c(1,2,3,4,5,6,7,8,9,10)

# Shape into long format
true_subject_parameters <- true_subject_parameters %>%
  pivot_longer(
    !sim,
    names_to = "parameter_subject",
    values_to = "generating_value"
  )

true_subject_parameters <- true_subject_parameters %>%
  mutate(
    parameter = str_extract(parameter_subject, "^\\w+"),
    subject = str_extract(parameter_subject, "\\d+(?=\\])") %>% 
      as.integer()
  ) %>%
  select(-parameter_subject)

### Get subject parameters of recoveries -------

# Define the prefixes
patterns <- c(
  "^w1T\\[", "^dw1\\[",
  "^w2T\\[", "^dw2\\[",
  "^thetaT\\[", "^dtheta\\[",
  "^phiT\\[", "^dphi\\[",
  "^alpha\\[", "^dalpha\\[",
  "^scaling\\[", "^dscaling\\[",
  "^tau\\[", "^dtau\\[",
  "^sp\\[", "^dsp\\["
)

# Create a combined regex pattern
pattern <- paste(patterns, collapse = "|")

# Get subject parameters
subjParameters_recovery1 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery1, pattern, 1)
subjParameters_recovery2 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery2, pattern, 2)
subjParameters_recovery3 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery3, pattern, 3)
subjParameters_recovery4 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery4, pattern, 4)
subjParameters_recovery5 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery5, pattern, 5)
subjParameters_recovery6 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery6, pattern, 6)
subjParameters_recovery7 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery7, pattern, 7)
subjParameters_recovery8 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery8, pattern, 8)
subjParameters_recovery9 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery9, pattern, 9)
subjParameters_recovery10 <- parameter_recovery_subjectParameters(combined_mcmcfin_recovery10, pattern, 10)

subjParameters_recoveries <- rbind(subjParameters_recovery1,
                                  subjParameters_recovery2,
                                  subjParameters_recovery3,
                                  subjParameters_recovery4,
                                  subjParameters_recovery5,
                                  subjParameters_recovery6,
                                  subjParameters_recovery7,
                                  subjParameters_recovery8,
                                  subjParameters_recovery9,
                                  subjParameters_recovery10)

rm(subjParameters_recovery1,
   subjParameters_recovery2,
   subjParameters_recovery3,
   subjParameters_recovery4,
   subjParameters_recovery5,
   subjParameters_recovery6,
   subjParameters_recovery7,
   subjParameters_recovery8,
   subjParameters_recovery9,
   subjParameters_recovery10)

# save data
saveRDS(subjParameters_recoveries, file = "data/subjParameters_recoveries.rds")
saveRDS(true_subject_parameters, file = "data/true_subject_parameters.rds")
