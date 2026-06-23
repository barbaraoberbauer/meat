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
source("R/functions/DBDA2E-utilities.R")
source("R/functions/fun_parameter_recovery_hdis.R")
source("R/functions/fun_parameter_recovery_subjectParameters.R")

# Load required modules
load.module("wiener")

rm(package, packages, is_package_installed)


### Load data ------

# specify subset of data 

# datasets: "original", "replication"
dataset <- "original"

# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace"
translation_of_interest <- "environmental_friendliness"

# time stamp of data generation
time <- "20260519_0532"

# load data

filename <- paste0("data/modeling/runJagsOutmaaDDMDirichlet", "_", dataset, "_", translation_of_interest, "_", time, ".rds")
runJagsOut <- readRDS(filename)

runJagsOut_recovery <- lapply(1:10, function(i) {
  readRDS(paste0("data/recovery/runJagsOutmaaDDM_recovery", i, "_",
                 dataset, "_", translation_of_interest, "_", time, ".rds"))
})

### Store as MCMC objects and combine chains --------

mcmcfin = as.mcmc.list(runJagsOut)
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))

# recoveries
mcmcfin_recovery <- lapply(runJagsOut_recovery, as.mcmc.list)

combined_mcmcfin_recovery <- lapply(mcmcfin_recovery, function(x) {
  as.data.frame(do.call(rbind, x))
})

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
cols_to_keep <- c("mu_w[1]", "mu_w[2]", "mu_w[3]",
                  "mu_dalr1", "mu_dalr2",
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

hdi_recovery <- lapply(seq_along(combined_mcmcfin_recovery), function(i) {
  parameter_recovery_hdis(combined_mcmcfin_recovery[[i]], i)
})

hdi_recoveries <- do.call(rbind, hdi_recovery)


# 2 - Ability to correctly infer individual parameters -------

### Get Generating Subject Parameters --------

# Define the prefixes
pattern <- c(
  "^wT\\[",
  "^theta\\[", "^dtheta\\[",
  "^phi\\[", "^dphi\\[",
  "^alpha\\[", "^dalpha\\[",
  "^scaling\\[", "^dscaling\\[",
  "^tau\\[", "^dtau\\[",
  "^sp\\[", "^dsp\\["
)

# Create a combined regex pattern
pattern <- paste(pattern, collapse = "|")

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
    parameter = case_when(
      str_detect(parameter_subject, "^d?wT\\[") ~
        paste0(str_extract(parameter_subject, "^d?wT"),
               str_extract(str_extract(parameter_subject, ",\\s*(\\d+)\\]"), "\\d+")),
      TRUE ~ str_extract(parameter_subject, "^\\w+")
    ),
    subject = as.integer(case_when(
      str_detect(parameter_subject, "^d?wT\\[") ~
        str_extract(parameter_subject, "(?<=\\[)\\d+"),
      TRUE ~
        str_extract(parameter_subject, "\\d+(?=\\])")
    ))
  ) %>%
  dplyr::select(-parameter_subject)

### Get subject parameters of recoveries -------

# Get subject parameters

subjParameters_recovery <- lapply(seq_along(combined_mcmcfin_recovery), function(i) {
  parameter_recovery_subjectParameters(combined_mcmcfin_recovery[[i]], pattern, i)
})

subjParameters_recoveries <- do.call(rbind, subjParameters_recovery)

# 3 - Correlations between Parent Parameters ---------

### Get parent parameters ---------

# Write function that gets parent parameters
getParents <- function(combined_mcmcfin, sim){
  # keep only parent parameters
  cols_to_keep <- c("mu_w[1]",
                    "mu_w[2]",
                    "mu_w[3]",
                    "mu_dalr1",
                    "mu_dalr2",
                    "mu_theta",
                    "mu_dtheta",
                    "mu_phi",
                    "mu_dphi",
                    "mu_alpha",
                    "mu_dalpha",
                    "mu_scaling",
                    "mu_dscaling",
                    "mu_tau",
                    "mu_dtau",
                    "mu_sp",
                    "mu_dsp")
  parent_parameters <- combined_mcmcfin[, cols_to_keep]
  # remove row names
  rownames(parent_parameters) <- NULL
  return(parent_parameters)
}

parents_recovery <- lapply(seq_along(combined_mcmcfin_recovery), function(i) {
  getParents(combined_mcmcfin_recovery[[i]], i)
})

### Calculate correlations -------

cor_recovery <- lapply(parents_recovery, function(x) {
  cor(x, use = "pairwise.complete.obs")
})


### Calculate mean and sd across simulations -----
params <- colnames(cor_recovery[[1]]) # parameter names
n_params <- length(params)

cor_recoveries <- array(NA, dim = c(n_params, n_params, length(cor_recovery)),
                        dimnames = list(params, params, NULL))

# Fill array
for (i in seq_along(cor_recovery)) {
  cor_recoveries[, , i] <- cor_recovery[[i]]
}

# Compute mean and sd across third dimension (simulations)
mean_cor <- apply(cor_recoveries, c(1,2), mean)
sd_cor <- apply(cor_recoveries, c(1,2), sd)

correlation_parents <- list(mean_cor, sd_cor)


# Save data -------

filename <- paste0("data/recovery/recoveryResults", "_", dataset, "_", translation_of_interest, ".RData")

save(true_parent_parameters,
     hdi_recoveries,
     true_subject_parameters,
     subjParameters_recoveries,
     correlation_parents,
     file = filename
)


