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
              "ggplot2",
              "rjags",
              "cowplot",
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
library(ggplot2)
library(rjags)
library(cowplot)
library(runjags)

# Load required functions
source("functions/fun_plot_posterior_distributions.R")
source("functions/fun_plot_change_parameters.R")

rm(package, packages, is_package_installed)


### Load data ------

runJagsOut <- readRDS("data/runJagsOut_environmental_friendliness_nobounds.rds")
mcmcfin = as.mcmc.list(runJagsOut)
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))
hdi_recoveries <- readRDS("data/hdi_recoveries.rds")
true_parent_parameters <- readRDS("data/true_parent_parameters.rds")

# 1 - Ability to correctly infer group mean -----------

# Combine hdi_recoveries and true_parent_parameters
str(hdi)

test <- left_join(hdi_recoveries, 
                  true_parent_parameters,
                  join_by(sim, parameters))


