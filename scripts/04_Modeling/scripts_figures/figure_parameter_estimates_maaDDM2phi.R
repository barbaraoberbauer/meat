#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# purpose: plot group-level parameter estimates for single condition (2phi)
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
              "patchwork",
              "grid",
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
library(patchwork)
library(grid)
library(runjags)

# Load required functions
source("R/functions/fun_plot_parameter_densities.R")
source("R/functions/fun_plot_parameter_change.R")


# Load theme
source("R/theme.R")
theme_set(themeMEAT())

rm(package, packages, is_package_installed)


### Load data ------

# Specify subset of data 

dataset <- "original"
# datasets: "original", "replication"

translation_of_interest <- "environmental_friendliness"

# translations for original dataset: "control", "emissions", "operating_costs", "environmental_friendliness"
# translations for replication dataset: "control", "emission_add", "rating_add", "emission_replace", "rating_replace"

time <- "20260619_0816"
# time stamp of data generation


# get runJagsOut and HDI
# create filename
filenameRunJagsOut <- paste0("data/modeling/runJagsOutmaaDDM2phiDirichlet", "_", 
                             dataset, "_", 
                             translation_of_interest, "_", 
                             time, ".rds")

filenameHDI <- paste0("data/modeling/hdimaaDDM2phiDirichlet", "_", 
                      dataset, "_", 
                      translation_of_interest, "_", 
                      time, ".rds")
  

# load files
runJagsOut <- readRDS(filenameRunJagsOut)
hdi <- readRDS(filenameHDI)

rm(filenameRunJagsOut, filenameHDI)

# Generate density and change plots ------

# set xtitle for plots
if (translation_of_interest == "control") {

    xtitleChangePlot <- "Difference btw. Sessions"
  
} else {
  
  xtitleChangePlot <- "Effect of Attr. Transl."
  
} 

# generate function
generate_all_plots <- function(runJagsOut, hdi, bound_attention_params) {
  
  # Combine Chains in Data 
  
  # store as mcmc object
  mcmcfin = as.mcmc.list(runJagsOut)
  
  # combine chains
  combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))
  
  # Extract weights 
  
  # Extract posterior samples
  mu_w_samples    <- as.matrix(combined_mcmcfin)[, c("mu_w[1]", "mu_w[2]", "mu_w[3]")]
  mu_dalr1_samples <- as.matrix(combined_mcmcfin)[, "mu_dalr1"]
  mu_dalr2_samples <- as.matrix(combined_mcmcfin)[, "mu_dalr2"]
  
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
  
  mu_w_AT_1 <- exp1_AT / denom_AT   # group mean price weight, Session 2
  mu_w_AT_2 <- exp2_AT / denom_AT   # group mean energy weight, Session 2
  mu_w_AT_3 <- 1       / denom_AT   # group mean popularity weight, Session 2
  
  
  # Extract theta and phi 
    
  theta_baseline    <- combined_mcmcfin$mu_theta
  theta_manipulation <- combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta
  
  phi_baseline    <- combined_mcmcfin$mu_phi
  phi_manipulation <- combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi
  
  phi2_baseline    <- combined_mcmcfin$mu_phi2
  phi2_manipulation <- combined_mcmcfin$mu_phi2 + combined_mcmcfin$mu_dphi2
  
  
  # Group mean density plots
  plots_density <- list(
    price = plot_group_means_densities(combined_mcmcfin,
                                       combined_mcmcfin$`mu_w[1]`,
                                       mu_w_AT_1,
                                       "Weight Price"),
    consumption = plot_group_means_densities(combined_mcmcfin,
                                             combined_mcmcfin$`mu_w[2]`,
                                             mu_w_AT_2,
                                             "Weight Consumption"),
    popularity = plot_group_means_densities(combined_mcmcfin,
                                            combined_mcmcfin$`mu_w[3]`,
                                            mu_w_AT_3,
                                            "Weight Popularity"),
    boundary = plot_group_means_densities(combined_mcmcfin, 
                                          combined_mcmcfin$mu_alpha, 
                                          combined_mcmcfin$mu_alpha + combined_mcmcfin$mu_dalpha, 
                                          "Boundary Separation"),
    theta = plot_group_means_densities(combined_mcmcfin,
                                       theta_baseline,
                                       theta_manipulation,
                                       "Discounting Unattended \nOption (theta)"),
    phi = plot_group_means_densities(combined_mcmcfin,
                                     phi_baseline,
                                     phi_manipulation,
                                     "Discounting Unattended \nAttribute (phi)"),
    phi2 = plot_group_means_densities(combined_mcmcfin,
                                     phi2_baseline,
                                     phi2_manipulation,
                                     "Discounting Unattended \nConsumption (phi2)"),
    sp = plot_group_means_densities(combined_mcmcfin,
                                    combined_mcmcfin$mu_sp,
                                    combined_mcmcfin$mu_sp + combined_mcmcfin$mu_dsp,
                                    "Starting Point Bias"),
    ndt = plot_group_means_densities(combined_mcmcfin,
                                     combined_mcmcfin$mu_tau,
                                     combined_mcmcfin$mu_tau + combined_mcmcfin$mu_dtau,
                                     "Non-Decision Time"),
    scaling = plot_group_means_densities(combined_mcmcfin,
                                         combined_mcmcfin$mu_scaling,
                                         combined_mcmcfin$mu_scaling + combined_mcmcfin$mu_dscaling,
                                         "Drift Scaling")
  ) # density plots
  
  
  # Change parameter plots
  plots_change <- list(
    dPrice = plot_change_param(mu_w_AT_1 - combined_mcmcfin$`mu_w[1]`,
                               hdi$w_price$hdi_change,
                               xtitleChangePlot),
    dConsumption = plot_change_param(mu_w_AT_2 - combined_mcmcfin$`mu_w[2]`,
                                     hdi$w_consumption$hdi_change,
                                     xtitleChangePlot),
    dPopularity = plot_change_param(mu_w_AT_3 - combined_mcmcfin$`mu_w[3]`,
                                    hdi$w_popularity$hdi_change,
                                    xtitleChangePlot),
    dBoundary = plot_change_param(combined_mcmcfin$mu_dalpha,
                                  hdi$alpha$hdi_change,
                                  xtitleChangePlot),
    dTheta = plot_change_param(theta_manipulation - theta_baseline,
                               hdi$theta$hdi_change,
                               xtitleChangePlot),
    dPhi = plot_change_param(phi_manipulation - phi_baseline,
                             hdi$phi$hdi_change,
                             xtitleChangePlot),
    dPhi2 = plot_change_param(phi2_manipulation - phi2_baseline,
                             hdi$phi2$hdi_change,
                             xtitleChangePlot),
    dSp = plot_change_param(combined_mcmcfin$mu_dsp,
                            hdi$sp$hdi_change,
                            xtitleChangePlot),
    dNdt = plot_change_param(combined_mcmcfin$mu_dtau,
                             hdi$tau$hdi_change,
                             xtitleChangePlot),
    dScaling = plot_change_param(combined_mcmcfin$mu_dscaling,
                                 hdi$scaling$hdi_change,
                                 xtitleChangePlot)
  ) # change plots
  
  list(density = plots_density,
       change = plots_change)
  
}

# Run for condition
plots <- generate_all_plots(runJagsOut, hdi, bound_attention_params)


# Combine plots -------

all_plots <- 
  # price
  plots$density$price + 
  plots$change$dPrice +
  # consumption
  plots$density$consumption + 
  plots$change$dConsumption +
  # popularity
  plots$density$popularity + 
  plots$change$dPopularity +
  # theta
  plots$density$theta +
  plots$change$dTheta +
  # phi
  plots$density$phi +
  plots$change$dPhi +
  # phi2
  plots$density$phi2 +
  plots$change$dPhi2+
  # boundary separation
  plots$density$boundary +
  plots$change$dBoundary +
  # starting point bias
  plots$density$sp +
  plots$change$dSp +
  # non-decision time
  plots$density$ndt +
  plots$change$dNdt +
  # drift scaling
  plots$density$scaling +
  plots$change$dScaling +
  plot_layout(ncol = 6,
              guides = 'collect') +
  plot_annotation(tag_levels = list(c('a', '', 'b', '', 'c', '', 
                                      'd', '', 'e', '', 'f', '',
                                      'g', '', 'h', '', 'i', ''))) &
  theme(
    plot.margin = margin(4, 4, 4, 4),
    legend.position = "bottom",
    plot.tag = element_text(size = 20, face = "bold")
  )


# Save plots --------

# save all parameters

filename <- paste0("figures/groupParamEstimates2phi", "_", 
                     dataset, "_", 
                     translation_of_interest, ".png")
  
  
  
ggsave(filename, all_plots, width = 13, height = 7, units = "in")



