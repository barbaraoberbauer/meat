#---
# title: "Computational Mechanisms of Attribute Translations" 
# author: Barbara Oberbauer (barbara.oberbauer@uni-hamburg.de)
# last update: "2025-05-10"
# produced under R version: 2024.09.0
#---

# plot differences between subgroups (w and w/o price translation)

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
              "rjags",
              "dplyr",
              "cowplot")

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
library(rjags)
library(dplyr)
library(cowplot)


# Load functions for plotting
# https://github.com/boboppie/kruschke-doing_bayesian_data_analysis/blob/master/2e/DBDA2E-utilities.R
source("functions/DBDA2E-utilities.R")
source("functions/fun_plot_group_differences_forest.R")

rm(package, packages, is_package_installed)


### Load data ------

translation_of_interest <- "emissions"
# translations: "control", "emissions", "operating_costs", "environmental_friendliness"

bounded <- FALSE # set to TRUE for bounded model, set to FALSE for unbounded model

if (bounded == TRUE) {
  
  file_extension <- "_bounds"
  
} else if (bounded == FALSE) {
  
  file_extension <- "_nobounds"
  
}

runJagsOut_pt_absent <- readRDS(paste0("data/runJagsOut_", translation_of_interest, "_price_translation_absent", file_extension, ".rds"))
runJagsOut_pt_present <- readRDS(paste0("data/runJagsOut_", translation_of_interest, "_price_translation_present", file_extension, ".rds"))

hdi_pt_absent <- readRDS(paste0("data/hdi_", translation_of_interest, "_price_translation_absent", file_extension, ".rds"))
hdi_pt_present <- readRDS(paste0("data/hdi_", translation_of_interest, "_price_translation_present", file_extension, ".rds"))

### Combine all chains ------

# store as mcmc object
mcmcfin_pt_absent = as.mcmc.list(runJagsOut_pt_absent)
mcmcfin_pt_present = as.mcmc.list(runJagsOut_pt_present)


# combine chains
combined_mcmcfin_pt_absent <- as.data.frame(do.call(rbind, mcmcfin_pt_absent))
combined_mcmcfin_pt_present <- as.data.frame(do.call(rbind, mcmcfin_pt_present))


# Calculate group differences -------

# PT present - PT absent 

### Weight price ---------

price_diff <- (pnorm(combined_mcmcfin_pt_present$mu_w1 + combined_mcmcfin_pt_present$mu_dw1) - 
                       pnorm(combined_mcmcfin_pt_present$mu_w1)) -
  (pnorm(combined_mcmcfin_pt_absent$mu_w1 + combined_mcmcfin_pt_absent$mu_dw1) -
     pnorm(combined_mcmcfin_pt_absent$mu_w1))

### Weight consumption ---------

consumption_diff <- (pnorm(combined_mcmcfin_pt_present$mu_w2 + combined_mcmcfin_pt_present$mu_dw2) - 
                     pnorm(combined_mcmcfin_pt_present$mu_w2)) -
  (pnorm(combined_mcmcfin_pt_absent$mu_w2 + combined_mcmcfin_pt_absent$mu_dw2) -
     pnorm(combined_mcmcfin_pt_absent$mu_w2))

### Weight popularity

combined_mcmcfin_pt_absent$mu_w3 <- 1 - pnorm(combined_mcmcfin_pt_absent$mu_w1) - pnorm(combined_mcmcfin_pt_absent$mu_w2)
combined_mcmcfin_pt_absent$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin_pt_absent$mu_w1 + combined_mcmcfin_pt_absent$mu_dw1) - 
  pnorm(combined_mcmcfin_pt_absent$mu_w2 + combined_mcmcfin_pt_absent$mu_dw2)

combined_mcmcfin_pt_present$mu_w3 <- 1 - pnorm(combined_mcmcfin_pt_present$mu_w1) - pnorm(combined_mcmcfin_pt_present$mu_w2)
combined_mcmcfin_pt_present$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin_pt_present$mu_w1 + combined_mcmcfin_pt_present$mu_dw1) - 
  pnorm(combined_mcmcfin_pt_present$mu_w2 + combined_mcmcfin_pt_present$mu_dw2)


popularity_diff <- (combined_mcmcfin_pt_present$mu_w3_AT - combined_mcmcfin_pt_present$mu_w3) -
  (combined_mcmcfin_pt_absent$mu_w3_AT - combined_mcmcfin_pt_absent$mu_w3)


### Theta -------

theta_diff <- combined_mcmcfin_pt_present$mu_dtheta -
  combined_mcmcfin_pt_absent$mu_dtheta


### Phi -------

phi_diff <- combined_mcmcfin_pt_present$mu_dphi -
  combined_mcmcfin_pt_absent$mu_dphi


### Boundary separation -------

alpha_diff <- combined_mcmcfin_pt_present$mu_dalpha -
  combined_mcmcfin_pt_absent$mu_dalpha


### Starting point bias -------

sp_diff <- combined_mcmcfin_pt_present$mu_dsp -
  combined_mcmcfin_pt_absent$mu_dsp


### Non-decision time ------

tau_diff <- combined_mcmcfin_pt_present$mu_tau - 
  combined_mcmcfin_pt_absent$mu_tau


### Drift scaling -------

scaling_diff <- combined_mcmcfin_pt_present$mu_dscaling -
  combined_mcmcfin_pt_absent$mu_dscaling


# Calculate modes -------

# write function that finds mode
distMode <- function(x){
  as.numeric(names(sort(-table(round(x,3))))[1])
}


# weight price
mode_w1_pt_absent <- distMode(pnorm(combined_mcmcfin_pt_absent$mu_w1 + combined_mcmcfin_pt_absent$mu_dw1) -
                                pnorm(combined_mcmcfin_pt_absent$mu_w1))
mode_w1_pt_present <- distMode(pnorm(combined_mcmcfin_pt_present$mu_w1 + combined_mcmcfin_pt_present$mu_dw1) -
                                 pnorm(combined_mcmcfin_pt_present$mu_w1))
mode_w1_diff <- distMode(price_diff)


# weight consumption
mode_w2_pt_absent <- distMode(pnorm(combined_mcmcfin_pt_absent$mu_w2 + combined_mcmcfin_pt_absent$mu_dw2) -
                                pnorm(combined_mcmcfin_pt_absent$mu_w2))
mode_w2_pt_present <- distMode(pnorm(combined_mcmcfin_pt_present$mu_w2 + combined_mcmcfin_pt_present$mu_dw2) -
                                 pnorm(combined_mcmcfin_pt_present$mu_w2))
mode_w2_diff <- distMode(consumption_diff)


# weight popularity
mode_w3_pt_absent <- distMode(combined_mcmcfin_pt_absent$mu_w3_AT - combined_mcmcfin_pt_absent$mu_w3)
mode_w3_pt_present <- distMode(combined_mcmcfin_pt_present$mu_w3_AT - combined_mcmcfin_pt_present$mu_w3)
mode_w3_diff <- distMode(popularity_diff)


# theta
mode_theta_pt_absent <- distMode(combined_mcmcfin_pt_absent$mu_dtheta)
mode_theta_pt_present <- distMode(combined_mcmcfin_pt_present$mu_dtheta)
mode_theta_diff <- distMode(theta_diff)


# phi
mode_phi_pt_absent <- distMode(combined_mcmcfin_pt_absent$mu_dphi)
mode_phi_pt_present <- distMode(combined_mcmcfin_pt_present$mu_dphi)
mode_phi_diff <- distMode(phi_diff)


# boundary
mode_alpha_pt_absent <- distMode(combined_mcmcfin_pt_absent$mu_dalpha)
mode_alpha_pt_present <- distMode(combined_mcmcfin_pt_present$mu_dalpha)
mode_alpha_diff <- distMode(alpha_diff)

# starting point
mode_sp_pt_absent <- distMode(combined_mcmcfin_pt_absent$mu_dsp)
mode_sp_pt_present <- distMode(combined_mcmcfin_pt_present$mu_dsp)
mode_sp_diff <- distMode(sp_diff)

# non-decision time
mode_tau_pt_absent <- distMode(combined_mcmcfin_pt_absent$mu_dtau)
mode_tau_pt_present <- distMode(combined_mcmcfin_pt_present$mu_dtau)
mode_tau_diff <- distMode(tau_diff)

# scaling
mode_scaling_pt_absent <- distMode(combined_mcmcfin_pt_absent$mu_dscaling)
mode_scaling_pt_present <- distMode(combined_mcmcfin_pt_present$mu_dscaling)
mode_scaling_diff <- distMode(scaling_diff)



### Combine to data frame -------
params <- c(mode_w1_pt_absent = mode_w1_pt_absent,
            mode_w1_pt_present = mode_w1_pt_present,
            mode_w1_diff = mode_w1_diff,
            mode_w2_pt_absent = mode_w2_pt_absent,
            mode_w2_pt_present = mode_w2_pt_present,
            mode_w2_diff = mode_w2_diff,
            mode_w3_pt_absent = mode_w3_pt_absent,
            mode_w3_pt_present = mode_w3_pt_present,
            mode_w3_diff = mode_w3_diff,
            mode_theta_pt_absent = mode_theta_pt_absent,
            mode_theta_pt_present = mode_theta_pt_present,
            mode_theta_diff = mode_theta_diff,
            mode_phi_pt_absent = mode_phi_pt_absent,
            mode_phi_pt_present = mode_phi_pt_present,
            mode_phi_diff = mode_phi_diff,
            mode_alpha_pt_absent = mode_alpha_pt_absent,
            mode_alpha_pt_present = mode_alpha_pt_present,
            mode_alpha_diff = mode_alpha_diff,
            mode_sp_pt_absent = mode_sp_pt_absent,
            mode_sp_pt_present = mode_sp_pt_present,
            mode_sp_diff = mode_sp_diff,
            mode_tau_pt_absent = mode_tau_pt_absent,
            mode_tau_pt_present = mode_tau_pt_present,
            mode_tau_diff = mode_tau_diff,
            mode_scaling_pt_absent = mode_scaling_pt_absent,
            mode_scaling_pt_present = mode_scaling_pt_present,
            mode_scaling_diff = mode_scaling_diff)

estimates <- data.frame(parameter = names(params),
                        mode = as.vector(params))

# rm(mode_alpha_rating,
#    mode_alpha_control,
#    mode_alpha_emissions,
#    mode_alpha_rc,
#    mode_alpha_ec,
#    mode_theta_rating,
#    mode_theta_control,
#    mode_theta_emissions,
#    mode_theta_rc,
#    mode_theta_re,
#    mode_w2_rating,
#    mode_w2_control,
#    mode_w2_emissions,
#    mode_w2_rc,
#    mode_w2_re)

estimates <- estimates %>%
  separate(parameter, into = c("prefix", "parameter", "group"), sep = "_", extra = "merge") %>%
  select(-prefix)

estimates$lowerHDI <- NA
estimates$upperHDI <- NA


### Add HDIs -------

estimates[estimates$parameter == "w1" & estimates$group == "pt_absent", 4:5] <- hdi_pt_absent$w_price[[3]] 
estimates[estimates$parameter == "w1" & estimates$group == "pt_present", 4:5] <- hdi_pt_present$w_price[[3]] 
estimates[estimates$parameter == "w1" & estimates$group == "diff", 4:5] <- HDIofMCMC(price_diff)

estimates[estimates$parameter == "w2" & estimates$group == "pt_absent", 4:5] <- hdi_pt_absent$w_consumption[[3]] 
estimates[estimates$parameter == "w2" & estimates$group == "pt_present", 4:5] <- hdi_pt_present$w_consumption[[3]] 
estimates[estimates$parameter == "w2" & estimates$group == "diff", 4:5] <- HDIofMCMC(consumption_diff)

estimates[estimates$parameter == "w3" & estimates$group == "pt_absent", 4:5] <- hdi_pt_absent$w_popularity[[3]] 
estimates[estimates$parameter == "w3" & estimates$group == "pt_present", 4:5] <- hdi_pt_present$w_popularity[[3]] 
estimates[estimates$parameter == "w3" & estimates$group == "diff", 4:5] <- HDIofMCMC(popularity_diff)

estimates[estimates$parameter == "theta" & estimates$group == "pt_absent", 4:5] <- hdi_pt_absent$theta[[3]] 
estimates[estimates$parameter == "theta" & estimates$group == "pt_present", 4:5] <- hdi_pt_present$theta[[3]] 
estimates[estimates$parameter == "theta" & estimates$group == "diff", 4:5] <- HDIofMCMC(theta_diff)

estimates[estimates$parameter == "phi" & estimates$group == "pt_absent", 4:5] <- hdi_pt_absent$phi[[3]] 
estimates[estimates$parameter == "phi" & estimates$group == "pt_present", 4:5] <- hdi_pt_present$phi[[3]] 
estimates[estimates$parameter == "phi" & estimates$group == "diff", 4:5] <- HDIofMCMC(phi_diff)

estimates[estimates$parameter == "alpha" & estimates$group == "pt_absent", 4:5] <- hdi_pt_absent$alpha[[3]] 
estimates[estimates$parameter == "alpha" & estimates$group == "pt_present", 4:5] <- hdi_pt_present$alpha[[3]] 
estimates[estimates$parameter == "alpha" & estimates$group == "diff", 4:5] <- HDIofMCMC(alpha_diff)

estimates[estimates$parameter == "sp" & estimates$group == "pt_absent", 4:5] <- hdi_pt_absent$sp[[3]] 
estimates[estimates$parameter == "sp" & estimates$group == "pt_present", 4:5] <- hdi_pt_present$sp[[3]] 
estimates[estimates$parameter == "sp" & estimates$group == "diff", 4:5] <- HDIofMCMC(sp_diff)

estimates[estimates$parameter == "tau" & estimates$group == "pt_absent", 4:5] <- hdi_pt_absent$tau[[3]] 
estimates[estimates$parameter == "tau" & estimates$group == "pt_present", 4:5] <- hdi_pt_present$tau[[3]] 
estimates[estimates$parameter == "tau" & estimates$group == "diff", 4:5] <- HDIofMCMC(tau_diff)

estimates[estimates$parameter == "scaling" & estimates$group == "pt_absent", 4:5] <- hdi_pt_absent$scaling[[3]] 
estimates[estimates$parameter == "scaling" & estimates$group == "pt_present", 4:5] <- hdi_pt_present$scaling[[3]] 
estimates[estimates$parameter == "scaling" & estimates$group == "diff", 4:5] <- HDIofMCMC(scaling_diff)

estimates$group <- as.factor(estimates$group)

# Plot effects of group -----

cols <- c("pt_absent" = "black",
          "pt_present" = "black",
          "diff" = "#8896AB")

# linewidth and sizes
lwRegular <- 2.5
lwDif <- 3.5

sizeRegular <- 7
sizeDif <- 10

# labels
groupLabels <- c("diff" = "PT present -\nPT absent", "pt_absent" = "PT absent", "pt_present" = "PT present")
valuesLinewidth <- c("pt_absent" = lwRegular, "pt_present" = lwRegular, "diff" = lwDif)
valuesSize <- c("pt_absent" = sizeRegular, "pt_present" = sizeRegular, "diff" = sizeDif)

### Price -------

plot_price_diff <- plot_group_differences_forest(estimates, 
                                                       "w1", 
                                                       "pt_present", 
                                                       "pt_absent", 
                                                       "diff",
                                                       valuesLinewidth,
                                                       valuesSize,
                                                       groupLabels, 
                                                       "Effects on Weight Price", 
                                                       "",
                                                       cols)


### Consumption -------

plot_consumption_diff <- plot_group_differences_forest(estimates, 
                                                     "w2", 
                                                     "pt_present", 
                                                     "pt_absent", 
                                                     "diff",
                                                     valuesLinewidth,
                                                     valuesSize,
                                                     groupLabels, 
                                                     "Effects on Weight Consumption", 
                                                     "",
                                                     cols)


### Popularity -------

plot_popularity_diff <- plot_group_differences_forest(estimates, 
                                                       "w3", 
                                                       "pt_present", 
                                                       "pt_absent", 
                                                       "diff",
                                                       valuesLinewidth,
                                                       valuesSize,
                                                       groupLabels, 
                                                       "Effects on Weight Popularity", 
                                                       "",
                                                       cols)


### Theta ---------

plot_theta_diff <- plot_group_differences_forest(estimates, 
                                               "theta", 
                                               "pt_present", 
                                               "pt_absent", 
                                               "diff",
                                               valuesLinewidth,
                                               valuesSize,
                                               groupLabels, 
                                               "Effects on Discounting \nUnattended Option (theta)", 
                                               "",
                                               cols)

### Phi ---------

plot_phi_diff <- plot_group_differences_forest(estimates, 
                                                 "phi", 
                                                 "pt_present", 
                                                 "pt_absent", 
                                                 "diff",
                                                 valuesLinewidth,
                                                 valuesSize,
                                                 groupLabels, 
                                                 "Effects on Discounting \nUnattended Attribute (phi)", 
                                                 "",
                                                 cols)


### Boundary -----

plot_boundary_diff <- plot_group_differences_forest(estimates, 
                                                  "alpha", 
                                                  "pt_present", 
                                                  "pt_absent", 
                                                  "diff",
                                                  valuesLinewidth,
                                                  valuesSize,
                                                  groupLabels, 
                                                  "Effects on Boundary Separation", 
                                                  "",
                                                  cols)

### Starting point bias -----

plot_sp_diff <- plot_group_differences_forest(estimates, 
                                                    "sp", 
                                                    "pt_present", 
                                                    "pt_absent", 
                                                    "diff",
                                                    valuesLinewidth,
                                                    valuesSize,
                                                    groupLabels, 
                                                    "Effects on \nStarting Point Bias", 
                                                    "",
                                                    cols)

### Non-decision time -----

plot_tau_diff <- plot_group_differences_forest(estimates, 
                                                    "tau", 
                                                    "pt_present", 
                                                    "pt_absent", 
                                                    "diff",
                                                    valuesLinewidth,
                                                    valuesSize,
                                                    groupLabels, 
                                                    "Effects on Non-Decision Time", 
                                                    "",
                                                    cols)

### Scaling -----

plot_scaling_diff <- plot_group_differences_forest(estimates, 
                                                    "scaling", 
                                                    "pt_present", 
                                                    "pt_absent", 
                                                    "diff",
                                                    valuesLinewidth,
                                                    valuesSize,
                                                    groupLabels, 
                                                    "Effects on Drift Scaling", 
                                                    "",
                                                    cols)






# Arrange plots --------

group_differences <- plot_grid(# plots
  plot_price_diff,
  plot_consumption_diff,
  plot_popularity_diff,
  plot_theta_diff,
  plot_phi_diff,
  plot_boundary_diff,
  plot_sp_diff,
  plot_tau_diff,
  plot_scaling_diff,


  # settings
  ncol = 3,
  labels = c("a", "b", "c", 
             "d", "e", "f",
             "g", "h", "i"),
  label_size = 20
)


# Save plot -------

ggsave(paste0("figures/price_translation_differences_", translation_of_interest, ".png"), group_differences, width = 14, height = 9)



