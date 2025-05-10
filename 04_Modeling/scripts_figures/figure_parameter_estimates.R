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

# specify subset of data 

group_of_interest <- "environmental_friendliness"
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

rm(filename)


# Combine Chains in Data -------

# store as mcmc object
mcmcfin = as.mcmc.list(runJagsOut)

# combine chains
combined_mcmcfin <- as.data.frame(do.call(rbind, mcmcfin))


# Plot Posterior Distributions ------

Nbins <- 25
col_sess1 <- "#225780"
col_sess2 <- "#8CC5E3"

### Boundary ------

pboundary <- plot_posterior_dist(combined_mcmcfin,
                                 combined_mcmcfin$mu_alpha,
                                 (combined_mcmcfin$mu_alpha + combined_mcmcfin$mu_dalpha),
                                 col_sess1,
                                 col_sess2,
                                 Nbins,
                                 "Boundary Separation")

### Scaling ------

pscaling <- plot_posterior_dist(combined_mcmcfin,
                                combined_mcmcfin$mu_scaling,
                                (combined_mcmcfin$mu_scaling + combined_mcmcfin$mu_dscaling),
                                col_sess1,
                                col_sess2,
                                Nbins,
                                "Drift Scaling")

### Tau ------

ptau <- plot_posterior_dist(combined_mcmcfin,
                            combined_mcmcfin$mu_tau,
                            (combined_mcmcfin$mu_tau + combined_mcmcfin$mu_dtau),
                            col_sess1,
                            col_sess2,
                            Nbins,
                            "Non-Decision Time")


### Theta ------

if (bounded == TRUE) {
  
  # bounded parameter
  ptheta <- plot_posterior_dist(combined_mcmcfin,
                      pnorm(combined_mcmcfin$mu_theta),
                      pnorm(combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta),
                      col_sess1,
                      col_sess2,
                      Nbins,
                      "Discounting Unattended\nOption (theta)")
  
} else if (bounded == FALSE) {
  
  # unbounded parameter
  ptheta <- plot_posterior_dist(combined_mcmcfin,
                                combined_mcmcfin$mu_theta,
                                (combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta),
                                col_sess1,
                                col_sess2,
                                Nbins,
                                "Discounting Unattended\nOption (theta)")
  
}


### Phi ------

if (bounded == TRUE) {
  
  # bounded parameter
  pphi <- plot_posterior_dist(combined_mcmcfin,
                      pnorm(combined_mcmcfin$mu_phi),
                      pnorm(combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi),
                      col_sess1,
                      col_sess2,
                      Nbins,
                      "Discounting Unattended\nAttribute (phi)")

} else if (bounded == FALSE) {
  
  # unbounded parameters
  pphi <- plot_posterior_dist(combined_mcmcfin,
                              combined_mcmcfin$mu_phi,
                              (combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi),
                              col_sess1,
                              col_sess2,
                              Nbins,
                              "Discounting Unattended\nAttribute (phi)")
  
}




### Weight Price ------

pprice <- plot_posterior_dist(combined_mcmcfin, 
                              pnorm(combined_mcmcfin$mu_w1), 
                              pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1),
                              col_sess1,
                              col_sess2, 
                              Nbins, 
                              "Weight Price")


### Weight Consumption ------

penergy <- plot_posterior_dist(combined_mcmcfin, 
                               pnorm(combined_mcmcfin$mu_w2), 
                               pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2),
                               col_sess1,
                               col_sess2, 
                               Nbins, 
                               "Weight Consumption")


### Weight Popularity ------

combined_mcmcfin$mu_w3 <- 1 - pnorm(combined_mcmcfin$mu_w1) - pnorm(combined_mcmcfin$mu_w2)
combined_mcmcfin$mu_w3_AT <- 1 - 
  pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1) - 
  pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2)


ppopularity <- plot_posterior_dist(combined_mcmcfin,
                                   combined_mcmcfin$mu_w3, 
                                   combined_mcmcfin$mu_w3_AT,
                                   col_sess1,
                                   col_sess2, 
                                   Nbins, 
                                   "Weight Popularity")


### Starting point -------

psp <- plot_posterior_dist(combined_mcmcfin,
                           combined_mcmcfin$mu_sp,
                           (combined_mcmcfin$mu_sp + combined_mcmcfin$mu_dsp),
                           col_sess1,
                           col_sess2,
                           Nbins,
                           "Starting Point Bias")


# Plot change parameters -------

# d stands for change (difference)

### Boundary -------

pdBoundary <- plot_change_param(combined_mcmcfin,
                                combined_mcmcfin$mu_dalpha,
                                Nbins)

### Scaling ------

pdScaling <- plot_change_param(combined_mcmcfin,
                               combined_mcmcfin$mu_dscaling,
                               Nbins)

### Tau ------

pdTau <- plot_change_param(combined_mcmcfin,
                           combined_mcmcfin$mu_dtau,
                           Nbins)

### Theta ------

if (bounded == TRUE) {
  
  # bounded parameter
  pdTheta <- plot_change_param(combined_mcmcfin,
                               pnorm(combined_mcmcfin$mu_theta + combined_mcmcfin$mu_dtheta) -
                                 pnorm(combined_mcmcfin$mu_theta),
                               Nbins)
  
} else if (bounded == FALSE) {
  
  # unbounded parameter
  pdTheta <- plot_change_param(combined_mcmcfin,
                               combined_mcmcfin$mu_dtheta,
                               Nbins)
  
}


### Phi ------


if (bounded == TRUE) {
  
  # bounded parameter
  pdPhi <- plot_change_param(combined_mcmcfin,
                               pnorm(combined_mcmcfin$mu_phi + combined_mcmcfin$mu_dphi) -
                                 pnorm(combined_mcmcfin$mu_phi),
                               Nbins)
  
} else if (bounded == FALSE) {
  
  # unbounded parameter
  pdPhi <- plot_change_param(combined_mcmcfin,
                             combined_mcmcfin$mu_dphi,
                             Nbins)
  
}



### Weight Price ------

# plot netto difference instead of phi-transformed change parameter
pdPrice <- plot_change_param(combined_mcmcfin,
                             pnorm(combined_mcmcfin$mu_w1 + combined_mcmcfin$mu_dw1) -
                               pnorm(combined_mcmcfin$mu_w1),
                             Nbins)


### Weight Energy ------

# plot netto difference instead of phi-transformed change parameter
pdEnergy <- plot_change_param(combined_mcmcfin,
                              pnorm(combined_mcmcfin$mu_w2 + combined_mcmcfin$mu_dw2) -
                                pnorm(combined_mcmcfin$mu_w2),
                              Nbins)


### Weight Popularity ------

pdPopularity <- plot_change_param(combined_mcmcfin,
                                  (combined_mcmcfin$mu_w3_AT -  
                                     combined_mcmcfin$mu_w3),
                                  Nbins)


### Starting point bias ----

pdsp <- plot_change_param(combined_mcmcfin,
                          combined_mcmcfin$mu_dsp,
                          Nbins)



# Arrange plots --------

group_param_estimates <- plot_grid(# plots
  pprice,
  pdPrice,
  penergy,
  pdEnergy,
  ppopularity,
  pdPopularity,
  pboundary,
  pdBoundary,
  ptheta,
  pdTheta,
  pphi,
  pdPhi,
  ptau,
  pdTau,
  pscaling,
  pdScaling,
  psp,
  pdsp,
  
  # settings
  ncol = 4,
  labels = c("a", "",
             "b", "",
             "c", "",
             "d", "",
             "e", "",
             "f", "",
             "g", "",
             "h", "",
             "i", ""
  )
)


# save plot
filename <- paste0("figures/group_param_estimates_", group_of_interest, file_extension, ".png")
ggsave(filename, group_param_estimates, width = 16, height = 12)



